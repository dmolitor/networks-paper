library(ggplot2)
library(sf)

sheets <- readxl::excel_sheets(here::here("scratch/total_migration.xlsx"))[-52]

# Import U.S. county data
counties <- tigris::counties(
  cb = TRUE,
  resolution = "20m",
  state = sheets,
  year = 2015
) |>
  dplyr::mutate(FIPS = paste0(STATEFP, COUNTYFP))

# Import U.S. states data
states <- tigris::states(TRUE, "20m", year = 2015) |>
  sf::st_drop_geometry() |>
  dplyr::select(STATEFP, NAME) |>
  dplyr::rename("state" = "NAME")

# Merge state names to county data
counties <- dplyr::left_join(counties, states, by = "STATEFP") |>
  dplyr::rename("county" = "NAME")

# Pull in urban areas
urban_des <- janitor::clean_names(
  readxl::read_excel(here::here("scratch/rural_urban.xls"), skip = 2)
)
urban_des <- urban_des |>
  dplyr::filter(
    metropolitan_micropolitan_statistical_area == "Metropolitan Statistical Area"
  ) |>
  dplyr::mutate(
    fips = paste0(fips_state_code, fips_county_code),
    urban = 1
  ) |>
  dplyr::select(fips, urban)

counties <- counties |>
  dplyr::left_join(urban_des, by = c("FIPS" = "fips")) |>
  dplyr::mutate(urban = ifelse(is.na(urban), 0, 1))

# Combine all sheets from the Excel file
migration <- dplyr::bind_rows(
  lapply(
    sheets,
    \(s) {
      janitor::clean_names(
        readxl::read_excel(
          here::here("scratch/total_migration.xlsx"),
          skip = 1,
          sheet = s
        )
      ) |>
        dplyr::select(-dplyr::starts_with("x")) |>
        dplyr::filter(!is.na(state_code_of_geography_a))
    }
  )
)

# Clean the migration data
migration <- migration |>
  dplyr::mutate(
    # Create origin and destination county FIPS codes
    dplyr::across(
      .cols = c(
        state_code_of_geography_a,
        state_u_s_island_area_foreign_region_code_of_geography_b
      ),
      .fns = \(s) substr(s, 2, 3)
    ),
    county_a_fips = paste0(
      state_code_of_geography_a,
      fips_county_code_of_geography_a
    ),
    county_b_fips = paste0(
      state_u_s_island_area_foreign_region_code_of_geography_b,
      fips_county_code_of_geography_b
    ),
    # Convert flows from string to integer
    dplyr::across(.cols = dplyr::contains("flow"), .fns = as.integer)
  ) |>
  dplyr::filter(
    # Only keep origins and destinations that are valid U.S. counties
    state_code_of_geography_a %in% counties$STATEFP,
    state_u_s_island_area_foreign_region_code_of_geography_b %in% counties$STATEFP,
    # Drop any cases where flows are missing (estimates that aren't available)
    !is.na(flow_from_geography_b_to_geography_a),
    !is.na(counterflow_from_geography_a_to_geography_b1),
    # Drop any cases where the number of movers == 0
    flow_from_geography_b_to_geography_a != 0,
    counterflow_from_geography_a_to_geography_b1 != 0
  )

# Get county-level population estimates for 2011 and 2015
county_pop <- tidycensus::get_estimates(
  geography = "county",
  product = "population",
  geometry = TRUE,
  keep_geo_vars = TRUE,
  year = 2015,
  time_series = TRUE
)

county_pop <- county_pop |>
  dplyr::filter(DATE %in% c(4, 8), variable == "POP") |>
  dplyr::mutate(
    FIPS = paste0(STATEFP, COUNTYFP),
    DATE = dplyr::case_match(DATE, 4 ~ 2011, 8 ~ 2015)
  ) |>
  dplyr::select(FIPS, DATE, value) |>
  dplyr::rename("POP" = "value") |>
  sf::st_drop_geometry() |>
  dplyr::rename("year" = "DATE", "pop" = "POP")

# Combine population data with migration data and calculate
# (1) P{D|O}: Probability that you're in D given that you grew up in O
# (2) P{O|D}: Probability that you're from O given that you live in D
migration <- migration |>
  dplyr::select(
    county_a_fips,
    county_b_fips,
    flow_from_geography_b_to_geography_a,
    net_migration_from_geography_b_to_geography_a1
  ) |>
  dplyr::rename(
    "o_fips" = "county_b_fips",
    "d_fips" = "county_a_fips",
    "n" = "flow_from_geography_b_to_geography_a",
    "n_net" = "net_migration_from_geography_b_to_geography_a1"
  ) |>
  # Join 2011 population as Origin population
  dplyr::left_join(
    dplyr::filter(county_pop, year == 2011),
    by = c("o_fips" = "FIPS")
  ) |>
  dplyr::select(-year) |>
  dplyr::rename("o_pop" = "pop") |>
  # Join 2015 population as Destination population
  dplyr::left_join(
    dplyr::filter(county_pop, year == 2015),
    by = c("d_fips" = "FIPS")
  ) |>
  dplyr::select(-year) |>
  dplyr::rename("d_pop" = "pop") |>
  # Calculate Pr{D|O} and Pr{O|D}
  dplyr::mutate(pr_d_o = n/o_pop, pr_o_d = n/d_pop)

# Add a binary indicator: 1 = County intersects urban area; 0 = No urban areas
migration <- migration |>
  dplyr::left_join(
    dplyr::select(counties, FIPS, county, state, urban),
    by = c("o_fips" = "FIPS")
  ) |>
  dplyr::rename(
    "o_county" = "county",
    "o_state" = "state",
    "o_urban" = "urban"
  ) |>
  dplyr::left_join(
    dplyr::select(sf::st_drop_geometry(counties), FIPS, county, state, urban),
    by = c("d_fips" = "FIPS")
  ) |>
  dplyr::rename(
    "d_county" = "county",
    "d_state" = "state",
    "d_urban" = "urban"
  ) |>
  dplyr::mutate(
    migration_type = dplyr::case_when(
      o_urban == 0 & d_urban == 0 ~ "rural-rural",
      o_urban == 0 & d_urban == 1 ~ "rural-urban",
      o_urban == 1 & d_urban == 0 ~ "urban-rural",
      o_urban == 1 & d_urban == 1 ~ "urban-urban"
    )
  )

# Output data
migration_out <- dplyr::select(migration, -geometry)
readr::write_csv(migration_out, here::here("data/migration.csv"))

# Plotting ----------------------------------------------------------------

# Plot counties by urban category
tigris::shift_geometry(counties) |>
  dplyr::mutate(Urban = as.logical(urban)) |>
  ggplot(aes(fill = Urban)) + 
  geom_sf() + 
  theme_light() +
  labs(title = "Urban/Rural Counties (as Classified by the US Census)") +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Plot counties by net migration to that county
migration_agg <- migration |>
  dplyr::mutate(n_net = as.numeric(n_net)) |>
  dplyr::group_by(o_fips, o_county, o_state) |>
  dplyr::summarise(
    n_net = sum(n_net),
    urban = dplyr::first(o_urban),
    geometry = dplyr::first(geometry),
    .groups = "drop"
  ) |>
  sf::st_as_sf()

# Get bin values for inflows/outflows
outflow_bins <- dplyr::filter(migration_agg, n_net >= 0) |>
  dplyr::pull(n_net) |>
  quantile(seq(0, 1, length.out = 4))

inflow_bins <- dplyr::filter(migration_agg, n_net <= 0) |>
  dplyr::pull(n_net) |>
  quantile(seq(0, 1, length.out = 4))

flow_bins <- unique(unname(c(inflow_bins, outflow_bins)))
flow_labels <- c(
  "High Inflow", "Medium Inflow", "Low Inflow",
  "Low Outflow", "Medium Outflow", "High Outflow"
)

# Plot it
migration_agg_plot_dat <- tigris::shift_geometry(migration_agg) |>
  dplyr::mutate(
    `Net Migration` = cut(
      n_net,
      breaks = flow_bins,
      labels = flow_labels,
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  )

### Counties (urban vs. rural) migration flows

# All counties
ggplot(tigris::shift_geometry(counties)) +
  geom_sf(fill = "gray") +
  geom_sf(
    data = migration_agg_plot_dat,
    mapping = aes(fill = `Net Migration`),
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = tigris::shift_geometry(tigris::states(TRUE, "20m", 2015)),
    alpha = 0,
    color = "white",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  labs(title = "Net Migration by County (2011-2015)") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "#F5F5F2", color = NA),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#F5F5F2", color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F5F5F2", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    text = element_text(family = "PT Sans")
  )

# Urban counties
ggplot(tigris::shift_geometry(counties)) +
  geom_sf(fill = "gray") +
  geom_sf(
    data = dplyr::filter(migration_agg_plot_dat, urban == 1),
    mapping = aes(fill = `Net Migration`),
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = tigris::shift_geometry(tigris::states(TRUE, "20m", 2015)),
    alpha = 0,
    color = "white",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  labs(title = "Net Migration in Urban Counties (2011-2015)") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "#F5F5F2", color = NA),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#F5F5F2", color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F5F5F2", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    text = element_text(family = "PT Sans")
  )

# Non-urban counties
ggplot(tigris::shift_geometry(counties)) +
  geom_sf(fill = "gray") +
  geom_sf(
    data = dplyr::filter(migration_agg_plot_dat, urban == 0),
    mapping = aes(fill = `Net Migration`),
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = tigris::shift_geometry(tigris::states(TRUE, "20m", 2015)),
    alpha = 0,
    color = "white",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  labs(title = "Net Migration in Non-Urban Counties (2011-2015)") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "#F5F5F2", color = NA),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#F5F5F2", color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F5F5F2", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    text = element_text(family = "PT Sans")
  )

### 

# Chord diagram

top_rural_to_urban <- migration |>
  dplyr::filter(migration_type == "rural-urban") |>
  dplyr::summarise(
    n = sum(as.numeric(n_net)),
    .by = c(o_fips, o_county, o_state)
  ) |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::slice_head(n = 5) |>
  dplyr::mutate(o_name = paste0(o_county, ", ", o_state))

top_rural_to_urban <- migration |>
  dplyr::filter(migration_type == "rural-urban") |>
  dplyr::mutate(
    o_name = paste0(o_county, ", ", o_state),
    d_name = paste0(d_county, ", ", d_state)
  ) |>
  dplyr::filter(o_name %in% top_rural_to_urban$o_name) |>
  dplyr::mutate(
    o_state = state.abb[match(o_state, state.name)],
    d_state = state.abb[match(d_state, state.name)],
    o_name = paste0(o_county, ", ", o_state),
    d_name = paste0(d_county, ", ", d_state)
  ) |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::group_by(o_county) |>
  dplyr::slice_head(n = 5) |>
  dplyr::ungroup() |>
  dplyr::arrange(o_fips) |>
  dplyr::mutate(
    to = paste0(d_county, ", ", substr(d_state, 1, 3)), 
    from = paste0(o_county, ", ", substr(o_state, 1, 3)),
    value = n,
    type = "Rural to Urban"
  ) |>
  dplyr::select(from, to, value, type)

# top_urban_to_rural <- migration |>
#   dplyr::filter(migration_type == "urban-rural") |>
#   dplyr::arrange(dplyr::desc(pr_d_o)) |>
#   dplyr::slice_head(n = 30) |>
#   dplyr::mutate(
#     to = paste0(d_county, ", ", d_state), 
#     from = paste0(o_county, ", ", o_state),
#     type = "Urban to Rural"
#   ) |>
#   dplyr::select(from, to, n, pr_d_o, type)
# 
# top_switches <- dplyr::bind_rows(top_rural_to_urban, top_urban_to_rural) |>
#   dplyr::rename("value" = "pr_d_o")

# Plot it!!!
circlize::chordDiagram(
  x = top_rural_to_urban[, c("from", "to", "value")],
  row.col = 1:5,
  directional = 1,
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow",
  annotationTrack = "grid",
  preAllocateTracks = 1
)
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(
      mean(xlim),
      ylim[1] + .1,
      sector.name,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5)
    )
    circos.axis(
      h = "top",
      labels.cex = 0.5,
      major.tick.length = 0.2,
      sector.index = sector.name,
      track.index = 2
    )
  },
  bg.border = NA
)
