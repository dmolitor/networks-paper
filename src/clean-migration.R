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
  readxl::read_excel(here::here("scratch/rural_urban.xlsx"))
)
counties <- counties |>
  dplyr::left_join(
    dplyr::distinct(
      dplyr::mutate(dplyr::select(urban_des, cty_fips), urban = 0),
      cty_fips,
      .keep_all = TRUE
    ), 
    by = c("FIPS" = "cty_fips")
  ) |>
  dplyr::mutate(urban = ifelse(is.na(urban), 1, 0))

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
    dplyr::select(counties, FIPS, urban),
    by = c("o_fips" = "FIPS")
  )

# Output data
migration_out <- sf::st_drop_geometry(migration) |> dplyr::select(-geometry)
readr::write_csv(migration_out, here::here("data/migration.csv"))

# Plotting ----------------------------------------------------------------

# Plot counties by urban category
tigris::shift_geometry(counties) |>
  dplyr::mutate(Urban = as.logical(urban)) |>
  ggplot(aes(fill = Urban)) + 
  geom_sf() + 
  theme_light() +
  labs(title = "Urban/Rural Counties (as Classified by the HRSA)") +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


