library(ggplot2)
library(sf)

# Import inter-gen mobility data and re-structure data --------------------

states <- function() {
  c(
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "District Of Columbia",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
  )
}

# County-level mobility data

intergen_mobility_county <- readxl::read_excel(
  path = here::here("data/inter-gen-mobility-county.xls"),
  skip = 29,
  sheet = "Online Data Table 3"
)
names(intergen_mobility_county) <- tolower(
  gsub("%", "_perc", gsub(" |-", "_", names(intergen_mobility_county)))
)
intergen_mobility_county <- intergen_mobility_county[-1, ]
intergen_mobility_county[, c(1, 6, 14:25)] <- lapply(
  intergen_mobility_county[, c(1, 6, 14:25)],
  as.integer
)
intergen_mobility_county$county_fips_code <- formatC(
  intergen_mobility_county$county_fips_code,
  width = 5,
  flag = "0"
)

# Commuting-zone level mobility data

intergen_mobility_cz <- readxl::read_excel(
  path = here::here("data/inter-gen-mobility-county.xls"),
  skip = 49,
  sheet = "Online Data Table 5"
)
names(intergen_mobility_cz) <- tolower(
  gsub(",", "", gsub(" |-", "_", names(intergen_mobility_cz)))
)
intergen_mobility_cz <- intergen_mobility_cz[-(1:2), ]
intergen_mobility_cz[, 4:ncol(intergen_mobility_cz)] <- lapply(
  intergen_mobility_cz[, 4:ncol(intergen_mobility_cz)],
  as.numeric
)

# Import and re-structure county shape files ------------------------------

state_list <- tolower(states())
state_fips <- unique(
  subset(tigris::fips_codes, tolower(state_name) %in% state_list)$state_code
)
county_lines <- tigris::counties(
  state = state_fips,
  cb = TRUE,
  resolution = "20m",
  year = 2010,
  progress_bar = FALSE
)

# Clean county lines data
names(county_lines) <- tolower(gsub(" |-", "_", names(county_lines)))
county_lines <- dplyr::mutate(
  county_lines,
  county_fips_code = paste0(statefp, countyfp)
)

# Convert counties to commuting zones
cz_vars <- c("county_fips_code", "commuting_zone_id", "commuting_zone_name")
cz_xwalk <- intergen_mobility_county[, cz_vars]
cz <- merge(county_lines, cz_xwalk, by = "county_fips_code")
cz <- dplyr::group_by(cz, commuting_zone_id) |> 
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
cz <- dplyr::left_join(
  cz,
  dplyr::distinct(
    intergen_mobility_county[
      ,
      c("commuting_zone_id", "commuting_zone_name", "state")
    ],
    commuting_zone_id,
    .keep_all = TRUE
  ),
  by = "commuting_zone_id"
)

# Add centroids
cz_centroids <- sf::st_centroid(cz)

# Commuting Zone features in 2000 -----------------------------------------

cz_features <- readxl::read_excel(
  path = here::here("data/inter-gen-mobility-county.xls"),
  skip = 6,
  sheet = "Online Data Table 8"
)
cz_features <- cz_features[-1, ] |> janitor::clean_names()
cz_features <- cz_features |>
  dplyr::mutate(
    cz = as.character(cz),
    dplyr::across(.cols = -(cz:state), .fns = as.numeric)
  )

# Migration data ----------------------------------------------------------

opp_migration <- readr::read_csv(here::here("scratch/od_pooled.csv")) |>
  dplyr::mutate(dplyr::across(c(o_cz, d_cz), as.character))
opp_migration_ns <- opp_migration |>
  dplyr::left_join(
    dplyr::rename_with(cz_centroids, \(x) paste0("o_", x), -commuting_zone_id),
    by = c("o_cz" = "commuting_zone_id")
  ) |>
  dplyr::left_join(
    dplyr::rename_with(cz_centroids, \(x) paste0("d_", x), -commuting_zone_id),
    by = c("d_cz" = "commuting_zone_id")
  ) |>
  dplyr::mutate(
    o_d_distance = sf::st_distance(o_geometry, d_geometry, by_element = TRUE)
  ) |>
  dplyr::select(-d_geometry) |>
  dplyr::rename("geometry" = "o_geometry") |>
  sf::st_as_sf()

# Prep for SIM model

opp_migration_zones <- cz |> 
  dplyr::mutate(cz_area = as.numeric(sf::st_area(geometry))/1e6)
key_zone_names <- c("commuting_zone_id", "commuting_zone_name", "cz_area")

opp_migration_ns_modeling <- opp_migration_ns |>
  sf::st_drop_geometry() |>
  dplyr::select(
    -dplyr::contains(c("name", "state", "pool", "pr_o_d", "pr_d_o"))
  ) |>
  dplyr::left_join(
    dplyr::rename_with(
      dplyr::select(cz_features, -cz_name, -state),
      .fn = \(x) paste0("o_", x),
      .cols = -cz
    ),
    by = c("o_cz" = "cz")
  ) |>
  dplyr::left_join(
    dplyr::rename_with(
      dplyr::select(cz_features, -cz_name, -state),
      .fn = \(x) paste0("d_", x),
      .cols = -cz
    ),
    by = c("d_cz" = "cz")
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::contains("geometry"), \(x) NULL),
    o_d_distance = as.numeric(o_d_distance/1000),
    dplyr::across(
      dplyr::where(is.numeric),
      ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))
    )
  )
key_flow_names <- c("o_cz", "d_cz", "n")

# Output files
readr::write_csv(
  opp_migration_ns_modeling,
  here::here("scratch/opp_migration_with_features.csv")
)
st_write(
  dplyr::arrange(cz, commuting_zone_id),
  "scratch/commuting_zone.geojson",
  delete_dsn = TRUE
)

# Create OD dataset
od_sim <- simodels::si_to_od(opp_migration_zones, opp_migration_zones)

# Create linear regularized model
train_test_split <- rsample::initial_split(
  data = opp_migration_ns_modeling,
  prop = .6,
  strata = o_urban_areas
)
training <- rsample::training(train_test_split)

lasso_model <- glmnet::cv.glmnet(
  x = model.matrix(n ~ . - o_cz - d_cz - 1, data = training),
  y = training$n,
  alpha = .5,
  trace.it = 1
)

rf_model <- ranger::ranger(
  formula = n ~ . - o_cz - d_cz,
  data = training,
  num.trees = 1000
)

# Get predictions
testing <- rsample::testing(train_test_split)
predictions <- drop(
  predict(
    lasso_model,
    model.matrix(n ~ . - o_cz - d_cz - 1, data = testing), 
    s = "lambda.min"
  )
)
predictions <- ifelse(predictions < 0, 0, predictions)
sqrt(mean((testing$n - predictions)^2))
mean(abs(testing$n - predictions))

rf_predictions <- predict(rf_model, testing)
rf_predictions <- ifelse(rf_predictions$predictions < 0, 0, rf_predictions$predictions)
sqrt(mean((testing$n - rf_predictions)^2))
mean(abs(testing$n - rf_predictions))

## Do it split out by income

# opp_migration_income <- readr::read_csv(here::here("scratch/od_inc.csv")) |>
#   dplyr::mutate(dplyr::across(c(o_cz, d_cz), as.character))
# 
# opp_migration_income_ns <- opp_migration_income |>
#   dplyr::filter(o_cz != d_cz) |>
#   dplyr::left_join(
#     dplyr::rename_with(cz_centroids, \(x) paste0("o_", x), -commuting_zone_id),
#     by = c("o_cz" = "commuting_zone_id")
#   ) |>
#   dplyr::left_join(
#     dplyr::rename_with(cz_centroids, \(x) paste0("d_", x), -commuting_zone_id),
#     by = c("d_cz" = "commuting_zone_id")
#   ) |>
#   dplyr::mutate(
#     o_d_distance = sf::st_distance(o_geometry, d_geometry, by_element = TRUE)
#   ) |>
#   dplyr::select(-d_geometry) |>
#   dplyr::rename("geometry" = "o_geometry") |>
#   sf::st_as_sf()
# 
# opp_migration_income_ns <- sf::st_drop_geometry(opp_migration_income_ns) |>
#   tidyr::pivot_wider(
#     id_cols = -c(pr_d_o, pr_o_d),
#     names_from = "pool",
#     values_from = "n",
#     names_prefix = "n_",
#     values_fill = 0, 
#   ) |>
#   dplyr::mutate(n_all = n_Q1 + n_Q2 + n_Q3 + n_Q4 + n_Q5) |>
#   dplyr::distinct(o_cz, d_cz, .keep_all = TRUE)

