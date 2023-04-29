library(dplyr)
library(ranger)
library(readr)
library(sf)

# Calculate Sorensen-Dice index and RMSE
cpc <- function(num_col, denom_col, digits = 5) {
  cpc <- sum(num_col)/sum(denom_col)
  cpc <- round(cpc, digits)
  cpc
}

rmse <- function(truth, estimate, digits = 5) {
  rmse <- sqrt(mean((estimate - truth)^2))
  return(rmse)
}

# Import flows data
flows <- read_csv("./data/opp_migration_with_features.csv")
flows_model <- flows |>
  select(
    -c("o_cz", "d_cz", "o_census_2000_population", "d_census_2000_population")
  )

# Import Commuting Zones w/ predicted flows
flows_predicted <- read_sf("./data/cz_cpc_measures.geojson")

# Model stuff
rf_model <- ranger(
  formula = n ~ .,
  data = flows_model,
  num.trees = 100,
  importance = "impurity"
)

# Predicted flows
rf_predictions <- floor(pmax(predict(rf_model, flows_model)$predictions, 0))
predictions <- flows |>
  select(c("o_cz", "d_cz", "n")) |>
  bind_cols(rf_predictions = rf_predictions) |>
  filter(o_cz != d_cz, n != 0 | rf_predictions != 0) |>
  mutate(
    min_g_r_flow = pmin(n, rf_predictions),
    sum_g_r_flow = n + rf_predictions
  )
cat("Overall CPC RF model:", cpc(predictions$min_g_r_flow, predictions$sum_g_r_flow))
cat("Overall RMSE RF model:", rmse(predictions$n, predictions$rf_predictions))

predictions <- predictions |>
  summarise(
    rf_cpc = cpc(min_g_r_flow, sum_g_r_flow),
    rf_rmse = rmse(n, rf_predictions),
    .by = o_cz
  )

# Merge to overall predictions
flows_predicted |>
  left_join(predictions, by = c("tile_ID" = "o_cz")) |>
  sf::st_write(dsn = "./data/cz_cpc_measures2.geojson")

# Var importance
tibble::enframe(rf_model$variable.importance, "variable", "importance") |>
  mutate(importance = importance/sum(importance)) |>
  arrange(desc(importance)) |>
  write_csv("./data/var_importance.csv")
