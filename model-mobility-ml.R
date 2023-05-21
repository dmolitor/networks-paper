library(dplyr)
library(ranger)
library(readr)
library(sf)
library(gt)

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
    -c(
      "o_cz",
      "d_cz",
      "o_census_2000_population",
      "d_census_2000_population",
      "o_migration_outlflow_rate",
      "d_migration_inflow_rate"
    )
  )

# Import Commuting Zones w/ predicted flows
flows_predicted <- read_sf("./scratch/cz_cpc_measures.geojson")

# Model stuff
rf_model_impurity <- ranger(
  formula = n ~ .,
  data = flows_model,
  num.trees = 100,
  importance = "impurity"
)
rf_model_impurity_corr <- ranger(
  formula = n ~ .,
  data = flows_model,
  num.trees = 100,
  importance = "impurity_corrected"
)

# Predicted flows
rf_predictions <- floor(pmax(predict(rf_model_impurity, flows_model)$predictions, 0))
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
flows_predicted <- flows_predicted |>
  left_join(predictions, by = c("tile_ID" = "o_cz"))
file.remove("./scratch/cz_cpc_measures.geojson")
sf::st_write(obj = flows_predicted, dsn = "./scratch/cz_cpc_measures.geojson")

# Var importance
tibble::enframe(rf_model_impurity$variable.importance, "variable", "importance") |>
  mutate(importance = importance/sum(importance)) |>
  arrange(desc(importance)) |>
  left_join(
    tibble::enframe(rf_model_impurity_corr$variable.importance, "variable", "corr_importance") |>
      mutate(corr_importance = corr_importance/sum(corr_importance)),
    by = "variable"
  ) |>
  arrange(desc(importance)) |>
  write_csv("./scratch/var_importance.csv")

# Create variable importance table\
var_importance <- read_csv("./scratch/var_importance.csv")
variable_names <- list(
  "o_d_distance" = "Origin to Destination Distance",
  "n_tot_o" = "Total Population at Origin",
  "n_tot_d" = "Total Population at Destination",
  "o_teacher_student_ratio" = "Teacher-Student Ratio at Origin",
  "o_segregation_of_affluence_p75" = "Segregation of Affluence (p75) at Origin",
  "o_frac_with_commute_15_mins" = "Fraction Commuting Within 15 Minutes at Origin",
  "o_frac_foreign_born" = "Fraction Foreign-Born at Origin",
  "o_income_segregation" = "Income Segregation at Origin",
  "o_local_government_expenditures_per_capita" = "Local Government Expenditures per Capita at Origin",
  "d_violent_crime_rate" = "Violent Crime Rate at Destination",
  "d_frac_foreign_born" = "Fraction Foreign-Born at Destination",
  "d_income_segregation" = "Income Segregation at Destination",
  "o_top_1_percent_income_share" = "Share of Income Held by Top 1% at Origin",
  "o_migration_inflow_rate" = "Migration Inflow Rate at Origin",
  "o_fraction_religious" = "Fraction of Religious Population at Origin",
  "o_college_graduation_rate_income_adjusted" = "Income-Adjusted College Graduation Rate at Origin",
  "d_high_school_dropout_rate_income_adjusted" = "Income-Adjusted High School Dropout Rate at Destination",
  "d_frac_with_commute_15_mins" = "Fraction Commuting Within 15 Minutes at Destination",
  "o_school_expenditure_per_student" = "School Expenditure per Student at Origin",
  "o_frac_between_p25_and_p75" = "Fraction with Income Between 25th and 75th Percentile at Origin",
  "d_teenage_labor_force_participation_rate" = "Teenage Labor Force Participation Rate at Destination",
  "o_frac_black" = "Fraction Black Population at Origin",
  "o_high_school_dropout_rate_income_adjusted" = "Income-Adjusted High School Dropout Rate at Origin",
  "d_household_income_per_capita" = "Household Income per Capita at Destination",
  "d_teacher_student_ratio" = "Teacher-Student Ratio at Destination",
  "o_violent_crime_rate" = "Violent Crime Rate at Origin",
  "o_test_score_percentile_income_adjusted" = "Income-Adjusted Test Score Percentile at Origin",
  "d_segregation_of_affluence_p75" = "Segregation of Affluence (p75) at Destination",
  "d_gini" = "Gini Coefficient at Destination",
  "o_college_tuition" = "College Tuition at Origin",
  "d_segregation_of_poverty_p25" = "Segregation of Poverty (p25) at Destination",
  "d_manufacturing_employment_share" = "Manufacturing Employment Share at Destination",
  "o_social_capital_index" = "Social Capital Index at Origin",
  "d_local_government_expenditures_per_capita" = "Local Government Expenditures per Capita at Destination",
  "o_fraction_of_adults_divorced" = "Fraction of Adults Divorced at Origin",
  "d_labor_force_participation_rate" = "Labor Force Participation Rate at Destination",
  "d_social_capital_index" = "Social Capital Index at Destination",
  d_college_graduation_rate_income_adjusted = "Income-adjusted College Graduation Rate at Destination",
  d_racial_segregation = "Racial Segregation at Destination",
  d_income_growth_2000_2006_10 = "Income Growth at Destination (2000-2006)",
  d_fraction_of_adults_married = "Fraction of Adults Married at Destination",
  d_local_tax_rate = "Local Tax Rate at Destination",
  d_test_score_percentile_income_adjusted = "Income-adjusted Test Score Percentile at Destination",
  d_fraction_religious = "Fraction Religious at Destination",
  d_frac_black = "Fraction Black at Destination",
  d_fraction_of_adults_divorced = "Fraction of Adults Divorced at Destination",
  d_growth_in_chinese_imports_1990_2000 = "Growth in Chinese Imports (1990-2000) at Destination",
  o_segregation_of_poverty_p25 = "Poverty Segregation at Origin (P25)",
  o_manufacturing_employment_share = "Share of Manufacturing Employment at Origin",
  d_top_1_percent_income_share = "Top 1% Income Share at Destination",
  o_labor_force_participation_rate = "Labor Force Participation Rate at Origin",
  o_gini = "Gini Coefficient at Origin",
  d_college_tuition = "College Tuition at Destination",
  d_migration_outlflow_rate = "Migration Outflow Rate at Destination",
  o_household_income_per_capita = "Household Income per Capita at Origin",
  o_racial_segregation = "Racial Segregation at Origin",
  o_growth_in_chinese_imports_1990_2000 = "Growth in Chinese Imports (1990-2000) at Origin",
  o_teenage_labor_force_participation_rate = "Teenage Labor Force Participation Rate at Origin",
  o_fraction_of_children_with_single_mothers = "Fraction of Children with Single Mothers at Origin",
  o_local_tax_rate = "Local Tax Rate at Origin",
  o_number_of_colleges_per_capita = "Number of Colleges per Capita at Origin",
  d_school_expenditure_per_student = "School Expenditure per Student at Destination",
  d_frac_between_p25_and_p75 = "Fraction of Income between P25 and P75 at Destination",
  d_number_of_colleges_per_capita = "Number of Colleges per Capita at Destination",
  o_fraction_of_adults_married = "Fraction of Adults Married at Origin",
  o_gini_bottom_99_percent = "Bottom 99% Gini Coefficient at Origin",
  d_gini_bottom_99_percent = "Bottom 99% Gini Coefficient at Destination",
  o_income_growth_2000_2006_10 = "Income Growth at Origin (2000-2006)",
  d_state_income_tax_progressivity = "State Income Tax Progressivity at Destination",
  o_state_income_tax_progressivity = "State Income Tax Progressivity at Origin",
  d_fraction_of_children_with_single_mothers = "Fraction of Children with Single Mothers at Destination",
  d_state_eitc_exposure = "State EITC Exposure at Destination",
  o_state_eitc_exposure = "State EITC Exposure at Origin",
  o_urban_areas = "Number of Urban Areas at Origin",
  d_urban_areas = "Number of Urban Areas at Destination"
)
var_importance_table <- var_importance |>
  left_join(
    tibble::enframe(unlist(variable_names)) |>
      mutate(
        value = gsub("\\(.*\\)", "", value),
        value = gsub("  ", " ", value)
      ),
    by = c("variable" = "name")
  ) |>
  mutate(variable = value, value = NULL, importance = round(importance, 3)) |>
  rename("Geographic Characteristic" = "variable", "Importance" = "importance") |>
  head(10) |>
  gt() |>
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = c(`Geographic Characteristic`, Importance),
      rows = corr_importance < 0
    )
  ) |>
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(
      columns = c(`Geographic Characteristic`, Importance),
      rows = corr_importance > 0
    )
  ) |>
  cols_hide(columns = corr_importance)
gtsave(var_importance_table, filename = "scratch/var-importance-table.png")
