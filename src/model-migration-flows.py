import pandas as pd
from sklearn.ensemble import RandomForestRegressor

# Import data
cz_flows = pd.read_csv("./data/opp_migration_with_features.csv")
cz_flows_modeling = (
    cz_flows
    .drop(
        ["o_census_2000_population", "d_census_2000_population", "o_cz", "d_cz"],
        axis=1
    )
)
flows_X = cz_flows_modeling.drop("n", axis=1)
flows_y = cz_flows_modeling["n"].to_numpy()

# Train Random Forest
rf = RandomForestRegressor(n_estimators=100, random_state=123)
rf_fitted = rf.fit(X=flows_X, y=flows_y)