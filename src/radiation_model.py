import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
import pygris
from pygris.utils import shift_geometry
from random import sample
import skmob
from skmob.models.radiation import Radiation

def states():
    return [
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
    ]

# Opportunity Insights migration data
opp_mobility = pd.read_csv("scratch/opp_migration_with_features.csv")
# opp_mobility = opp_mobility[opp_mobility["o_cz"] != opp_mobility["d_cz"]]
opp_mobility = opp_mobility[opp_mobility["n"] > 0]

# Commuting zone shape file
cz = gpd.read_file("scratch/commuting_zone.geojson")
cz["commuting_zone_id"] = cz["commuting_zone_id"].astype(int)
cz = cz.merge(
    opp_mobility[["o_cz", "n_tot_o"]],
    left_on="commuting_zone_id",
    right_on="o_cz"
)
cz = (
    cz
    .drop("o_cz", axis=1)
    .rename(columns={"n_tot_o":"population", "commuting_zone_id":"tile_ID"})
)

# Create FlowDataFrame for skmobility
flows = skmob.FlowDataFrame(
    data=opp_mobility,
    origin="o_cz",
    destination="d_cz",
    flow="n",
    tile_id="tile_ID",
    tessellation=cz
)

# Merge total outflows to Commuting Zones data
tot_outflows = (
    flows[flows["origin"] != flows["destination"]]
    .groupby(by="origin", axis=0)[["flow"]]
    .sum()
    .fillna(0)
)
tot_outflows.index = tot_outflows.index.astype(int)
cz = (
    cz
    .merge(tot_outflows, left_on="tile_ID", right_on="origin")
    .rename(columns={"flow": "tot_outflow"})
)

# Build radiation model
subsample = sample(list(range(1, len(cz))), round(.01*len(cz)))
subsample.sort()
cz_subsample = cz.iloc[subsample]

radiation = Radiation()
rad_flows = radiation.generate(
    spatial_tessellation=cz_subsample,
    tile_id_column="tile_ID",
    tot_outflows_column="tot_outflow",
    relevance_column="population",
    out_format="flows"
)
