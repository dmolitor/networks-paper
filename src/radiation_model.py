import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
import pygris
from pygris.utils import shift_geometry
from random import sample
import skmob
from skmob.models.gravity import Gravity
from skmob.models.radiation import Radiation
import tempfile
import webbrowser

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
    .drop_duplicates(subset="commuting_zone_id")
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

# Build gravity model

gravity = Gravity()
gravity.fit(flows, relevance_column="population")
gravity_flows = gravity.generate(
    spatial_tessellation=cz,
    tile_id_column="tile_ID",
    tot_outflows_column="tot_outflow",
    relevance_column= "population",
    out_format="flows"
)

# Build radiation model
radiation = Radiation()
rad_flows = radiation.generate(
    spatial_tessellation=cz,
    tile_id_column="tile_ID",
    tot_outflows_column="tot_outflow",
    relevance_column="population",
    out_format="flows"
)

# Compare predicted and actual flows
def compare_flows(name, min_flow=50, opacity=.5):
    tile_id = cz[cz["commuting_zone_name"] == name]["tile_ID"].tolist()
    pred_flows = rad_flows[rad_flows["origin"] == str(tile_id[0])]
    pred_grav_flows = gravity_flows[gravity_flows["origin"] == str(tile_id[0])]
    real_flows = flows[flows["origin"] == str(tile_id[0])]
    m0 = pred_grav_flows.plot_flows(
        flow_color="green",
        min_flow = min_flow,
        zoom = 5,
        opacity=opacity
    )
    m1 = pred_flows.plot_flows(
        flow_color="blue",
        min_flow = min_flow,
        zoom = 5,
        opacity=opacity
    )
    m2 = real_flows.plot_flows(
        flow_color="red",
        min_flow = min_flow,
        zoom = 5,
        opacity=opacity
    )
    m0_path = tempfile.mktemp(suffix=".html")
    m1_path = tempfile.mktemp(suffix=".html")
    m2_path = tempfile.mktemp(suffix=".html")
    m0.save(m0_path)
    m1.save(m1_path)
    m2.save(m2_path)
    webbrowser.open("file://" + m0_path)
    webbrowser.open("file://" + m1_path)
    webbrowser.open("file://" + m2_path)

# Merge predicted and actual flow counts

## Radiation flows
flows_rad = flows.merge(
    rad_flows.rename(columns={"flow":"radius_flow"}),
    on=["origin", "destination"],
    how="outer"
)
flows_rad["radius_flow"] = flows_rad["radius_flow"].fillna(0).astype(int)
flows_rad["flow"] = flows_rad["flow"].fillna(0).astype(int)

## Gravity flows
flows_grav = flows.merge(
    gravity_flows.rename(columns={"flow":"gravity_flow"}),
    on=["origin", "destination"],
    how="outer"
)
flows_grav["gravity_flow"] = flows_grav["gravity_flow"].fillna(0).astype(int)
flows_grav["flow"] = flows_grav["flow"].fillna(0).astype(int)

# Calculate Sorensen-Dice index
def cpc(data, num_col, denom_col, digits=5):
    cpc = data[num_col].sum()/data[denom_col].sum()
    cpc = round(cpc, digits)
    return cpc

flows_rad["min_g_r_flow"] = flows_rad[["flow", "radius_flow"]].min(axis=1) * 2
flows_rad["sum_g_r_flow"] = flows_rad[["flow", "radius_flow"]].sum(axis=1)

flows_grav["min_g_r_flow"] = flows_grav[["flow", "gravity_flow"]].min(axis=1) * 2
flows_grav["sum_g_r_flow"] = flows_grav[["flow", "gravity_flow"]].sum(axis=1)

print(f"CPC Radiation model: {cpc(flows_rad, 'min_g_r_flow', 'sum_g_r_flow')}")
print(f"CPC Gravity model: {cpc(flows_grav, 'min_g_r_flow', 'sum_g_r_flow')}")

# Aggregate CPC by commuting zone
cz_gravity_cpc = (
    flows_grav
    .groupby("origin", axis=0)
    .apply(lambda x: cpc(x, "min_g_r_flow", "sum_g_r_flow"))
    .reset_index()
    .rename(columns={0:"gravity_cpc"})
)
cz_gravity_cpc["origin"] = cz_gravity_cpc["origin"].astype(int)

cz_rad_cpc = (
    flows_rad
    .groupby("origin", axis=0)
    .apply(lambda x: cpc(x, "min_g_r_flow", "sum_g_r_flow"))
    .reset_index()
    .rename(columns={0:"rad_cpc"})
)
cz_rad_cpc["origin"] = cz_rad_cpc["origin"].astype(int)

cz = cz.merge(cz_gravity_cpc, left_on="tile_ID", right_on="origin", how="left")
cz = cz.merge(cz_rad_cpc, left_on="tile_ID", right_on="origin", how="left")

# Output CPC as geojso
cz_out = cz[[
    "tile_ID", "commuting_zone_name", "state", "geometry", "population",
    "tot_outflow", "gravity_cpc", "rad_cpc"
]]
cz_out.to_file("scratch/cz_cpc_measures.geojson")
