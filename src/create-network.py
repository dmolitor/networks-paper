import matplotlib.pyplot as plt
import networkx as nx
import pandas as pd

print("Importing migration data ...")
migration = pd.read_csv("scratch/od_pooled.csv")
migration = migration[migration["n"] != 0]
migration["o_name"] = migration["o_cz_name"] + ", " +  migration["o_state_name"]
migration["d_name"] = migration["d_cz_name"] + ", " +  migration["d_state_name"]

print("Creating migration directed graph ...")
migration = migration[["o_name", "d_name", "n", "pr_d_o", "pr_o_d"]]
migration_net = nx.from_pandas_edgelist(
    df=migration,
    source="o_name",
    target="d_name",
    edge_attr=True,
    create_using=nx.DiGraph
)

# print("Generating quick viz ...")
# pos = nx.spring_layout(G=migration_net, weight='n')
# nx.draw_networkx_nodes(migration_net, pos=pos)

print("Outputting network data ...")
nx.write_graphml(migration_net, "scratch/migration_network.graphml")
