import pandas as pd
import igraph
import matplotlib.pyplot as plt

diff_17 = pd.read_csv("../data/2017_diff_corr.csv")
diff_17[((diff_17 < 0.7) & (diff_17 > 0)) | ((diff_17 > -0.7) & (diff_17 < 0))] = 0

diff_18 = pd.read_csv("../data/2018_diff_corr.csv")
diff_18[((diff_18 < 0.7) & (diff_18 > 0)) | ((diff_18 > -0.7) & (diff_18 < 0))] = 0


g_17 = igraph.Graph.Weighted_Adjacency(diff_17, mode="undirected", loops=False)
g_17.vs["label"] = diff_17.columns


g_18 = igraph.Graph.Weighted_Adjacency(diff_18, mode="undirected", loops=False)
g_18.vs["label"] = diff_18.columns

# Graph
visual_style = {}
visual_style["edge_width"] = [w * 1.5 for w in g_18.es["weight"]]
visual_style["edge_color"] = ['red' if g_18.es[e]['weight'] > 0 else 'blue' for e in range(g_18.ecount())]

fig, ax = plt.subplots()
igraph.plot(g_18, target=ax, **visual_style)
fig.savefig("../2018_network.png", bbox_inches='tight')
plt.show()

