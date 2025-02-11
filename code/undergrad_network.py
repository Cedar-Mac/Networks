import pandas as pd
import igraph
import matplotlib.pyplot as plt

diff_17 = pd.read_csv("../data/2017_diff_corr.csv")
diff_17[((diff_17 < 0.8) & (diff_17 > 0)) | ((diff_17 > -0.8) & (diff_17 < 0))] = 0
diff_17_noweight = diff_17.astype(bool).astype(int)

diff_18 = pd.read_csv("../data/2018_diff_corr.csv")
diff_17[((diff_18 < 0.8) & (diff_18 > 0)) | ((diff_18 > -0.8) & (diff_18 < 0))] = 0
diff_18_noweight = diff_18.astype(bool).astype(int)

g_17 = igraph.Graph.Adjacency(diff_17_noweight, mode = "undirected", loops=False)

g_18 = igraph.Graph.Adjacency(diff_17_noweight, mode = "undirected", loops=False)

# Graph
fig, ax = plt.subplots()
igraph.plot(g_18, target=ax)
plt.show()
