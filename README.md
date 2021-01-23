# LinkPrediction
Sampling and Hierarchical random graph methods for link prediction

	Hierarchical random graph is a method which is used for analyzing the well-formed network. In this method we convert a network into a hierarchical dendrogram and with sampling of dendrograms using MCMC – Markov chain and Monte Carlo method by changing edges and finding the best possible fit dendrogram for the network with this the resulted dendrogram helps us to understand the behavior of the network and also gives valuable insights of possible linkages in the network with the probabilities.

Packages Used: igraph, igraphdata
Dataset: karate and kite 
Methods: predict_edges,add_edges,delete_edges

Goal: Delete the edges from the network and use the MCMC of the predict_edges method to check for the deleted edges in the predicted list of edges and verify its results

Newly created functions:
fnseqedgesdel – which returns the random sequence of edge indexes that to be deleted on the input of graph and percent of edges to be deleted

fnlistofedgesdel – which returns the edges with vertices which will be deleted based on the input of the graph and sequence of edge ids

ffinddeledgesinpred – which returns the deleted edges found in predicted edges with their probabilities 
which will be helpful to understand the accuracy of the MCMC method to predict the new edges

faddedgestograph – which adds predicted edges to the graph if the edge is one of the deleted edges then it adds as green to show the edge was recovered from predicted edges if its new edge then it will be added as red

KarateDataset: Social network which has two fighting clubs A and H and how its members are connected.

Observation: Prediction of edges using the hierarchical random graph which will open new possibilities when applied onto real network as it can predict possible new linkages and with probabilities so analyzing such links will give some new insights. 

Possible usage in finding new chemicals: When analyzing network of chemical structure of complex compounds, using this method will find new links between molecular structures which will be able to produce new chemicals compounds without any real experiments. 
