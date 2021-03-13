# Title     : TODO
# Objective : TODO
# Created by: guifo
# Created on: 02/03/2021

library(igraph)
library(lsa)
library('rgl')
source('test_creation_graphe_aleatoire.R')
source('centralities_correlation.R')




NUMBER_CENTRALITY <- 8;
list_centrality <- c( betweenness, degree, coreness ,eigen_centrality, closeness ,hub_score, page_rank);
list_graph <- c(barabasi.game,erdos.renyi.game, forest.fire.game);#, watts.strogatz.game);
G <- c()

NB_TYPE <- 100;
NB_GRAPH <- NB_TYPE*length(list_graph);

for(g in list_graph){
  for(i in 1:NB_TYPE){
    G <- c(G,list(creation_graphe_rand(g)));
  }
}

centralities_matrix <- c();

for(g in G){
  centrality_matrix <- matrix(0,nrow = length(V(g)), ncol=NUMBER_CENTRALITY);
  
  i<-1
  for (centrality in list_centrality) {
    
    centrality_matrix[,i] <- if (is.list(centrality(g))) {centrality(g)$vector;} else {centrality(g);}
     
    i <- i+1;
  }
  centralities_matrix <- c(centralities_matrix,list(centrality_matrix));
}

modified_centralities <- c();
for(cm in centralities_matrix){
  modified_centralities <- c(modified_centralities, list(princomp(cm, scores=TRUE)$score[,1:3]));
}

coords <- c();
for(c in modified_centralities){
  coord <- centralities_correlations(c);
  coords <- c(coords,list(coord));
}

x<-c();
y<-c();
z<-c();
for(c in coords){
  x <- c(x,c[1]);
  y <- c(y,c[2]);
  z <- c(z,c[3]);
  
}

print(coords[1])

coords <- matrix(unlist(coords), ncol = 3, byrow = TRUE);
clusters = kmeans(coords,centers = 3)$cluster;
palette(rainbow(3));

plot3d(x,y,z, type = "p", col=clusters);



# i<-0
# score_centrality <-c();
# sum_similarity <- c();
# for(i in c(1:NUMBER_CENTRALITY)){
#   for(j in c(1:NUMBER_CENTRALITY)){
#     sum_similarity <- sum(sum_similarity, cosine(centrality_matrix[,c(i,j)]));
# }
#   
#   score_centrality <- c(score_centrality,sum_similarity);
#   sum_similarity <- 0;
# }
# 
# print(score_centrality)

