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
list_centrality <- c(alpha_centrality, authority_score, closeness ,hub_score, betweenness, degree, page_rank, coreness);
list_graph <- c(barabasi.game,erdos.renyi.game, forest.fire.game);#, watts.strogatz.game);
G <- c()

NB_TYPE <- 2;
NB_GRAPH <- NB_TYPE*length(list_graph);

for(g in list_graph){
  for(i in 1:NB_TYPE){
    #print(g);
    G <- c(G,list(creation_graphe_rand(g)));
  }
}

#print(G)
centralities_matrix <- c();

for(g in G){
  centrality_matrix <- matrix(0,nrow = length(V(g)), ncol=NUMBER_CENTRALITY);
  
  i<-1
  for (centrality in list_centrality) {
    
    centrality_matrix[,i] <- if (is.list(centrality(g))) {centrality(g)$vector;} else {centrality(g);}
    print(centralities_matrix);
     
    i <- i+1;
  }
  centralities_matrix <- c(centralities_matrix,centrality_matrix);
}

modified_centralities <- c();
for(cm in centralities_matrix){
  modified_centralities <- c(modified_centralities, princomp(cm, scores=TRUE)$score[,1:3]);
}

po <- centralities_correlations(modified_centralities);
print(po[1]);
plot3d(x=c(0,po[1]),y=c(0,po[2]),z=c(0,po[3]), type = "p", col="red");



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

