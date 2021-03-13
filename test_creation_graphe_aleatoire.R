library(igraph)

getMaxComponent<-function(g){
  cl<-clusters(g)
  nodes<-which(cl$membership==which.max(cl$csize))
  return (induced.subgraph(g,nodes))
}


creation_graphe_rand <-function(nom_func_crea){
  repeat{
    nb_point <- sample(9:100,1);
    dens_link <- runif(1,0.1,0.7);
    if (identical(nom_func_crea,watts.strogatz.game)) {
      g <- (nom_func_crea(nb_point,dens_link));
    }
    g <- (nom_func_crea(nb_point,dens_link,directed=FALSE));
    sg <- getMaxComponent(g);
    if(length(V(sg))>8) return(sg);
  }
}

