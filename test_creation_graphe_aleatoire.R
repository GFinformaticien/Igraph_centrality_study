library(igraph)

creation_graphe_rand <-function(nom_func_crea){
  nb_point <- sample(5:100,1);
  dens_link <- runif(1,0.1,0.7);
  if (identical(nom_func_crea,watts.strogatz.game)) {
    return(nom_func_crea(nb_point,dens_link));
  }
  return(nom_func_crea(nb_point,dens_link,directed=FALSE));
}

