
centralities_correlations <- function(centralities){
  coord <- c();
  for( i in 1:length(centralities)){
    for(j in 1:length(centralities)){
      if(j>i){
        coord <- c(coord, cor(centralities[[i]], centralities[[j]]));
      }
    }
  }
  return(coord);
}

centralities_correlations(list(c(1,2,3,4,5),c(5,4,3,2,1),c(1,4,2,5,3),c(3,5,10,2,4)));