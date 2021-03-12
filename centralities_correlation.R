
centralities_correlations <- function(centralities){
  coord <- c();
  print(1:(dim(centralities)[2]));
  for( i in c(1:(dim(centralities)[2]))){
    for(j in c(1:(dim(centralities)[2]))){
      if(j>i){
        coord <- c(coord, cor(centralities[,i], centralities[,j]));
      }
    }
  }
  return(coord);
}
