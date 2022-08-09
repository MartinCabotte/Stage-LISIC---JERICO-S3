#' @title Calcule l'appartenance d'un point à des centres
#' @author Martin CABOTTE
#' @description 
#' @param point coordonnées du point.
#' @param centers coordonnées des centres à caractériser.

compute.point.membership <- function(point,centers){
  
  membership <- vector(length=length(centers[,1]))
  
  for(j in 1:length(membership))
  {
    sum <- 0
    dij <- sqrt(rowSums((point - centers[j,])^2))
    for(k in 1:length(membership))
    {
      dik <- sqrt(rowSums((point - centers[k,])^2))
      
      sum <- sum + (dij/dik)^2
    }  
    membership[j] <- 1/sum
  }
  
  membership
}