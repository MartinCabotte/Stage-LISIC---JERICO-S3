#' @title Calcule l'appartenance cluster à cluster
#' @author Martin CABOTTE
#' @description 
#' @param kClusters le nombre de clusters.
#' @param clustering le clustering à calculer.
#' @return retourne une matrice d'appartenance cluster à cluster.

clusterToClusterMembership <- function(kClusters,clustering){
  
  membership <- matrix(NA,ncol=kClusters,nrow=kClusters)
  
  # 100% des points
  for(i in 1:kClusters)
  {
    for(j in 1:kClusters)
    {
      membership[i,j] <- sum(clustering$membership[which(clustering$cluster == i),j])/sum(clustering$cluster == i)
    }
    
    self <- max(membership[i,])
    
    for(j in 1:kClusters)
    {
      if(i != j){membership[i,j] <- membership[i,j]/(1-self)}
      else{membership[i,j] <- 0}
    }
  }
  
  membership
}