#' @title Assigne a une racine les sous clusters qui en découlent
#' @author Martin CABOTTE
#' @description 
#' @param clustering the resulting sub-clustering.
#' @param affectation an array where first row corresponds to the root and next rows are clusters to affect to the root.
#' @param nbBranch number of final clusters.
#' @return returns an affectation of subclusters to clusters.

assign.tree <- function(clustering,affectation,nbBranch){
  
  KFinal <- nbBranch
  cluster <- clustering$cluster
  newcluster <- vector(length = length(cluster))
  
  for(i in 1:KFinal)
  {
    j <- 1
    while(affectation[j,i] != 0)
    {
      newcluster[which(cluster == affectation[j,i])] <- i
      j <- j+1
    }
  }
  
  newcluster
}