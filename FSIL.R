#' @title Compute fuzzy silhouette of a cluster
#' @author Martin CABOTTE
#' @description 
#' @param data the dataset.
#' @param clustering clustering labels.
#' @param clusterToCompute the cluster's number to compute.
#' @param membership membership matrix of the dataset.
#' @return returns the fuzzy silhouette criterion of a given cluster
#' @references Fuzzy Silhouette formula by R.J.G.B. Campello and E.R. Hruschka

FSIL <- function(data,clustering,clusterToCompute,membership)
{
  #First of all, extract points of a cluster
  clusterToCalculate <- data[clustering == clusterToCompute,]
  
  if(length(clusterToCalculate[,1]) <= 1){Fuzzy_Silhouette <- 1}
  else{
    #Then, we can calculate to value of s of each point
    s <- cluster::silhouette(clustering,dist(data))[,"sil_width"]
    
    #Extract the ones we need
    sCluster <- s[clustering == clusterToCompute]
    
    #Extract membership matrix of the points we want
    membershipCluster <- membership[clustering == clusterToCompute,]
    
    #Initialization
    num <- 0
    den <- 0
    
    #ForLoop over all the cluster's points
    for(i in 1:length(clusterToCalculate[,1]))
    {
      #Sort membership matrix decreasingly
      sortedW <- sort(membershipCluster[i,],decreasing = TRUE)
      
      num <- num + (sortedW[1]-sortedW[2])*sCluster[i]
      den <- den + (sortedW[1]-sortedW[2])
    }
    
    Fuzzy_Silhouette <- num/den
  }
  
  out <- Fuzzy_Silhouette
}