#' @title Fait converger le C-means
#' @author Martin CABOTTE

convergenceCmeans <- function(df,kClusters,convergence,verbose=FALSE){
  
  df <- df
  clus <- cmeans(df,kClusters)
  within <- clus$withinerror
  clustering <- clus
  for(i in 2:convergence)
  {
    clus <- cmeans(df,kClusters)
    if(within > clus$withinerror)
    {
      within <- clus$withinerror
      if(verbose){message(within)}
      clustering <- clus
    }
  }
  
  clustering
}