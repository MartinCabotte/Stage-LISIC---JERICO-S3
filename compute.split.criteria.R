#' @title Compute split criterion
#' @author Martin CABOTTE
#' @description 
#' @param data the datapoints.
#' @param clustering the clustering result.
#' @param criteria name of the plit criterion.
 
compute.split.criteria <- function(data,clusters,membership = NULL,criteria,distMatrix = NULL,verbose=FALSE){
  
  silLevel <- c()
  
  if(criteria == "FuzzySilhouette"){
    silLevel <- sapply(unique(clusters), FUN = function(x){
      FSIL(data,clusters,x,membership)
    })
  }else if(criteria == "Mass25"){
    for(x in 1:length(unique(clusters)))
    {
      sortedmass <- sort(membership[clusters == x,x])
      sortedmasslow <- sortedmass[1:length(sortedmass)/4]
      if(length(sortedmasslow) < 4){
        silLevel <- c(silLevel,0)
      }else{silLevel <- c(silLevel,sum(sortedmasslow)/length(sortedmasslow))}
    }
  }else if(criteria == "Mass50"){
    for(x in 1:length(unique(clusters)))
    {
      sortedmass <- sort(membership[clusters == x,x])
      sortedmasslow <- sortedmass[1:length(sortedmass)/2]
      if(length(sortedmasslow) < 2){
        silLevel <- c(silLevel,0)
      }else{silLevel <- c(silLevel,sum(sortedmasslow)/length(sortedmasslow))}
    }
  }else if(criteria == "Mass100"){
    for(x in 1:length(unique(clusters)))
    {
      sortedmass <- sort(membership[clusters == x,x])
      sortedmasslow <- sortedmass[1:length(sortedmass)]
      silLevel <- c(silLevel,sum(sortedmasslow)/length(sortedmasslow))
    }
  }else if(criteria == "CardSil"){
    silLevel <- sapply(unique(clusters), FUN = function(x){
      sum(cluster::silhouette(clusters,distMatrix)[,"sil_width"][which(clusters == x)] < 0)
    })
  }else if(criteria == "Silhouette"){
    silLevel <- sapply(unique(clusters), FUN = function(x){
      mean(cluster::silhouette(clusters,distMatrix)[,"sil_width"][which(clusters == x)])
    })
  }
  
  if(verbose){print(silLevel)}
  
  silLevel
}