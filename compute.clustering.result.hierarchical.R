compute.clustering.result.hierarchical <- function(data,spectral,
                                                   alpha=1,beta=2,delta=10,
                                                   levelMax=5,
                                                   method = "ward.D2",realiteTerrain,
                                                   algorithme,criteria,nstart=10,
                                                   vois=7, flagDiagZero = FALSE,Kmax=20,
                                                   tolerence=0.99,threshold=0.9,
                                                   verbose=FALSE,minPtsMax = 200){
  dataToCompute <- c()
  result <- c()
  
  l <- 200 #Starting point of convergence, if l isn't large enought, before = current
  K <- realiteTerrain
  if(spectral == "Spectral"){
    spectralData <- compute.spectral(data,vois,Kmax,tolerence,threshold,verbose,flagDiagZero,trueK=realiteTerrain)
    dataToCompute <- spectralData$eigenspace
  }else{
    dataToCompute <- data
  }
  
  if(algorithme == "hclust"){
    graph <- hclust(dist(dataToCompute), method = method)
    result <- cutree(graph,K)
  }
  if(algorithme == "hdbscanbefore" || algorithme == "hdbscanafter"){
    beforeTrueK <- c()
    currentTrueK <- c()
    
    stop <- FALSE
    
    while(!stop)
    {
      beforeTrueK <- currentTrueK
      currentTrueK <- hdbscan(dataToCompute,minPts = l)
      if(length(unique(currentTrueK$cluster)) != 0 && max(unique(currentTrueK$cluster))>realiteTerrain)
      {
        stop <- TRUE
      }
      l <- l-1
    }
    result <- currentTrueK$cluster
    
    if(algorithme == "hdbscanbefore" && stop==TRUE){result <- beforeTrueK$cluster}
  }
  
  result <- list("result"=result,"silMin"=NULL,"minPts"=l)
  
  result
}