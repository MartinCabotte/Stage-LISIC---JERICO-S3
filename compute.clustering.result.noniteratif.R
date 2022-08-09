compute.clustering.result.noniteratif <- function(algorithme,realiteTerrain,nstart,
                                                  data,spectral,
                                                  alpha,beta,delta,
                                                  vois,Kmax,tolerence,threshold,
                                                  verbose,flagDiagZero)
{
  dataToCompute <- c()
  result <- c()
  if(spectral == "Spectral"){
    spectralData <- compute.spectral(data,vois,Kmax,tolerence,threshold,verbose,flagDiagZero,trueK=realiteTerrain)
    dataToCompute <- spectralData$eigenspace
  }else{
    dataToCompute <- data
  }
  
  
  if(algorithme == "kmeans"){
    result <- kmeans(dataToCompute,realiteTerrain,nstart = nstart)$cluster
  }else if(algorithme == "cmeans"){
    result <- convergenceCmeans(dataToCompute,realiteTerrain,convergence = nstart)$cluster
  }else if(algorithme == "ecm"){
    result <- ecm(dataToCompute,realiteTerrain,alpha = alpha,beta=beta,delta=delta,disp=FALSE,ntrials=nstart)$y.bel
  }else{
    stop("Choose a correct non iterative algorithm")
  }
  
  result <- list("result"=result,"silMin"=NULL,"minPts"=NULL)

  result
}