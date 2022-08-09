#' @title Compute spectral embedded space
#' @author Martin CABOTTE
#' @description 
#' @param data the datapoints.
#' @param vois k neighborhood.
#' @param Kmax maximum K-value.
#' @param tolerence
#' @param threshold
#' @param trueK the true K-value.

compute.spectral <- function(data,vois,Kmax,tolerence,threshold,verbose,flagDiagZero,trueK = NULL){
  W <- NULL
  verbose <- FALSE
  W <- compute.similarity.ZP(data, vois=vois)
  W <- checking.gram.similarityMatrix(W, flagDiagZero = flagDiagZero, verbose = verbose)
  W <- compute.laplacian.NJW(W, verbose = verbose)
  eigenValues <- W$eigen$values
  if(is.null(trueK)){
    kClusters <- compute.kclust(eigenValues,method = "PEV", Kmax = Kmax, 
                              tolerence =  tolerence,threshold = threshold,
                              verbose = verbose)
    if(kClusters<=1){kClusters=2}
  }else{
    kClusters <- trueK
  }
  dataSpec=W$eigen$vectors[,1:kClusters]
  dataSpec <- dataSpec/apply(dataSpec, MARGIN = 1, FUN = function(x) norm(matrix(x),"f"))
  
  returnlist <- list("eigenspace"=dataSpec,"pevK"=kClusters)
  
  returnlist
}