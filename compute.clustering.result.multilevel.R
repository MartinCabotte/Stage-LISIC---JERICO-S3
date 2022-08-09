#' @title Compute multilevel clustering
#' @author Martin CABOTTE
#' @description 
#' @param silMin the threshold.
#' @param data the datapoints.
#' @param alpha ECM alpha.
#' @param beta ECM beta.
#' @param delta ECM delta.
#' @param levelMax the maximum level allowed.
#' @param algorithme "kmeans", "cmeans", "ecm"
#' @param criteria listed in compute.split.criteria.R
#' @param nstart the number of iteration to perform convergence 

compute.clustering.result.multilevel <- function(silMin,data,spectral,
                                                 alpha=1,beta=2,delta=10,
                                                 levelMax=5,facteurClustering=5,
                                                 algorithme,criteria,nstart=10,
                                                 vois=7, flagDiagZero = FALSE,Kmax=20,
                                                 tolerence=0.99,threshold=0.9,
                                                 verbose=FALSE)
{
  if(spectral == "Spectral"){
    spectral <- TRUE
  }else{
    spectral <- FALSE
  }
  
  result <- Fuzzy_MC(X = data,spectral = spectral,
                     alpha=alpha,beta=beta,delta=delta,
                     levelMax = levelMax,
                     choice=algorithme,verbose=verbose, 
                     criteria = criteria,
                     silMin = silMin,
                     nstart=nstart,facteurClustering=facteurClustering,
                     vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,tolerence=tolerence,threshold=threshold)
  
  while(criteria != "CardSil" && length(unique(result[,ncol(result)])) < realiteTerrain )
  {
    if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100")
    {
      silMin <- silMin - 0.05
    }
    else
    {
      silMin <- silMin + 0.05
    }
    result <- Fuzzy_MC(X = data,spectral = spectral,
                       alpha=alpha,beta=beta,delta=delta,
                       levelMax = levelMax,
                       choice=algorithme,verbose=verbose, 
                       criteria = criteria,
                       silMin = silMin,
                       nstart=nstart,facteurClustering=facteurClustering,
                       vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,tolerence=tolerence,threshold=threshold)
    
    print(paste("SilMin =",silMin))
  }
  
  result <- list("result"=result,"silMin"=silMin,"minPts"=NULL)
  
  result
}