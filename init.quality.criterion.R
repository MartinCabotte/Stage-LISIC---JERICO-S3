#' @title Initialize quality criteria list
#' @author Martin CABOTTE
#' @description 
#' @param listQuality list of quality criteria names.
#' @return returns a list containing the following wanted initialized elements:
#' \itemize{
#'  \item{Silhouette}
#'  \item{ARI}
#'  \item{Vmeasure}
#'  \item{Folkes_Mallow index}
#'  \item{F1 score}
#'  \item{Accuracy}
#'  \item{Precision}
#'  \item{Recall}
#'  \item{Overlap}
#'  \item{Clusters}
#' }

init.quality.criterion <- function(listQuality){
  
  quality <- list("SilMin" = c())
  minPts <- list("minPts" = c())
  quality <- append(quality,minPts)
  
  if("Silhouette" %in% listQuality){
    SilArray <- list("Sil" = c())
    quality <- append(quality,SilArray)
  }
  if("ARI" %in% listQuality){
    ARIArray <- list("ARI" = c())
    quality <- append(quality,ARIArray)
  }
  if("Vmeasure" %in% listQuality){
    VmesArray <- list("Vmes" = c())
    quality <- append(quality,VmesArray)
  }
  if("Folkes-Mallow" %in% listQuality){
    FMArray <- list("FM" = c())
    quality <- append(quality,FMArray)
  }
  if("F1_Score" %in% listQuality){
    F1Array <- list("F1" = c())
    quality <- append(quality,F1Array)
  }
  if("Precision" %in% listQuality){
    PrecisionArray <- list("Precision" = c())
    quality <- append(quality,PrecisionArray)
  }
  if("Recall" %in% listQuality){
    RecallArray <- list("Recall" = c())
    quality <- append(quality,RecallArray)
  }
  if("Accuracy" %in% listQuality){
    AccuracyArray <- list("Accuracy" = c())
    quality <- append(quality,AccuracyArray)
  }
  if("Overlap" %in% listQuality){
    OverlapArray <- list("Overlap" = c())
    quality <- append(quality,OverlapArray)
  }
  
  ClustersArray <- list("Clusters" = c())
  quality <- append(quality,ClustersArray)
  
  
  quality
}