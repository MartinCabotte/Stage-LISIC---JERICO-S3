#' @title Analyse des critères avec des algorithmes multilevel (2D)
#' @author Martin CABOTTE
#' @description 
#' @param data the datapoints.
#' @param df the dataframe.
#' @param criteriaChosen criteria to be computed.
#' @param masscrit number of boundary points computed.
#' @param spectral specify is the analysis is in spectral embedded space or initial rought feature plan.
#' @param realiteTerrain the true K-number.
#' @param iteration The number of iterations in order to compute mean value.
#' @param algorithm the chosen algorithm : "kmean", "cmean", "ecmbel" for y.bel in ecm, "ecmpl" for y.pl in ecm.
#' @param filenameLabel filename of resulting clustering label computation.
#' @param filenameResults filename of resulting criteria computation.
#' @param verbose to plot images and print relevant information.
#' @return returns a list containing the following elements:
#' \itemize{
#'  \item{Silhouette: }{min, max and mean value}
#'  \item{ARI: }{min, max and mean value}
#'  \item{Vmeasure: }{min, max and mean value}
#'  \item{Folkes_Mallow index: }{min, max and mean value}
#'  \item{F1 score: }{min, max and mean value}
#'  \item{Accuracy: }{min, max and mean value}
#'  \item{Precision: }{min, max and mean value}
#'  \item{Recall: }{min, max and mean value}
#'  \item{Overlap: }{min, max and mean value}
#'  \item{Clusters: }{min, max, mean value as well as number of min and max values computed (i.e nbMax = 3 means that algorithms performed 3 times the max number of final clusters)}
#' }

analyseCritere <- function(df,labels=NULL,spectral="Initial",
                           criteriaChosen = "NONE",
                           iteration,
                           algorithme,
                           realiteTerrain, method = "ward.D2",
                           filenameLabel,filenameResults,
                           verbose=FALSE,facteurClustering=5,
                           nonIteratif=FALSE, Hierarchical = FALSE,
                           levelMax=5,alpha=1,beta=2,delta=10,
                           vois=7, flagDiagZero = FALSE,Kmax=20,
                           tolerence=0.99,threshold=0.9,
                           qualityCriteria = list("Silhouette","ARI","Vmeasure","Folkes-Mallow","F1_Score","Precision","Recall","Accuracy","Overlap"),
                           relevantCriteria = list("Mean","Min","Max","EC"))
{
  if(is.null(labels))
  {
    data <- df[,-3]
    trueLabels <- df[,3]
  }
  else
  {
    data <- df
    trueLabels <- labels
  }
  
  
  #Parameters
  if(criteriaChosen == "Mass25" || criteriaChosen == "Mass50" || criteriaChosen == "Mass100")
  {
    silMin = 1
  }
  else
  {
    silMin = -1
  }
  
  minPts <- c()
  result <- c()
  quality <- init.quality.criterion(qualityCriteria)
  
  for(i in 1:iteration)
  {
    #On realise les differents algorithmes pour avoir les resultats
    if(nonIteratif == TRUE){
      result <- compute.clustering.result.noniteratif(algorithme=algorithme,realiteTerrain=realiteTerrain,nstart=iteration,
                                                      data=data,spectral=spectral,
                                                      alpha=alpha,beta=beta,delta=delta,
                                                      vois=vois,Kmax=Kmax,tolerence=tolerence,threshold=threshold,
                                                      verbose=verbose,flagDiagZero=flagDiagZero)
      silMin <- NULL
      minPts <- NULL
      result <- result$result
    }else if(Hierarchical == TRUE){
      result <- compute.clustering.result.hierarchical(data = data,spectral = spectral,
                                                       alpha=alpha,beta=beta,delta=delta,
                                                       levelMax=levelMax,method = method, realiteTerrain=realiteTerrain,
                                                       algorithme = algorithme,criteria = criteriaChosen,nstart=iteration,
                                                       vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,
                                                       tolerence=tolerence,threshold=threshold,
                                                       verbose=verbose)
      silMin <- NULL
      minPts <- result$minPts
      result <- result$result
    }else{
      result <- compute.clustering.result.multilevel(silMin = silMin,data = data,spectral = spectral,
                                                     alpha=alpha,beta=beta,delta=delta,
                                                     levelMax=levelMax,facteurClustering=facteurClustering,
                                                     algorithme = algorithme,criteria = criteriaChosen,nstart=iteration,
                                                     vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,
                                                     tolerence=tolerence,threshold=threshold,
                                                     verbose=verbose)
      silMin <- result$silMin
      minPts <- NULL
      result <- result$result
    }
    
    result <- as.matrix(result) ##If returned value is a vector, convert to matrix
    quality$SilMin <- c(quality$SilMin,silMin)
    quality$minPts <- c(quality$minPts,minPts)
    
    write.labels(result[,ncol(result)],filenameLabel,i)
    
    currentLabels <- as.numeric(factor(result[,ncol(result)]))
    
    numberOfClusters = length(unique(currentLabels))
    quality$Clusters <- c(quality$Clusters,numberOfClusters)
    
    if(length(unique(currentLabels)) != 1)
    {
      if(verbose){message("---- Calcul des statistiques ----")}
      if(verbose){message("---- Calcul de la silhouette moyenne ----")}
      sil <- cluster::silhouette(currentLabels,dist(data))
      recap <- summary(sil)
      quality$Sil <- c(quality$Sil,recap$avg.width)
      
      
      if(verbose){message("---- Calcul de l'ARI ----")}
      currentAri <- adj.rand.index(trueLabels,currentLabels)
      quality$ARI <- c(quality$ARI,currentAri)
      
      if(verbose){message("---- Calcul de la Vmeasure ----")}
      currentVmes <- v_measure(trueLabels, currentLabels, beta = 1)
      quality$Vmes <- c(quality$Vmes,currentVmes)
      
      
      if(verbose){message("---- Calcul de la precision, recall, F1, Folkes-Mallow ----")}
      assign <- assignVote(trueLabels,currentLabels)
      
      if(length(unique(factor(assign))) != length(unique(factor(trueLabels))))
      {
        manquant <- unique(trueLabels[!trueLabels %in% assign])
        cpt <- 1
        for(p in manquant)
        {
          assign[cpt] <- p
          cpt <- cpt+1
        }
      }
      
      print(factor(assign))
      print(unique(assign))
      print(factor(trueLabels))
      print(unique(trueLabels))
      
      
      confusion <- confusionMatrix(factor(assign),factor(trueLabels))
      
      precisionVector <- confusion$byClass[,5]
      recallVector <- confusion$byClass[,6]
      F1Vector <- confusion$byClass[,7]
      
      precisionVector[is.na(precisionVector)] <- 0
      recallVector[is.na(recallVector)] <- 0
      F1Vector[is.na(F1Vector)] <- 0
      currentFM <- sqrt(mean(recallVector)*mean(precisionVector))
      
      quality$Precision <- c(quality$Precision,mean(precisionVector))
      quality$Recall <- c(quality$Recall,mean(recallVector))
      quality$F1 <- c(quality$F1,mean(F1Vector))
      quality$FM <- c(quality$FM,currentFM)
      
      if(verbose){message("---- Calcul de l'Accuracy ----")}
      currentAccuracy <- unname(confusion$overall[1])
      quality$Accuracy <- c(quality$Accuracy,currentAccuracy)
      
      if(verbose){message("---- Calcul de l'overlap ----")}
      currentOverlapvar <- Overlap(trueLabels,currentLabels)
      quality$Overlap <- c(quality$Overlap,currentOverlapvar)
    }
  }
  
  toSave <- extract.relevant.values(quality,relevantCriteria)
  
  saveRDS(toSave,filenameResults)
}