#' @title Analyse des critères avec des algorithmes multilevel
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

analyseCritere <- function(data,df,spectral=FALSE,
                           criteriaChosen,masscrit,
                           iteration,
                           algorithme,
                           realiteTerrain,
                           filenameLabel,filenameResults,
                           verbose=FALSE)
{
  #Parameters
  
  if(criteriaChosen == "mass")
  {
    silMin = 1
  }
  else
  {
    silMin = -1
  }
  
  avgSil = 0
  minavgSil = 0
  maxavgSil = 0
  
  ari = 0
  minari = 0
  maxari = 0
  
  Vmes = 0
  minVmes = 0
  maxVmes = 0
  
  FM = 0
  minFM = 0
  maxFM = 0
  
  F1 = 0
  minF1 = 0
  maxF1 = 0
  
  Precision = 0
  minPrecision = 0
  maxPrecision = 0
  
  Recall = 0
  minRecall = 0
  maxRecall = 0
  
  Accuracy = 0
  minAccuracy = 0
  maxAccuracy = 0
  
  Overlapvar = 0
  minOverlapvar = 0
  maxOverlapvar = 0
  
  nbclusters = 0
  maxclusters = 0
  minclusters = 100
  cptm = 0
  cptp = 0
  loupe <- 0
  
  for(i in 1:iteration)
  {
    #On realise les differents algorithmes pour avoir les resultats
    if(spectral == TRUE){
      result <- Fuzzy_MSC(data,alpha=1,beta=2,delta=10,levelMax = 5,choice=algorithme,verbose=verbose, criteria = criteriaChosen,masscrit=masscrit,silMin = silMin,nstart=10)
    }
    else{
      result <- Fuzzy_MC(data,alpha=1,beta=2,delta=10,levelMax = 5,choice=algorithme,verbose=verbose, criteria = criteriaChosen,masscrit=masscrit,silMin = silMin,nstart=10)
    }
    while(criteriaChosen != "CardSil" && length(unique(result[,ncol(result)])) < realiteTerrain )
    {
      if(criteriaChosen == "mass")
      {
        silMin <- silMin - 0.05
      }
      else
      {
        silMin <- silMin + 0.05
      }
      if(spectral == TRUE){
        result <- Fuzzy_MSC(data,alpha=1,beta=2,delta=10,levelMax = 5,choice=algorithme,verbose=verbose, criteria = criteriaChosen,masscrit=masscrit,silMin = silMin,nstart=10)
      }
      else{
        result <- Fuzzy_MC(data,alpha=1,beta=2,delta=10,levelMax = 5,choice=algorithme,verbose=verbose, criteria = criteriaChosen,masscrit=masscrit,silMin = silMin,nstart=10)
      }
    }
    message(silMin)
    
    if(i == 1)
    {
      write.csv(result[,ncol(result)],filenameLabel,row.names = FALSE)
    }
    else
    {
      previousLabels <- read.csv(filenameLabel)
      unlink(filenameLabel)
      Labels <- cbind(previousLabels, result[,ncol(result)])
      write.csv(Labels,filenameLabel,row.names = FALSE)
    }
    
    clus = length(unique(result[,ncol(result)]))
    nbclusters = nbclusters + clus
    
    if(maxclusters < clus)
    {
      maxclusters <- clus
      cptp <- 0
    }
    if(minclusters > clus)
    {
      minclusters <- clus
      cptm <- 0
    }
    if(minclusters == clus)
    {
      cptm <- cptm + 1
    }
    if(maxclusters == clus)
    {
      cptp <- cptp + 1
    }
    
    totalLabel <- unique(as.factor(result[,ncol(result)]))
    index <- c(1:length(totalLabel))
    array <- data.frame("label"= index,"algo"= totalLabel)
    label <- vector(mode="numeric", length = length(data[,1]))
    for(j in 1:length(data[,1]))
    {
      label[j] <- array$label[which(result[j,ncol(result)] == array$algo)]
    }
    
    # message(label)
    
    if(max(unique(label)) != 1)
    {
      if(verbose){message("---- Calcul des statistiques ----")}
      if(verbose){message("---- Calcul de la silhouette moyenne ----")}
      sil <- cluster::silhouette(as.numeric(factor(result[,ncol(result)])),dist(data))
      recap <- summary(sil)
      avgSil = avgSil + recap$avg.width
      
      if(i == 1)
      {
        minavgSil <- recap$avg.width
        maxavgSil <- recap$avg.width
      }
      else
      {
        if(minavgSil>recap$avg.width)
        {
          minavgSil <- recap$avg.width
        }
        if(maxavgSil<recap$avg.width)
        {
          maxavgSil <- recap$avg.width
        }
      }
      
      if(verbose){message("---- Calcul de l'ARI ----")}
      currentAri <- adj.rand.index(df,as.numeric(factor(result[,ncol(result)])))
      ari = ari + currentAri
      
      if(i == 1)
      {
        minari <- currentAri
        maxari <- currentAri
      }
      else
      {
        if(minari>currentAri)
        {
          minari <- currentAri
        }
        if(maxari<currentAri)
        {
          maxari <- currentAri
        }
      }
      
      if(verbose){message("---- Calcul de la Vmeasure ----")}
      currentVmes <- v_measure(df, as.numeric(factor(result[,ncol(result)])), beta = 1)
      Vmes = Vmes + currentVmes
      
      if(i == 1)
      {
        minVmes <- currentVmes
        maxVmes <- currentVmes
      }
      else
      {
        if(minVmes>currentVmes)
        {
          minVmes <- currentVmes
        }
        if(maxVmes<currentVmes)
        {
          maxVmes <- currentVmes
        }
      }
      
      if(verbose){message("---- Calcul de la precision, recall, F1, Folkes-Mallow ----")}
      
      assign <- assignVote(df,as.integer(factor(result[,ncol(result)])))
      
      if(length(unique(factor(assign))) != length(unique(factor(df))))
      {
        manquant <- unique(df[!df %in% assign])
        cpt <- 1
        for(i in manquant)
        {
          assign[cpt] <- i
          cpt <- cpt+1
        }
      }
      
      confusion <- confusionMatrix(factor(assign),factor(df))
      
      precisionVector <- confusion$byClass[,5]
      recallVector <- confusion$byClass[,6]
      F1Vector <- confusion$byClass[,7]
      
      precisionVector[is.na(precisionVector)] <- 0
      recallVector[is.na(recallVector)] <- 0
      F1Vector[is.na(F1Vector)] <- 0
      currentFM <- sqrt(mean(recallVector)*mean(precisionVector))
      
      Precision = Precision + mean(precisionVector)
      Recall = Recall + mean(recallVector)
      F1 = F1 + mean(F1Vector)
      FM = FM + currentFM
      
      if(i == 1)
      {
        minPrecision <- mean(precisionVector)
        minRecall <- mean(recallVector)
        minF1 <- mean(F1Vector)
        maxPrecision <- mean(precisionVector)
        maxRecall <- mean(recallVector)
        maxF1 <- mean(F1Vector)
        minFM <- currentFM
        maxFM <- currentFM
      }
      else
      {
        if(minPrecision>mean(precisionVector))
        {
          minPrecision <- mean(precisionVector)
        }
        if(maxPrecision<mean(precisionVector))
        {
          maxPrecision <- mean(precisionVector)
        }
        if(minRecall>mean(recallVector))
        {
          minRecall <- mean(recallVector)
        }
        if(maxRecall<mean(recallVector))
        {
          maxRecall <- mean(recallVector)
        }
        if(minF1>mean(F1Vector))
        {
          minF1 <- mean(F1Vector)
        }
        if(maxF1<mean(F1Vector))
        {
          maxF1 <- mean(F1Vector)
        }
        if(minFM>currentFM)
        {
          minFM <- currentFM
        }
        if(maxFM<currentFM)
        {
          maxFM <- currentFM
        }
      }
      
      if(verbose){message("---- Calcul de l'Accuracy ----")}
      currentAccuracy <- unname(confusion$overall[1])
      Accuracy = Accuracy + currentAccuracy
      
      if(i == 1)
      {
        minAccuracy <- currentAccuracy
        maxAccuracy <- currentAccuracy
      }
      else
      {
        if(minAccuracy>currentAccuracy)
        {
          minAccuracy <- currentAccuracy
        }
        if(maxAccuracy<currentAccuracy)
        {
          maxAccuracy <- currentAccuracy
        }
      }
      
      if(verbose){message("---- Calcul de l'overlap ----")}
      currentOverlapvar <- Overlap(df,as.integer(factor(result[,ncol(result)])))
      Overlapvar = Overlapvar + currentOverlapvar
      
      if(i == 1)
      {
        minOverlapvar <- currentOverlapvar
        maxOverlapvar <- currentOverlapvar
      }
      else
      {
        if(minOverlapvar>currentOverlapvar)
        {
          minOverlapvar <- currentOverlapvar
        }
        if(maxOverlapvar<currentOverlapvar)
        {
          maxOverlapvar <- currentOverlapvar
        }
      }
    }
    else
    {
      loupe <- loupe+1
      nbclusters <- nbclusters - 1
    }
    
  }
  
  iteration <- iteration - loupe
  
  Final <- list("avgSil_Mean"=rbind(previousLabels$avgSil_Mean,avgSil/iteration),
                "avgSil_Min"=rbind(previousLabels$avgSil_Min,minavgSil/1),
                "avgSil_Max"=rbind(previousLabels$avgSil_Max,maxavgSil/1),
                "ari_Mean"=rbind(previousLabels$ari_Mean,ari/iteration),
                "ari_Min"=rbind(previousLabels$ari_Min,minari/1),
                "ari_Max"=rbind(previousLabels$ari_Max,maxari/1),
                "Vmes_Mean"=rbind(previousLabels$Vmes_Mean,Vmes/iteration),
                "Vmes_Min"=rbind(previousLabels$Vmes_Min,minVmes/1),
                "Vmes_Max"=rbind(previousLabels$Vmes_Max,maxVmes/1),
                "FM_Mean"=rbind(previousLabels$FM_Mean,FM/iteration),
                "FM_Min"=rbind(previousLabels$FM_Min,minFM/1),
                "FM_Max"=rbind(previousLabels$FM_Max,maxFM/1),
                "F1_Mean"=rbind(previousLabels$F1_Mean,F1/iteration),
                "F1_Min"=rbind(previousLabels$F1_Min,minF1/1),
                "F1_Max"=rbind(previousLabels$F1_Max,maxF1/1),
                "Accuracy_Mean"=rbind(previousLabels$Accuracy_Mean,Accuracy/iteration),
                "Accuracy_Min"=rbind(previousLabels$Accuracy_Min,minAccuracy/1),
                "Accuracy_Max"=rbind(previousLabels$Accuracy_Max,maxAccuracy/1),
                "Precision_Mean"=rbind(previousLabels$Precision_Mean,Precision/iteration),
                "Precision_Min"=rbind(previousLabels$Precision_Min,minPrecision/1),
                "Precision_Max"=rbind(previousLabels$Precision_Max,maxPrecision/1),
                "Recall_Mean"=rbind(previousLabels$Recall_Mean,Recall/iteration),
                "Recall_Min"=rbind(previousLabels$Recall_Min,minRecall/1),
                "Recall_Max"=rbind(previousLabels$Recall_Max,maxRecall/1),
                "Overlap_Mean"=rbind(previousLabels$Overlap_Mean,Overlapvar/iteration),
                "Overlap_Min"=rbind(previousLabels$Overlap_Min,minOverlapvar/1),
                "Overlap_Max"=rbind(previousLabels$Overlap_Max,maxOverlapvar/1),
                "Cluster_nbclusters"=rbind(previousLabels$Cluster_nbclusters,nbclusters/iteration),
                "Cluster_MaxCluster"=rbind(previousLabels$Cluster_MaxCluster,maxclusters/1),
                "Cluster_MinCluster"=rbind(previousLabels$Cluster_MinCluster,minclusters/1),
                "Cluster_nbMax"=rbind(previousLabels$Cluster_nbMax,cptp/1),
                "Cluster_nbMin"=rbind(previousLabels$Cluster_nbMin,cptm/1))
  
  saveRDS(Final,filenameResults)
}