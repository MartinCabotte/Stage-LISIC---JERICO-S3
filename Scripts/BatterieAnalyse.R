#' @title Analyse un dataset complet automatiquement et réalisation d'un .csv pour compiler les résultats
#' @author Martin CABOTTE
#' @description Perform algorithm and split criteria analysis. In order to run the code, please set up dataframe name!
#' @return save results in the working directory (labels and lists)


##############################################
#############                    #############
#############  Parameters setup  #############
#############                    #############
##############################################

##Root of the project
root <- "~/Stage/Tests/AnalysisFiles/"

##Directory where results are saved
saveDirectory <- "~/Stage/Tests/AnalyseValidation/"

##Number of iteration of the code to perform
iteration = 10

##Maximum final clusters
Kmax=7

##Name of the dataframe to be analysed
nameDf <- "Coumpound"

verbose <- FALSE


##Analysis in spectral or rought feature plan or both ?
isSpectral = TRUE
isRought = TRUE

##Algorithms to compute
AllAlgorithms = TRUE
algorithme <- c()
algorithmeHierarchique <- c()
algorithmeNonIteratif <- c()
nbcriteria <- c()

algorithme = list("mbscan")
algorithmeHierarchique = list()
algorithmeNonIteratif = list()

##If all algorithms are not computed, please select the one you want according to
##the names entered in the compute.algorithm.R (3 list have to be setup : algorithme, 
##algorithmeHierarchique, algorithmeNonIteratif)
#Example of lists : algorithme = list("kmeans","cmeans","ecm")
#                   algorithmeHierarchique = list("hclust","hdbscan")
#                   algorithmeNonIteratif = list("kmeans","cmeans","ecm")

##The, set the nbcriteria list according to the number of first split criteria to compute
##in multilevel algorithms
#Default nbcriteria (for multilevel) : nbcriteria <- c(2,6,6)



##Split criteria to compute
AllSplitCriteria = TRUE
criteria <- c()
criteria = list("Mass25","Mass100")
nbcriteria <- c(2)

##If all split criteria are not computed, please select the one you want according to
##the names entered in the compute.split.criteria.R

#Example of list : criteria = list("CardSil","Silhouette","Mass50","Mass100")


##Spectral parameters :
vois=3
flagDiagZero = TRUE
tolerence=0.999
threshold=0.9

##Want to compute for quality criteria
relevantCriteria = list("Mean","Min","Max","EC")

##Method used in hclust, default is WardD2
method = "ward.D2"

##Number of subclusters per cluster in mbscan
facteurClustering=5

###############################################
#############                     #############
#############  Do not change the  #############
#############   following lines   #############
#############                     #############
###############################################

if(AllAlgorithms == TRUE)
{
  algorithme = list("kmeans","cmeans","ecm")
  algorithmeHierarchique = list("hclust","hdbscanafter","hdbscanbefore")
  algorithmeNonIteratif = list("kmeans","cmeans","ecm")
}


if(AllSplitCriteria == TRUE)
{
  criteria = list("CardSil","Silhouette","FuzzySilhouette","Mass25","Mass50","Mass100")
  nbcriteria <- c(2,6,6)
}


spectral <- c()
if(isSpectral == TRUE && isRought == TRUE){
  spectral <- list("Initial","Spectral")
}else if(isSpectral == TRUE){
  spectral <- list("Spectral")
}else if(isRought == TRUE){
  spectral <- list("Initial")
}

library(evclust)
library(sClust)
library(e1071)
library(dbscan)
library(pdfCluster)
library(FDRSeg)
library(caret)
library(fpc)
library(fclust)
library(cluster)
library(Rfast)

setwd(dir=root)

source(file = "Fuzzy_MC.R")
source(file = "FSIL.R")
source(file = "assignVote.R")
source(file = "analyseCritere.R")
source(file = "Overlap.R")
source(file = "load.dataframe.R")
source(file = "compute.split.criteria.R")
source(file = "convergenceCmeans.R")
source(file = "compute.spectral.R")
source(file = "compute.clustering.result.multilevel.R")
source(file = "compute.clustering.result.hierarchical.R")
source(file = "compute.clustering.result.noniteratif.R")
source(file = "extract.relevant.values.R")
source(file = "write.labels.R")
source(file = "init.quality.criterion.R")
source(file = "Kbrut.R")
source(file = "compute_cmeans_fusion.R")
source(file = "removeNoise.R")
source(file = "clusterToClusterMembership.R")
source(file = "removeNoiseDistance.R")
source(file = "compute.root.tree.R")
source(file = "unfold.tree.R")
source(file = "assign.tree.R")
source(file = "spanningTree.R")
source(file = "buildBranch.R")
source(file = "assignNoise.R")
source(file = "compute.point.membership.R")
source(file = "mbscan.R")
source(file = "CompilationTableau.R")

df <- load.dataframe(nameDf,root)
  
realiteTerrain <- length(unique(df[,3]))

setwd(dir=saveDirectory)

##Pour chaque dataset, algorithme, espace, on va créer un dossier pour sauvegarder les fichiers.
dir.create(file.path(saveDirectory,nameDf), showWarnings = FALSE)
setwd(dir=file.path(saveDirectory,nameDf))

for(i in 1:length(spectral))
{
  dir.create(file.path(saveDirectory,nameDf,spectral[[i]]), showWarnings = FALSE)
  dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel"), showWarnings = FALSE)
  setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel"))
    
  ##On commence avec les algorithmes multilevel
  if(verbose){message("On commence les algorithmes multilevel")}
  for(j in 1:length(algorithme))
  {
    dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel",algorithme[[j]]), showWarnings = FALSE)
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel",algorithme[[j]]))

    if(verbose){message(paste("On commence l'algorithme ",algorithme[[j]]))}
    for(k in 1:nbcriteria[j])
    {
      if(verbose){message(paste("On commence le critere ",criteria[[k]]))}

      filenameLabel <- paste("labelML_",spectral[[i]],"_",criteria[[k]],".csv",sep = "", collapse = NULL)
      filenameResult <- paste("ML_",spectral[[i]],"_",criteria[[k]],".Rds",sep = "", collapse = NULL)

      analyseCritere(df = df,
                     spectral = spectral[[i]],
                     algorithme = algorithme[[j]],
                     criteriaChosen = criteria[[k]],
                     iteration = iteration,
                     facteurClustering=facteurClustering,
                     realiteTerrain = realiteTerrain,
                     filenameLabel = filenameLabel,
                     filenameResult = filenameResult,
                     vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,
                     tolerence=tolerence,threshold=threshold,
                     nonIteratif = FALSE,verbose = verbose,
                     relevantCriteria = relevantCriteria)
    }
  }

  ##On passe ensuite au hierarchique
  if(verbose){message("On commence les algorithmes hierarchique")}

  dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique"), showWarnings = FALSE)
  setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique"))


  for(j in 1:length(algorithmeHierarchique))
  {
    dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique",algorithmeHierarchique[[j]]), showWarnings = FALSE)
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique",algorithmeHierarchique[[j]]))

    filenameLabel <- paste("labelHierarchical_",spectral[[i]],"_",algorithmeHierarchique[[j]],".csv",sep = "", collapse = NULL)
    filenameResult <- paste("Hierarchical_",spectral[[i]],"_",algorithmeHierarchique[[j]],".Rds",sep = "", collapse = NULL)

    analyseCritere(df = df,
                   spectral = spectral[[i]],
                   algorithme = algorithmeHierarchique[[j]],
                   iteration = iteration,
                   realiteTerrain = realiteTerrain,
                   filenameLabel = filenameLabel,
                   filenameResult = filenameResult,
                   method = method,
                   vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,
                   tolerence=tolerence,threshold=threshold,
                   Hierarchical = TRUE,verbose = verbose,
                   relevantCriteria = relevantCriteria)
  }

  ##On termine avec le noniteratif
  if(verbose){message("On commence les algorithmes non iteratif")}

  dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif"), showWarnings = FALSE)
  setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif"))

  for(j in 1:length(algorithmeNonIteratif))
  {
    dir.create(file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif",algorithmeNonIteratif[[j]]), showWarnings = FALSE)
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif",algorithmeNonIteratif[[j]]))

    filenameLabel <- paste("labelNonIteratif_",spectral[[i]],"_",algorithmeNonIteratif[[j]],".csv",sep = "", collapse = NULL)
    filenameResult <- paste("NonIteratif_",spectral[[i]],"_",algorithmeNonIteratif[[j]],".Rds",sep = "", collapse = NULL)

    analyseCritere(df = df,
                   spectral = spectral[[i]],
                   algorithme = algorithmeNonIteratif[[j]],
                   iteration = iteration,
                   realiteTerrain = realiteTerrain,
                   filenameLabel = filenameLabel,
                   filenameResult = filenameResult,
                   vois=vois, flagDiagZero = flagDiagZero,Kmax=Kmax,
                   tolerence=tolerence,threshold=threshold,
                   nonIteratif = TRUE,verbose = verbose,
                   relevantCriteria = relevantCriteria)
  }
  
  message("Analyse terminée", spectral[[i]])
}

CompilationTableau(saveDirectory=saveDirectory,nameDf=nameDf,spectral=spectral,algorithme=algorithme,nbcriteria=nbcriteria,algorithmeHierarchique=algorithmeHierarchique,algorithmeNonIteratif=algorithmeNonIteratif)
