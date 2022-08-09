#' @title Analyse du critère facteur de mbscan et réalisation d'un .csv pour compiler les résultats
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

verbose <- TRUE


##Analysis in spectral or rought feature plan or both ?
isSpectral = TRUE
isRought = TRUE

##Algorithms to compute
AllAlgorithms = FALSE
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
AllSplitCriteria = FALSE
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
  algorithme = list("kmeans","cmeans","ecm","mbscan")
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

Mass <- list("Mass25","Mass100")

for(i in 1:length(Mass))
{
  # ##Facteur 1
  # analyseCritere(df,criteriaChosen = Mass[[i]],
  #                iteration=10,algorithme="mbscan",
  #                realiteTerrain = realiteTerrain,
  #                filenameLabel = paste("Facteur1",nameDf,Mass[[i]],"Label.csv",sep=""),
  #                filenameResults = paste("Facteur1",nameDf,Mass[[i]],".Rds",sep=""),
  #                facteurClustering = 1)
  # 
  # ##Facteur 2
  # analyseCritere(df,criteriaChosen = Mass[[i]],
  #                iteration=10,algorithme="mbscan",
  #                realiteTerrain = realiteTerrain,
  #                filenameLabel = paste("Facteur2",nameDf,Mass[[i]],"Label.csv",sep=""),
  #                filenameResults = paste("Facteur2",nameDf,Mass[[i]],".Rds",sep=""),
  #                facteurClustering = 2)
  # 
  # ##Facteur 5
  # analyseCritere(df,criteriaChosen = Mass[[i]],
  #                iteration=10,algorithme="mbscan",
  #                realiteTerrain = realiteTerrain,
  #                filenameLabel = paste("Facteur5",nameDf,Mass[[i]],"Label.csv",sep=""),
  #                filenameResults = paste("Facteur5",nameDf,Mass[[i]],".Rds",sep=""),
  #                facteurClustering = 5)
  # 
  # ##Facteur 10
  # analyseCritere(df,criteriaChosen = Mass[[i]],
  #                iteration=10,algorithme="mbscan",
  #                realiteTerrain = realiteTerrain,
  #                filenameLabel = paste("Facteur10",nameDf,Mass[[i]],"Label.csv",sep=""),
  #                filenameResults = paste("Facteur10",nameDf,Mass[[i]],".Rds",sep=""),
  #                facteurClustering = 10)
  
  ##Facteur 20
  analyseCritere(df,criteriaChosen = Mass[[i]],
                 iteration=10,algorithme="mbscan",
                 realiteTerrain = realiteTerrain,
                 filenameLabel = paste("Facteur20",nameDf,Mass[[i]],"Label.csv",sep=""),
                 filenameResults = paste("Facteur20",nameDf,Mass[[i]],".Rds",sep=""),
                 facteurClustering = 20)
  
  ##Facteur 50
  analyseCritere(df,criteriaChosen = Mass[[i]],
                 iteration=10,algorithme="mbscan",
                 realiteTerrain = realiteTerrain,
                 filenameLabel = paste("Facteur50",nameDf,Mass[[i]],"Label.csv",sep=""),
                 filenameResults = paste("Facteur50",nameDf,Mass[[i]],".Rds",sep=""),
                 facteurClustering = 50)
}




