#' @title Analyse un dataset complet automatiquement
#' @author Martin CABOTTE
#' @description In order to run the code, several parameters have to be set up : first, the dataframe (df) with 3 rows corresponding to x,y,labels respectively; the working directory; the number of iterations to perform
#' @return save results in the working directory (labels and lists)

library(evclust)
library(sClust)
library(e1071)
library(fpc)
library(fclust)
library(ggplot2)
library(pdfCluster)
library(mltools) 
library(FDRSeg)
library(MLmetrics)
library(caret)

setwd(dir="C:/Users/33605/Documents/Stage/Tests/")

source(file = "Fuzzy_MSC.R")
source(file = "Fuzzy_MC.R")
source(file = "FSIL.R")
source(file = "assignVote.R")
source(file = "analyseCritere.R")
source(file = "Overlap.R")

df <- read.csv("C:/Users/33605/Documents/Stage/Tests/df.aggregation_1.csv")
# df <- read.csv("C:/Users/33605/Documents/Stage/Tests/df.AmbiguousCercle.csv")
# df <- read.csv("C:/Users/33605/Documents/Stage/Tests/df.coumpoundBrut.csv")
# df <- read.csv("C:/Users/33605/Documents/Stage/Tests/df.bananas.csv")
# df <- df[,-1]

setwd(dir="C:/Users/33605/Documents/Stage/Tests/")

data<-df[,-3]
df <- df[,3]

iteration = 10

criteria = list("CardSil","Silhouette","FuzzySilhouette","mass","mass","mass")
masscrit = list("lowbel100","lowbel50","lowbel25")
algorithme = list("kmean","cmean","ecm")
spectral = list("Initial","Spectral")


realiteTerrain <- max(df)

truemasscrit <- " "

for(k in 1:2)
{
  for(j in 1:3)
  {
    nbcriteria <- 6
    if(algorithme[j] == "kmean"){nbcriteria <- 2}
      
    for(i in 1:nbcriteria)
    {
      message(paste("On commence le critere numero",i))
      filenameLabel <- paste(algorithme[j],"/label_",spectral[[k]],criteria[[i]],".csv",sep = "", collapse = NULL)
      filenameResult <- paste(algorithme[j],"/",spectral[[k]],criteria[[i]],".Rds",sep = "", collapse = NULL)
      
      if(criteria[[i]] == "mass")
      {
        truemasscrit <- masscrit[[i-3]]
        filenameLabel <- paste(algorithme[j],"/label_",criteria[[i]],truemasscrit,".csv",sep = "", collapse = NULL)
        filenameResult <- paste(algorithme[j],"/",criteria[[i]],truemasscrit,".Rds",sep = "", collapse = NULL)
      }
      if(k == 1){
        analyseCritere(data,df,spectral=FALSE,criteria[[i]],truemasscrit,iteration=10,algorithme[j],realiteTerrain,filenameLabel,filenameResult)
      }else{
        analyseCritere(data,df,spectral=TRUE,criteria[[i]],truemasscrit,iteration=10,algorithme[j],realiteTerrain,filenameLabel,filenameResult)
      }
    }
  }
}
                                          