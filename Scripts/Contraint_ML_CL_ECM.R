##Script qui realise un ecm contraint avec un certain nombre de contraites
# de must ling et un autre nombre de contraintes de cannot link

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
library(ape)

setwd(dir="C:/Users/33605/Documents/Stage/Tests/")


df <- read.csv("C:/Users/33605/Documents/Stage/Tests/df.aggregation_1.csv")

data <- df[,-3]
vois <- 3
verbose <- TRUE
Kmax <- 20
tolerence <- 0.999
threshold <- 0.9
MLperCluster <- 5
CLperCluster <- 10

# plot(data,pch=as.character(df[,3]))


# W <- NULL
# W <- compute.similarity.ZP(data, vois=vois)
# W <- checking.gram.similarityMatrix(W, flagDiagZero = TRUE, verbose = verbose)
# W <- compute.laplacian.NJW(W, verbose = verbose)
# eigenValues <- W$eigen$values
# kClusters <- compute.kclust(eigenValues,method = "PEV", Kmax = Kmax, tolerence =  tolerence,threshold = threshold,verbose = verbose)
# if(kClusters<=1){kClusters=2}
# dataSpec=W$eigen$vectors[,1:kClusters]
# dataSpec <- dataSpec/apply(dataSpec, MARGIN = 1, FUN = function(x) norm(matrix(x),"f"))
# 
# clusteringSpectral <- kmeans(dataSpec,kClusters,nstart=1)
# 
# plot(data,col=clusteringSpectral$cluster,pch=clusteringSpectral$cluster)
# plot(dataSpec,col=clusteringSpectral$cluster,xlim=c(-1,1),ylim=c(-1,1))
# plot(dataSpec[,2:3],col=clusteringSpectral$cluster,xlim=c(-1,1),ylim=c(-1,1))
# 
# stop("ok")

numc1 <- 5
numc2 <- 7
c1 <- length(which(df[,3] == numc1))
c2 <- length(which(df[,3] == numc2))

min <- 0
max <- 0

if(numc1 < numc2){
  max <- numc1
  min <- numc2
}else{
  max <- numc2
  min <- numc1
}

CL1 <- matrix(NA,ncol=2,nrow=min(c1,c2))
CL2 <- matrix(NA,ncol=2,nrow=(c1*c2))

for(i in 1:min(c1,c2))
{
  rand <- as.integer(runif(1,1,(max(c1,c2)+1)))
  CL1[i,] <- c(which(df[,3] == min)[i],which(df[,3] == max)[rand])
}

for(i in 1:min(c1,c2))
{
  for(j in 1:max(c1,c2))
  {
    rand1 <- as.integer(runif(1,1,(max(c1,c2)+1)))
    rand2 <- as.integer(runif(1,1,(min(c1,c2)+1)))

    CL2[(((i-1)*min(c1,c2))+j),] <- c(which(df[,3] == min)[rand2],which(df[,3] == max)[rand1])
  }
}


kClusters <- 5
clusteringContraint1 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1,xi=0.99)
plot(clusteringContraint1,X=data)
plot(data,col=clusteringContraint1$y.pl)
# clusteringContraint2 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint2,X=data)
# clusteringContraint3 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint3,X=data)
# clusteringContraint4 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint4,X=data)
# clusteringContraint5 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint5,X=data)
# clusteringContraint6 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint6,X=data)
# clusteringContraint7 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL1)
# plot(clusteringContraint7,X=data)
# clusteringContraint2 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL2)

# kClusters <- 2
# clusteringContraint1 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL2)
# plot(clusteringContraint1,X=data)
# clusteringContraint2 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL2)
# plot(clusteringContraint2,X=data)
# clusteringContraint3 <- cecm(data,kClusters,ntrials=1,ML=matrix(1:2,ncol = 2,nrow = 1),CL=CL2)
# plot(clusteringContraint3,X=data)

# 
# ML <- matrix(NA, ncol=2, nrow = kClusters*MLperCluster)
# CL <- matrix(NA, ncol=2, nrow = kClusters*CLperCluster + length(which(df[,3] == 5)))
# 
# for(i in 1:kClusters)
# {
#   cluster <- which(clusteringSpectral$cluster == i)
#   notincluster <- which(clusteringSpectral$cluster != i)
#   
#   ##ML
#   for(j in 1:MLperCluster)
#   {
#     points <- as.integer(runif(2,1,length(cluster)))
#     ML[((i-1)*MLperCluster)+j,] <- cluster[points]
#   }
#   
#   ##CL
#   for(j in 1:CLperCluster)
#   {
#     point1 <- as.integer(runif(1,1,length(cluster)))
#     point2 <- as.integer(runif(1,1,length(notincluster)))
#     
#     points <- c(cluster[point1],notincluster[point2])
#     CL[((i-1)*CLperCluster)+j,] <- points
#   }
# }
# 
# for(i in 1:length(which(df[,3] == 5)))
# {
#   cluster <- which(df[,3] == 5)
#   cluster2 <- which(df[,3] == 1)
#   point1 <- as.integer(runif(1,1,length(cluster2)))
#   point2 <- as.integer(runif(1,1,length(cluster)))
#   points <- c(which(df[,point1]),which(df[,point3]))
#   
#   CL[kClusters*CLperCluster+i,]
# }
# 
# clusteringContraint <- cecm(data,kClusters,ntrials=5,ML=ML,CL=CL)
# 
# 
# plot(data,col=clusteringContraint$y.pl)
