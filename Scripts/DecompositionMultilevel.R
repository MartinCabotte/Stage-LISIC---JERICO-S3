## Script qui décompose en multilevel un clustering et qui calcule pour chaque
# niveau l'ari correspondant

library(pdfCluster)

setwd("~/Stage/Tests/AnalysisFiles")
source("load.dataframe.R")

df <- load.dataframe("Bananas","~/Stage/Tests/AnalysisFiles")
data <- df[,-3]

setwd("~/Stage/Tests/AnalysisFiles/")
labels <- read.csv("Leg1Label.csv")
label <- labels[[1]]
length(unique(label))
plot(df$Chl_OF,col=as.numeric(factor(label)),pch=as.numeric(factor(label)))

ari <- c()

for(i in 1:length(label))
{
  numberofzero <- (7 - nchar(label[i]))/2
  zeros <- ""
  if(numberofzero != 0)
  {
    for(j in 0:(numberofzero-1))
    {
      zeros <- paste(zeros,".0",sep = "")
    }
  }
  label[i] <- paste(label[i],zeros,sep="")
}

level1 <- sub("\\..*", "", label)
level <- paste(level1,sep="")
level <- as.numeric(factor(level))
ari <- adj.rand.index(truelabel,level)
plot(data,col=(as.numeric(level1)+1))

label2 <- sub("..","",label)



level2 <- sub("\\..*", "", label2)

level <- paste(level1,level2,sep="")
level <- as.numeric(factor(level))
ari <- c(ari,adj.rand.index(truelabel,level))
plot(data,col=(as.numeric(level)+1),pch=(as.numeric(level)+1))

label3 <- sub("....","",label)

level3 <- sub("\\..*", "", label3)

level <- paste(level1,level2,level3,sep="")
level <- as.numeric(factor(level))
ari <- c(ari,adj.rand.index(truelabel,level))
plot(data,col=(as.numeric(level)+1),pch=(as.numeric(level)+1))

label4 <- sub("......","",label)

level4 <- sub("\\..*", "", label4)

level <- paste(level1,level2,level3,level4,sep="")
level <- as.numeric(factor(level))
ari <- c(ari,adj.rand.index(truelabel,level))
plot(data,col=(as.numeric(level)+1),pch=(as.numeric(level)+1))

plot(data,col=as.numeric(factor(label)))



print(paste("ARI Niveau 1 :",ari[1]))
print(paste("ARI Niveau 2 :",ari[2]))
print(paste("ARI Niveau 3 :",ari[3]))
print(paste("ARI Niveau 4 :",ari[4]))

