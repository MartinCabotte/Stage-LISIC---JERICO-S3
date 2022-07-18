##Script de creation du dataset Cercle

PI <- pi
R <- 0.3
separation <- 0.45
Rmin <- 1.5
largeurTrait <- 1.5
numberpoints1 <- 100 #Cercle interne
numberpoints2 <- 1000 #Cercle externe
numberpoints3 <- 0 #Ligne qui relie les 2
numberpoints4 <- 0 #Bruit


randomx1 <- runif(numberpoints1, 0, 1)
randomy1 <- runif(numberpoints1, 0, 1)
randomx2 <- runif(numberpoints1, 0, 1)
randomy2 <- runif(numberpoints1, 0, 1)
randomx3 <- runif(numberpoints1, 0, 1)
randomy3 <- runif(numberpoints1, 0, 1)
randomx4 <- runif(numberpoints1, 0, 1)
randomy4 <- runif(numberpoints1, 0, 1)

r1 = R * sqrt(randomx1)
theta1 = randomy1 * 2 * PI
r2 = R * sqrt(randomx2)
theta2 = randomy2 * 2 * PI
r3 = R * sqrt(randomx3)
theta3 = randomy3 * 2 * PI
r4 = R * sqrt(randomx4)
theta4 = randomy4 * 2 * PI

x11 = r1 * cos(theta1) + separation
y11 = r1 * sin(theta1)

x12 = r2 * cos(theta2)
y12 = r2 * sin(theta2) + separation

x13 = r3 * cos(theta3) - separation
y13 = r3 * sin(theta3)

x14 = r4 * cos(theta4)
y14 = r4 * sin(theta4) - separation

randomx2 <- runif(numberpoints2, 0, 1)
randomy2 <- runif(numberpoints2, 0, 1)

r2 = R * sqrt(randomx2) + Rmin
theta2 = randomy2 * 2 * PI

x2 = r2 * cos(theta2)
y2 = r2 * sin(theta2)

randomx4 <- runif(numberpoints4, -R-Rmin, R+Rmin)
randomy4 <- runif(numberpoints4, -R-Rmin, R+Rmin)

randomx3 <- runif(numberpoints3, -(Rmin+R/2), Rmin+R/2)
randomy3 <- runif(numberpoints3, -largeurTrait/2, largeurTrait/2)

randomy5 <- runif(numberpoints3, -(Rmin+R/2)+largeurTrait/2, (Rmin+R/2) -largeurTrait/2)
randomx5 <- runif(numberpoints3, -largeurTrait/2, largeurTrait/2)
for(i in randomx5)
{
  if(i > 0){i <- i+largeurTrait/2}
  else{i <- i+largeurTrait/2}
}

a <- matrix(data = 2,ncol=length(x11),nrow = 1)
b <- matrix(data = 3,ncol=length(x12),nrow = 1)
c <- matrix(data = 4,ncol=length(x13),nrow = 1)
d <- matrix(data = 5,ncol=length(x14),nrow = 1)
e <- matrix(data = 6,ncol=length(x2),nrow = 1)

f <- matrix(data = 1,ncol=length(c(randomx3,randomx4,randomx5)),nrow = 1)

data <- data.frame("V1"=c(x11,x12,x13,x14,x2,randomx3,randomx4,randomx5),"V2"=c(y11,y12,y13,y14,y2,randomy3,randomy4,randomy5),"V3"=c(a,b,c,d,e,f))

plot(data[,-3])

rm(list=setdiff(ls(), "data"))
# 
# write.csv(data,"df.AmbiguousCercle.csv")
# 
vois <- 70
threshold <- 0.9
tolerence <- 1.0
Xprime <- df[,-3]
verbose <- FALSE
Kmax <- 20
nstart <- 20

W <- NULL
W <- compute.similarity.ZP(Xprime, vois=vois)
W <- checking.gram.similarityMatrix(W, flagDiagZero = FALSE,
                                    verbose = verbose)
W <- compute.laplacian.NJW(W, verbose = verbose)

eigenValues <- W$eigen$values
if(TRUE){print(eigenValues[1:10])}

kClusters <- compute.kclust(eigenValues,method = "PEV", Kmax = Kmax,
                            tolerence =  tolerence,threshold = threshold,
                            verbose = verbose)
if(kClusters<=1){kClusters=2}

dataSpec=W$eigen$vectors[,1:kClusters]
groups <- kmeans(dataSpec, kClusters,nstart=nstart,iter.max = 100)$cluster
plot(df[,-3],col=groups)


data1 <- data[which(groups==1),]
data2 <- data[which(groups==2),]
plot(data2)

W <- compute.similarity.ZP(data2, vois=vois)
W <- checking.gram.similarityMatrix(W, flagDiagZero = FALSE,
                                    verbose = verbose)
W <- compute.laplacian.NJW(W, verbose = verbose)

eigenValues <- W$eigen$values
if(TRUE){print(eigenValues[1:10])}

kClusters <- compute.kclust(eigenValues,method = "PEV", Kmax = Kmax,
                            tolerence =  tolerence,threshold = threshold,
                            verbose = verbose)
if(kClusters<=1){kClusters=2}

dataSpec=W$eigen$vectors[,1:kClusters]
groups <- kmeans(dataSpec, kClusters,nstart=nstart,iter.max = 100)$cluster
plot(data2,col=groups)

clust <- cmeans(data2,kClusters)
max <- vector(length = length(data2[,1]))
for(i in 1:length(max))
{
  max[i] <- max(clust$membership[i,])
}
plot(data2,col=clust$cluster,pch=16,cex=max*1.3)

levels <- Fuzzy_MSC(data[,-3],levelMax=4,silMin=0.8,vois=70,choice="cmean",criteria="mass",masscrit="lowbel25")


plot(data[,-3],col=as.numeric(factor(levels[,ncol(levels)])),pch=as.numeric(factor(levels[,ncol(levels)])))
plot(data[,-3],col=data[,3])
