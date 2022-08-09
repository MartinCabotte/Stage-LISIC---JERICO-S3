removeNoiseDistance <- function(df,clustering,gap=0,verbose=FALSE){
  
  distToCenter <- vector(length=length(df[,1]))
  
  data <- df[,c(-1,-2)]
  
  
  for(j in 1:length(data[,1]))
  {
    distToCenter[j] <- sqrt(sum((data[j,]-clustering$centers[clustering$cluster[j],])^2))
  }
  
  finalDist <- distToCenter
  distToCenter <- sort(distToCenter)
  
  R50 <- distToCenter[(length(data[,1])/2):length(distToCenter)]
  
  distanceMaxBetweenPointAndCenters <- gap + (mean(R50))
  
  noise <- df[which(finalDist >= distanceMaxBetweenPointAndCenters),]
  df <- df[which(finalDist < distanceMaxBetweenPointAndCenters),]
  
  returnlist <- list("df"=df,"noise"=noise)
  
  returnlist
}