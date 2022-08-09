removeNoise <- function(df,membershipMatrix,silcut){
 
  lengthDf <- length(df[,1])
  sortedMembership <- matrix(NA, ncol=2, nrow = lengthDf) 
  
  for(i in 1:lengthDf)
  {
    sortedMembership[i,2] <- max(membershipMatrix[i,])
    sortedMembership[i,1] <- i
  }
  
  sortedMembership <- sortedMembership[order(sortedMembership[,2],decreasing=FALSE),]
  indices <- sortedMembership[,1]
  cut <- 1
  stop <- FALSE
  cpt <- 0
  while(!stop){
    cpt <- cpt +1
    if(sortedMembership[cpt,2] >= silcut){
      cut <- cpt
      stop <- TRUE
    }
  }
  
  remainingIndices <- indices[cut:length(indices)]
  noise <- indices[1:(cut-1)]
  
  data <- data.frame(df,"indice"=1:lengthDf)
  
  dataset <- data.frame("V1"=df$V1[data$indice %in% remainingIndices],"V2"=df$V2[data$indice %in% remainingIndices],"V3"=df$V3[data$indice %in% remainingIndices])
  noisedata <- data.frame("V1"=df$V1[data$indice %in% noise],"V2"=df$V2[data$indice %in% noise],"V3"=df$V3[data$indice %in% noise])
  
  returnlist <- list("df"=dataset,"noise"=noisedata)
  
  returnlist
}