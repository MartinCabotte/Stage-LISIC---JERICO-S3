spanningTree <- function(kClusters,membership){
  currentPointDepart <- as.integer(runif(1,1,max=(kClusters+1)))
  
  spanningtree <- data.frame("Point"=currentPointDepart,"ArcDepart"=0,"ArcArrive"=0,"Cost"=0)
  
  for(i in 2:kClusters)
  {
    membership[currentPointDepart,] <- 0
    max <- 0
    currentPointDepart <- NA
    currentPointArrive <- NA
    for(j in spanningtree$Point)
    {
      if(max <= max(membership[,j])){
        current <- j
        currentPointArrive <- which.max(membership[,j])
        max <- max(membership[,j])
      }
    }
    
    pts <- c(spanningtree$Point,currentPointArrive)
    dep <- c(spanningtree$ArcDepart,current)
    arr <- c(spanningtree$ArcArrive,currentPointArrive)
    cos <- c(spanningtree$Cost,max)
    spanningtree <- data.frame("Point"=pts,"ArcDepart"=dep,"ArcArrive"=arr,"Cost"=cos)
    currentPointDepart <- currentPointArrive
  }
  
  spanningtree <- spanningtree[-1,]
  spanningtree <- spanningtree[,-1]
  
  spanningtree
}