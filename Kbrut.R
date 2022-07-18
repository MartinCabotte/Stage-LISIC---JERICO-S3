#' @title Best K according to silhouette
#' @author Martin CABOTTE
#' @description 
#' @param data the dataset.
#' @param Kmax maximum K-value.
#' @return returns the best K according to silhouette criterion

Kbrut <- function(data,Kmax=7,nstart=20,verbose=FALSE)
{
  #Initialize our Silhouette Array 
  silArray <- matrix(nrow = Kmax-1, ncol = 2)
  silArray[,1] <- c(1:(Kmax-1))
  silArray[,2] <- rep(0,Kmax-1)
  
  #Minimum Kmax is 2
  if(Kmax < 2){Kmax<-2}
  
  #ForLoop to compute silhouette criterion. The array returned is and average 
  #width of the Silhouette.
  for(i in 2:Kmax)
  {
    clus <- kmeans(data,i,nstart=nstart,iter.max = 100)
    sil <- cluster::silhouette(clus$cluster,dist(data))
    recap <- summary(sil)
    silArray[i-1,2] <- recap$avg.width
  }
  
  if(verbose == TRUE)
  {
    message("-- Determiner le Kbrut --")
    print("La valeur de silhouette")
    print(silArray[,2])
    plot(silArray[,2], type="o", col="blue", pch="o", lty=1, ylim=c(min(silArray[,2])-0.05,max(silArray[,2])+0.05))
    print("Le maximum est la valeur suivante : ")
    print(silArray[which(silArray[,2] == max(silArray[,2]))])
  }
  
  #We want to return the value of K which maximize the silhouette
  out <- silArray[which(silArray[,2] == max(silArray[,2]))]+1
}
