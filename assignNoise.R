#' @title Assigne le bruit à un cluster
#' @author Martin CABOTTE
#' @description 
#' @param df the dataframe.
#' @param noise the noise dataframe.
#' @param centers centers of clusters.
#' @return returns the dataframe with noise points in it.

assignNoise <- function(df,noise,centers,computeMembership=TRUE){
  
  membership <- matrix(NA,ncol=length(centers[,1]),nrow=length(noise[,1]))
  
  #On a noise : les points de bruit avec leur coordonnées
  for(i in 1:length(noise[,1]))
  {
    #On calcule une distance entre noise et tous les points de dataset
    dist <- dista(noise[,c(-1,-2)],df[,c(-1,-2)])
    
    #On prend la plus petite distance
    mindistPts <- which(dist == min(dist),arr.ind=TRUE)
    
    #On prend qu'une seule valeur si plusieurs en ressortent
    mindistPts <- as.matrix(mindistPts)
    mindistPts <- mindistPts[1,]
    
    #On affecte la couleur du point et calculons l'appartenance du point
    noise[mindistPts[1],2] <- df[mindistPts[2],2] ##On affecte le bon label
    df <- rbind(df,noise[mindistPts[1],])
    
    if(computeMembership == TRUE){membership[i,] <- compute.point.membership(noise[mindistPts[1],c(-1,-2)],centers)}
    
    noise <- noise[-mindistPts[1],]
  }
  
  if(computeMembership == FALSE){membership <- NULL}
  returnlist <- list("df"=df,"membership"=membership)
  
  returnlist
}