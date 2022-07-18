#' @title Multilevel spectral clustering - Fuzzy and evidential
#' @author Martin CABOTTE
#' @description 
#' @param X the dataset.
#' @param levelMax maximum level.
#' @param choice algorithm: "kmean","cmean","ecm".
#' @param criteria split criteria used: "CardSil", "Silhouette", "FuzzySilhouette", "mass".
#' @param masscrit "lowbel25", "lowbel50", "lowbel100": percentage of ambiguous points computed in mass split criterion.
#' @return returns a clustering tree

Fuzzy_MSC <- function(X, levelMax, choice="kmean", criteria="CardSil",masscrit="bel",
                silMin=0.7,nstart=10,
                vois=7,
                alpha=1, beta = 2, delta = 10,
                flagDiagZero=FALSE,
                method = "default", Kmax = 20, tolerence = 0.999,threshold = 0.9,
                minPoint = 7,
                verbose = FALSE){
  
  #Initialization
  clusterToCut <- 1 ; level <- 1 ; stop <- FALSE
  cl <- matrix(1, nrow = nrow(X), ncol = levelMax)
  Winit <- compute.similarity.ZP(X, vois=vois)
  
  clusterToCutBeforeUpdate <- c()
  
  if(criteria == "FuzzySilhouette" && (choice == "kmean" || choice == "pam"))
  {
    message("Le critere Fuzzy Silhouette est identique au critere Silhouette pour du hard clustering, criteria = Silhouette")
    criteria = "Silhouette"
  }
  
  #Level clustering
  while(!stop){
    silLevel <- c() ;newSil <- c() ; newCluster <- c() ; membership <- c() ; dataFS <- c()
    cl[,level+1] <- cl[,level]
    if(TRUE){message(level)}
    
    # clusterToCutBeforeUpdate <<- c()
    
    #For each cluster to cut
    sapply(clusterToCut, FUN = function(x){
      indices = which(cl[,level]==x)
      Xprime <- X[indices,]
      LP <- 0
      silLevel <- c()
      
      if(nrow(Xprime) > minPoint){
        W <- NULL
        W <- compute.similarity.ZP(Xprime, vois=vois)
        
        
          W <- checking.gram.similarityMatrix(W, flagDiagZero = TRUE, 
                                              verbose = verbose)
        
        W <- compute.laplacian.NJW(W, verbose = verbose)
        
        eigenValues <- W$eigen$values
        if(TRUE){print(eigenValues[1:10])}
        
        kClusters <- compute.kclust(eigenValues,method = "PEV", Kmax = Kmax, 
                                    tolerence =  tolerence,threshold = threshold,
                                    verbose = verbose)
        if(verbose){cat("K = ",kClusters,"\n")}
        
        if(kClusters<=1){kClusters=2}
        
        dataSpec=W$eigen$vectors[,1:kClusters]
        dataSpec <- dataSpec/apply(dataSpec, MARGIN = 1, FUN = function(x) norm(matrix(x),"f"))
        
        if(choice == "kmean")
        {
          groups <- kmeans(dataSpec, kClusters,nstart=nstart,iter.max = 100)$cluster
        }
        else if(choice == "cmean")
        {
          #Lower withinerror

          clust <- cmeans(dataSpec, kClusters)
          within <- clust$withinerror
          if(verbose){message(within)}
          groups <- clust$cluster
          clus <- clust
          for(i in 2:nstart)
          {
            clust <- cmeans(dataSpec, kClusters)
            if(within > clust$withinerror)
            {
              within <- clust$withinerror
              if(verbose){message(within)}
              groups <- clust$cluster
              clus <- clust
            }
          }

          if(criteria == "FuzzySilhouette")
          {
            cluster <- clus$cluster
            uniqueCluster <- unique(cluster)
            
            silLevel <- sapply(uniqueCluster, FUN = function(x){
              FSIL(dataSpec,cluster,x,clus$membership)
            })
            
            if(verbose){print(silLevel)}
          }
          if(criteria == "mass")
          {
            
            cluster <- groups
            uniqueCluster <- unique(cluster)
            
            if(masscrit=="lowbel100")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedmass <- sort(clus$membership[clus$cluster == x,x])
                sortedmasslow <- sortedmass[1:length(sortedmass)]
                
                sum(sortedmasslow)/length(sortedmasslow)
              })      
            }
            if(masscrit=="lowbel50")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedmass <- sort(clus$membership[clus$cluster == x,x])
                sortedmasslow <- sortedmass[1:length(sortedmass)/2]
                
                sum(sortedmasslow)/length(sortedmasslow)
              })      
            }
            if(masscrit=="lowbel25")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedmass <- sort(clus$membership[clus$cluster == x,x])
                sortedmasslow <- sortedmass[1:length(sortedmass)/4]
                
                sum(sortedmasslow)/length(sortedmasslow)
              })      
            }
            silMean <- 0
            
            if(length(silLevel) != 0){silMean <- mean(silLevel)}
            
            if(TRUE){print(silMean)}
            if(TRUE){print(silMin)}
            
            if(TRUE){print(silLevel)}
            
            if(!is.na(silMean) && silMean < silMin){groups <- NULL}
          }
        }
        else if(choice == "ecm")
        {
          clust <- ecm(dataSpec, kClusters, type="full", init="rand", beta=beta, delta=delta,alpha=alpha,ntrials=nstart,disp=FALSE)
          groups <- clust$y.bel
          #
          # plot(clust,X=X)
          
          if(criteria == "EvidentialSilhouette")
          {
            cluster <- groups
            print(cluster)
            uniqueCluster <- unique(cluster)
            
            silLevel <- sapply(uniqueCluster, FUN = function(x){
              ((length(clust$lower.approx[[x]])/length(clust$upper.approx[[x]]))^SIL.F(dataSpec[cluster == x,], clust$mass.n[cluster == x,]))
            })
            
            # plot(clust,X=dataSpec,mfrow=c(2,2))
            plot(clust,X=Xprime,mfrow=c(2,2))
            
            if(TRUE){print(silLevel)}
            if(TRUE){print(silMin)}
            
          }
          if(criteria == "FuzzySilhouette")
          {
            cluster <- groups
            uniqueCluster <- unique(cluster)
            
            message("les groupes sont:")
            message(unique(groups))
            
            message(length(clust$bel.n))
            
            silLevel <- sapply(uniqueCluster, FUN = function(x){
              FSIL(dataSpec,cluster,x,clust$bel.n)
            })
            
            # plot(clust,X=dataSpec,mfrow=c(2,2))
            # plot(clust,X=Xprime,mfrow=c(2,2))
            
            if(TRUE){print(silLevel)}
            if(TRUE){print(silMin)}
            
          }
          
          if(criteria == "mass")
          {
            
            cluster <- groups
            uniqueCluster <- unique(cluster)
            
            if(masscrit=="bel")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sum(clust$bel.n[clust$lower.approx[[x]],x])/length(clust$lower.approx[[x]])
              })
            }
            if(masscrit == "lowbel100")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedbel <- sort(clust$bel.n[clust$lower.approx[[x]],x])
                sortedbellow <- sortedbel[1:length(sortedbel)]
                
                sum(sortedbellow)/length(sortedbellow)
              })
            }
            if(masscrit == "lowbel50")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedbel <- sort(clust$bel.n[clust$lower.approx[[x]],x])
                sortedbellow <- sortedbel[1:length(sortedbel)/2]
                
                sum(sortedbellow)/length(sortedbellow)
              })
            }
            if(masscrit == "lowbel25")
            {
              silLevel <- sapply(uniqueCluster, FUN = function(x){
                sortedbel <- sort(clust$bel.n[clust$lower.approx[[x]],x])
                sortedbellow <- sortedbel[1:length(sortedbel)/4]
                
                sum(sortedbellow)/length(sortedbellow)
              })
            }
            
            silMean <- 0
            
            if(length(silLevel) != 0){silMean <- mean(silLevel)}
            
            if(TRUE){print(silMean)}
            if(TRUE){print(silMin)}
            
            if(TRUE){print(silLevel)}
            
            if(!is.na(silMean) && silMean < silMin){groups <- NULL}
            #else{
            
              # plot(clust,X=dataSpec,mfrow=c(2,2))
              # if(level == 1)
              # {
                
                # plot(clust,X=Xprime,mfrow=c(2,2))
                # 
                # for(i in 1:kClusters)
                # {
                #   colfunc<-colorRampPalette(c("yellow","orange","red","black"))
                #   colors <- (colfunc(500))
                #   colors <- colors[rank(clust$mass.n[,i])]
                #   plot(Xprime,col=colors,pch=16)
                # }
              # }
            #}
            
            if(TRUE){print(silLevel)}
            if(TRUE){print(silMin)}
          }
        }
        
        
        #Changing the cluster if necessary
        if(!is.null(groups) && length(unique(groups))>1){
          if(level == 1 ){
            cl[indices,level+1] <<- paste0(groups)
            newCluster <<- c(newCluster, unique(paste0(groups)))
          }else{
            cl[indices,level+1] <<- paste0(cl[indices,level],".",groups)
            newCluster <<- c(newCluster, unique(paste0(cl[indices,level],".",groups)))
          }
          if(verbose){
            message("CLUSTERS VECTOR")
            print(cl[,level+1])
          }
          
         
          if(criteria == "FuzzySilhouette" || criteria == "EvidentialSilhouette")
          {
            clusterToCutBeforeUpdate <<- c(clusterToCutBeforeUpdate,newCluster[newCluster %in% unique(cl[,level+1])][which(silLevel<silMin)])
            newCluster <<- c()
          }
          
          if(criteria == "mass")
          {
            clusterToCutBeforeUpdate <<- c(clusterToCutBeforeUpdate,newCluster[newCluster %in% unique(cl[,level+1])][which(silLevel>silMin)])
            newCluster <<- c()
          }
        }
      }
    })
    
    
    clusterToCut <- c()
    
    #Calculating the silhouette
    cluster <- as.numeric(as.factor(cl[,level+1]))
    uniqueCluster <- unique(cluster)
    
    
    
    if(criteria == "CardSil")
    {####
      cardinal <- sapply(uniqueCluster, FUN = function(x){
        sum(cluster::silhouette(cluster,as.dist(1-Winit))[,"sil_width"][which(cluster == x)] < 0)
      })
      clusterToCut <- newCluster[which(newCluster %in% unique(cl[,level+1])[which(cardinal > 0)])]
      if(verbose){print(cardinal)}
    }
    else if(criteria == "FuzzySilhouette"  || criteria == "EvidentialSilhouette"  || criteria == "mass")
    {
      clusterToCut <- clusterToCutBeforeUpdate
    }
    else if(criteria == "Silhouette")
    {####
      silLevel <- sapply(uniqueCluster, FUN = function(x){
        mean(cluster::silhouette(cluster,as.dist(1-Winit))[,"sil_width"][which(cluster == x)])
      })
      clusterToCut <- newCluster[which(newCluster %in% unique(cl[,level+1])[which(silLevel<silMin)])]
      if(TRUE){print(silLevel)}
    }
    
    if(verbose){print(clusterToCut)}
    
    stop <- ((level+1) >= levelMax) || (length(clusterToCut)==0)
    level <- level + 1
    
    # print(
    #   ggplot(data = X)+
    #   geom_point(mapping = aes(x = data[,1], y = data[,2], color = factor(cl[,level]), shape = factor(cl[,level])))+
    #   scale_shape_manual(values=1:nlevels(factor(cl[,level])))+
    #   ggtitle(level-1)+
    #   labs(y="Y",x="X",color="Legend",shape="Legend")
    # )
    
  }
  out <- cl[,1:level]
}

