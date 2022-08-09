#' @title Multilevel clustering - Fuzzy and evidential - Rought and spectral
#' @author Martin CABOTTE
#' @description
#' @param X the dataset.
#' @param spectral if result is in spectral embedded space or in rought feature plan.
#' @param levelMax maximum level.
#' @param choice algorithm: "kmean","cmean","ecm".
#' @param criteria split criteria used: "CardSil", "Silhouette", "FuzzySilhouette", "Mass25", "Mass50", "Mass100".
#' @return returns a clustering tree

Fuzzy_MC <- function(X,spectral, levelMax, choice="kmean", criteria="CardSil",
                silMin=0.7,nstart=10,facteurClustering=5,
                vois=7,
                alpha=1, beta = 2, delta = 10,
                flagDiagZero=FALSE,lambdaVie = FALSE,
                method = "default", Kmax = 20, tolerence = 0.99,threshold = 0.9,
                minPoint = 7,
                verbose = FALSE){
  
  #Initialization
  clusterToCut <- 1 ; level <- 1 ; stop <- FALSE
  cl <- matrix(1, nrow = nrow(X), ncol = levelMax)
  clusterToCutBeforeUpdate <- c()
  stabilite <- data.frame("Label"=c(0),"Stabilite"=c(0))
  
  
  if(criteria == "FuzzySilhouette" && choice == "kmeans")
  {
    message("Le critere Fuzzy Silhouette est identique au critere Silhouette pour du hard clustering, criteria = Silhouette")
    criteria = "Silhouette"
  }
  
  minPoint <- max(Kmax,minPoint)
  
  #Level clustering
  while(!stop){
    silLevel <- c(); newCluster <- c();
    cl[,level+1] <- cl[,level]
    if(verbose){message(level)}
    stabiliteATraiter <- c()
    
    
    #For each cluster to cut
    sapply(clusterToCut, FUN = function(x){
      indices = which(cl[,level]==x)
      Xprime <- X[indices,]
      silLevel <- c()
      
      if(!is.null(nrow(Xprime)) && nrow(Xprime) > minPoint){
        
        ##Compute space, K-value
        if(spectral == TRUE){
          spectralData <- compute.spectral(Xprime,vois,Kmax,tolerence,threshold,verbose,flagDiagZero)
          Xprime <- spectralData$eigenspace
          kClusters <- spectralData$pevK
        }else{
          if(verbose){
            print(length(Xprime[,1]))
            print(Kmax)
            print(nstart)
          }
          kClusters <- Kbrut(Xprime,Kmax=Kmax,nstart=nstart,verbose=verbose)
        }
        if(verbose){cat("K = ",kClusters,"\n")}
        
        
        ##Compute clustering and split criterion
        
        ##If an algorithm have to be added, please copy/paste cmeans or ecm elseif
        ##and change choice option as well as the algorithm used. split criteria
        ##will remain the same.
        
        ##If a split criteria is added, please add it in the different elseif just
        ##below. Then, add the criteria in the clusterToCutBeforeUpdate (in the
        ##a priori or a posteriori list according to the criteria) if it computes
        ##membership.
        ##Last thing is to add the criteria in the last section. If membership is 
        ##computed, just add the name in the first elseif as for other membership-
        ##based split criterion.
        
        if(choice == "kmeans")
        {
          groups <- kmeans(Xprime, kClusters,nstart=nstart,iter.max = 100)$cluster
        }
        else if(choice == "cmeans")
        {
          clus <- convergenceCmeans(Xprime,kClusters,nstart)
          groups <- clus$cluster
          
          ##Split criteria
          if(criteria == "FuzzySilhouette"){
            silLevel <- compute.split.criteria(Xprime,clus$cluster,membership = clus$membership,criteria = criteria, verbose = verbose)
          }
          
          ##Non-split criteria
          if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100"){
            silLevel <- compute.split.criteria(Xprime,clus$cluster,membership = clus$membership,criteria = criteria, verbose = verbose)
            
            ##Cancel split if mean(membership) < silMin
            silMean <- 0
            if(length(silLevel) != 0){silMean <- mean(silLevel)}
            if(!is.na(silMean) && silMean < silMin){groups <- NULL}
          }
        }
        else if(choice == "ecm")
        {
          clus <- ecm(Xprime, kClusters, type="full", init="kmeans", beta=beta, delta=delta,alpha=alpha,ntrials=nstart,disp=FALSE)
          groups <- clus$y.bel
          
          ##Split criteria
          if(criteria == "FuzzySilhouette"){
            silLevel <- compute.split.criteria(Xprime,clus$y.bel,membership = clus$bel.n,criteria = criteria,verbose=verbose)
          }
          
          ##Non-split criteria
          if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100"){
            silLevel <- compute.split.criteria(Xprime,clus$y.bel,membership = clus$bel.n,criteria = criteria,verbose=verbose)
            
            ##Cancel split if mean(membership) < silMin
            silMean <- 0
            if(length(silLevel) != 0){silMean <- mean(silLevel)}
            if(!is.na(silMean) && silMean < silMin){groups <- NULL}
          }
        }
        else if(choice == "mbscan")
        {
          if(length(Xprime[,1]) > (kClusters*facteurClustering))
          {
            clus <- mbscan(iteration=nstart,df=Xprime,KFinal=kClusters,facteurClustering = facteurClustering,criteria = criteria,verbose=verbose)
            groups <- clus$labels
            stabiliteATraiter <- clus$Stabilite
            
             
            
            ##Split criteria
            if(criteria == "FuzzySilhouette"){
              stop("This split criterion hasn't been implemented yet")
            }
            
            ##Non-split criteria
            if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100" || criteria == "knn"){
              if(criteria == "Mass25"){silLevel <- clus$Mass25}
              else if(criteria == "Mass50"){silLevel <- clus$Mass50}
              else if(criteria == "Mass100"){silLevel <- clus$Mass100}
              else if(criteria == "knn"){silLevel <- clus$knn}
              
              ##Cancel split if mean(membership) < silMin
              silMean <- 0
              if(length(silLevel) != 0){silMean <- mean(silLevel)}
              if(!is.na(silMean) && silMean < silMin){groups <- NULL}
              if(min(silLevel) == -2){groups <- NULL}##Ici, on n'a pas eu assez de points par cluster
                                                      ##pour calculer notre silhouette (un cluster n'a)
                                                      ##pas eu de calcul de critère
            }  
          }else{
            groups <- NULL
            silLevel <- 1
          }
        }
        
        #Changing the cluster if necessary
        if(!is.null(groups) && length(unique(groups))>1){
          if(level == 1 ){
            cl[indices,level+1] <<- paste0(groups)
            newCluster <<- c(newCluster, unique(paste0(groups)))
            
            
            ##On créé notre arbre de stabilité
            if(choice == "mbscan"){
              uniqueGroups <- unique(groups)
              
              for(z in uniqueGroups)
              {
                stabilite <<- rbind(stabilite, c(z,stabiliteATraiter[which(stabiliteATraiter[,1] == z),2]))
              }
              print(stabilite)
            }
            
          }else{
            cl[indices,level+1] <<- paste0(cl[indices,level],".",groups)
            newCluster <<- c(newCluster, unique(paste0(cl[indices,level],".",groups)))
            
            
            ##On continu notre arbre de stabilité
            if(choice == "mbscan"){
              uniqueGroups <- unique(cl[indices,level+1])
              
              for(z in uniqueGroups)
              {
                currentGroup <- substr(z, nchar(z), nchar(z))
                stabilite <<- rbind(stabilite, c(z,stabiliteATraiter[which(stabiliteATraiter[,1] == currentGroup),2]))
              }
              print(stabilite)
            }
            
          }
          if(FALSE){
            message("CLUSTERS VECTOR")
            print(cl[,level+1])
          }
          
          ##FuzzySilhouette and Mass criterion are computed thanks to clustering results, so
          ##we need to know which cluster to cut in next iteration before deleting this
          ##information
          if(criteria == "FuzzySilhouette")
          {
            clusterToCutBeforeUpdate <<- c(clusterToCutBeforeUpdate,newCluster[newCluster %in% unique(cl[,level+1])][which(silLevel<silMin)])
            newCluster <<- c()
          }
          if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100" || criteria == "knn")
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
    {
      cardinal <- compute.split.criteria(X,cluster,criteria="CardSil",distMatrix=dist(X),verbose=verbose)
      clusterToCut <- newCluster[which(newCluster %in% unique(cl[,level+1])[which(cardinal > 0)])]
    }
    else if(criteria == "FuzzySilhouette" || criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100" || criteria == "knn")
    {
      clusterToCut <- clusterToCutBeforeUpdate
    }
    else if(criteria == "Silhouette")
    {
      silLevel <- compute.split.criteria(X,cluster,criteria="Silhouette",distMatrix=dist(X),verbose=verbose)
      clusterToCut <- newCluster[which(newCluster %in% unique(cl[,level+1])[which(silLevel<silMin)])]
    }
    
    if(verbose){print(clusterToCut)}
    
    stop <- ((level+1) >= levelMax) || (length(clusterToCut)==0)
    level <- level + 1
  }
  
  stabilite <- stabilite[-1,]
  
  print(stabilite)
  
  if(choice == "mbscan" && lambdaVie == TRUE && length(stabilite$Label != 0)){
    ##On save les labels pour les modifier à la fin
    labels <- cl[,level]
    
    ##On fait les paires de clusters parent -> fils
    i <- 1
    PereFils <- matrix(NA,ncol=2,nrow=length(stabilite$Label))
    
    for(i in 1:length(stabilite$Label)){
      lengthString <- nchar(stabilite$Label[i])
      
      fils <- c()
      for(j in 1:length(stabilite$Label)){
        if(substr(stabilite$Label[j],0,lengthString) == stabilite$Label[i] && !(stabilite$Label[j] %in% PereFils[,2]) && (nchar(stabilite$Label[j]) == (lengthString + 2))){
          fils <- c(fils,stabilite$Label[j])
        }
      }
      
      if(!is.null(fils)){
        firstI <- i
        for(j in 1:length(fils))
        {
          PereFils[i,1] <- stabilite$Label[firstI]
          PereFils[i,2] <- fils[j]
          i <- i + 1
        }
        i <- i - 1
      }
    }
    
    print(PereFils)
    PereFils <- na.omit(PereFils)
    print(PereFils)
    if(!is.null(PereFils)){
      PereFils <- PereFils[nrow(PereFils):1,]
      print(PereFils)
    
      uniqueFather <- unique(PereFils[,1])
      PoidFils <- matrix(NA,nrow=length(uniqueFather),ncol=2)
      PoidFils[,1] <- uniqueFather
      
      sum <- 0
      prevFather <- PereFils[1,1]
      for(i in 1:length(PereFils[,1])){
        if(prevFather == PereFils[i,1]){
          print(sum)
          sum <- sum + as.numeric(stabilite$Stabilite[which(stabilite$Label == PereFils[i,2])])
        }else{
          PoidFils[which(PoidFils[,1] == prevFather),2] <- sum
          prevFather <- PereFils[i,1]
          sum <- as.numeric(stabilite$Stabilite[which(stabilite$Label == PereFils[i,2])])
        }
      }
      PoidFils[nrow(PoidFils),2] <- sum
      
      print(PoidFils)
      
      for(i in 1:length(PoidFils[,1])){
        if(PoidFils[i,2] <= stabilite$Stabilite[which(stabilite$Label == PoidFils[i,1])]){
          labels[PereFils[which(PereFils[,1] == PoidFils[i,1]),2]] <- PoidFils[i,1]
        }
      }
    }
    out <- labels
  }else{
    out <- cl[,1:level]
  }
  
  
  out
}

