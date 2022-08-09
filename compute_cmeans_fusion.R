compute_cmeans_fusion <- function(df,KFinal,gap=0.3,
                                  convergence = 20,silcut = 0.3,facteurClustering = 5,
                                  verbose=FALSE,computeMembership=TRUE){
  
  ##On assigne une classe bruit à tous nos points dès le départ
  ##Le V4 sert à connaitre l'ordre des points initialement
  ##On va faire le traitement normalement et à la fin, pour éviter de redonner une dataframe
  ##changé, on va le réorganiser par V4 et sortir ainsi le même ordre que le dataframe initial
  df <- data.frame("index"=c(1:length(df[,1])),"label"=rep(0,length(df[,1])),df)
  
  ##K surclustering
  kClusters <- KFinal * facteurClustering
  
  #####Clustering qui sert à retirer les points initiaux ambigues#####
  clustering <- convergenceCmeans(df[,c(-1,-2)],kClusters,convergence)
  if(verbose){plot(df[,c(-1,-2)],col=clustering$cluster)}
  
  
  #####On va retirer les appartenances ambigues pour pouvoir continuer par la suite avec des clusters#####
  #####sont plus représentatifs#####
  # listdf <- removeNoise(df,clustering$membership,silcut) #Faire le V4 dedans
  listdf <- removeNoiseDistance(df,clustering,gap)
  noise <- listdf$noise
  df <- listdf$df
  
  
  #####Clustering qui va servir de base pour la suite de la fusion#####
  clustering <- convergenceCmeans(df[,c(-1,-2)],kClusters,convergence)
  if(verbose){plot(df[,c(-1,-2)],col=clustering$cluster,pch=as.character(clustering$cluster))}
  
  #####Calcul de la matrice d'appartenance cluster par cluster normalisée#####
  membership <- clusterToClusterMembership(kClusters,clustering)
  
  #####Déterminer le spanning tree#####
  #Algorithme de prims
  savemembership <- membership
  spanningtree <- spanningTree(kClusters,membership)
  if(verbose){print(spanningtree)}
  
  
  #####Créer les branches à partir du spanning tree#####
  ##En premier, on fait les racines de l'arbre
  branch <- compute.root.tree(KFinal,spanningtree)
  cost <- branch
  branch <- branch$branch
  ##Ensuite, on créé les branches
  affectation <- unfold.tree(branch,KFinal,KFinal*facteurClustering,spanningtree)
  ##On affecte les nouveaux super clusters
  newcluster <- assign.tree(clustering,affectation,KFinal)
  
  if(verbose){print(affectation)}
  if(verbose){print(cost)}
  if(verbose){plot(df[,c(-1,-2)],col=newcluster)}
  
  
  
  ##Enfin, l'affectation des points d'ambiguité avec une mesure de distance minimale
  ##On itère sur tous les points retirés et on prend celui qui est le plus proche d'un points deja labellisé
  ##On l'affecte avec son label et on continue jusqu'à ce qu'il n'y ait plus rien
  
  df$label <- newcluster
  
  ##On récupère le jeu de données du knn pour comparaison ultérieure
  testnoise <- noise
  assignationknn <- knn3Train(train= df[,c(-1,-2)],test=testnoise[,c(-1,-2)],factor(df[,2]),k=3)
  testnoise[,2] <- assignationknn
  knndf <- rbind(df,testnoise)
  

  
  
  ##Si on n'a pas de bruit, on n'a pas à le réassigner, donc on ne le fait pas
  if(length(noise[,1]) != 0)
  {
    list <- assignNoise(df,noise,clustering$centers,computeMembership=computeMembership)
    df <- list$df
    membership <- list$membership
    
    clustering$membership <- rbind(clustering$membership,membership)
  }
  
  
  
  
  
  if(verbose){
    max <- vector(length = length(df[,1]))
    for(i in 1:length(df[,1]))
    {
      max[i] <- max(clustering$membership[i,]) 
    }
    plot(df[,c(-1,-2)],col=df[,2],pch=16,cex=(max-0.3)) 
  }
  
  
  #####Construction de l'appartenance d'un points à son super-cluster#####
  superMembershipFast <- matrix(NA, ncol = KFinal, nrow = length(clustering$membership[,1]))
  for(i in 1:KFinal)
  {
    colonnes <- sort(unique(affectation[,i]))
    colonnes <- colonnes[-1]
    superMembershipFast[,i] <- rowSums(clustering$membership[,colonnes,drop=FALSE])
  }
  
  
  if(verbose){
    self <- vector(length = length(df[,1]))
    for(i in 1:length(df[,1])){
      self[i] <- superMembershipFast[i,df[i,2]]
    }
    
    plot(df[,c(-1,-2)],col=df[,2],pch=as.character(self*10),cex=self)
  }
  
  
  ##Compute Mass criteria (25,50,100)
  Mass25 <- c()
  Mass50 <- c()
  Mass100 <- c()
  knn <- c()
  Stabilite <- c()
  
  for(i in 1:KFinal)
  {
    sortedMass <- sort(superMembershipFast[which(newcluster==i),i])
    sortedMassLow <- sortedMass[1:length(sortedMass)/4]
    if(length(sortedMassLow) < 4){
      Mass25[i] <- -2
    }else{Mass25[i] <- sum(sortedMassLow)/length(sortedMassLow)}
  }
  
  for(i in 1:KFinal)
  {
    sortedMass <- sort(superMembershipFast[which(newcluster==i),i])
    sortedMassLow <- sortedMass[1:length(sortedMass)/2]
    if(length(sortedMassLow) < 2){
      Mass50[i] <- -2
    }else{Mass50[i] <- sum(sortedMassLow)/length(sortedMassLow)}
    
  }
  
  for(i in 1:KFinal)
  {
    sortedMass <- sort(superMembershipFast[which(newcluster==i),i])
    
    Mass100[i] <- sum(sortedMass)/length(sortedMass)
  }
  
  df <- df[order(df[,1]),]
  knndf <- knndf[order(knndf[,1]),]
  
  for(i in 1:KFinal)
  {
    sumWronglyAssigned <- sum(df[which(newcluster==i),2] != knndf[which(newcluster==i),2])
    
    knn[i] <- 1 - (sumWronglyAssigned/length(df[which(newcluster==i),1]))
  }
  
  Stabilite <- data.frame()
  for(i in 1:KFinal)
  {
    clustersWoCut <- superMembershipFast[which(newcluster==i),i]-cost[i,2]
    Stabilite <- rbind(Stabilite,c(i,sum(clustersWoCut)))
  }
  
  

  
  
  
  ##inversion de la première avec la dernière ligne pour donner un dataframe identique à ce qui est traité
  returndf <- df[,c(3:ncol(df),2)]
  
  returnList <- list("labels"=df[,2],"Silhouette"=summary(silhouette(df[,2],dist(df[,c(-1,-2)])))$avg.width,"df"=returndf,"membership"=superMembershipFast,"Mass25"=Mass25,"Mass50"=Mass50,"Mass100"=Mass100,"knn"=knn,"Stabilite"=Stabilite)
  
  returnList
}