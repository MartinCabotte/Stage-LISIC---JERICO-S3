unfold.tree <- function(branch,nbBranch,kClusters,spanningtree){
  KFinal <- nbBranch
  affectation <- matrix(NA,ncol = KFinal,nrow = kClusters)
  
  ##On initialise nos noeuds traités avec les racines
  alreadyTreated <- branch
  
  ##On va parcourir toutes les racines
  for(i in 1:KFinal)
  {
    ##On construit la première branche
    listBranch <- buildBranch(spanningtree,branch[i],alreadyTreated)
    
    ##On affecte les returns de la construction de branche
    affectation[,i] <- listBranch$Branch
    alreadyTreated <- listBranch$Treated
  }
  
  affectation
}