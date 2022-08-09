#' @title Construit les branches de l'arbre
#' @author Martin CABOTTE
#' @description 
#' @param spanningtree the dataframe returned by spanningtree function.
#' @param branchToCompute The branch to compute.
#' @param branches roots of the tree.
#' @return returns a branch of the tree.
 
buildBranch <- function(spanningtree,branchToCompute,branches,verbose=FALSE){
  cpt <- 0
  stop <- FALSE
  finalBranch <- c(branchToCompute)
  
  NotAvailablePairs <- branches
  toCompute <- c(branchToCompute)
  
  
  if(verbose==TRUE){print(spanningtree)}
  
  while(length(toCompute) != 0){
    
    currentPoint <- toCompute[1]
    toCompute <- toCompute[-1]
    NotAvailablePairs <- c(NotAvailablePairs,currentPoint)
    NotAvailablePairs <- unique(NotAvailablePairs)
    
    if(verbose == TRUE)
    {
      message("--------------------")
      message("Cluster à traiter")
      print(toCompute)
      message("Cluster en traitement")
      print(currentPoint)
      message("Cluster deja traités")
      print(NotAvailablePairs)
    }
    
    for(i in 1:length(spanningtree[,1])){
      if(spanningtree$ArcDepart[i] == currentPoint && !(spanningtree$ArcArrive[i]%in%NotAvailablePairs))
      {
        toCompute <- c(toCompute,spanningtree$ArcArrive[i])
        finalBranch <- c(finalBranch,spanningtree$ArcArrive[i])
      }
      if(spanningtree$ArcArrive[i] == currentPoint && !(spanningtree$ArcDepart[i]%in%NotAvailablePairs))
      {
        toCompute <- c(toCompute,spanningtree$ArcDepart[i])
        finalBranch <- c(finalBranch,spanningtree$ArcDepart[i])
      }
    }
  }
  
  #On complète l'arbre avec des NA pour return la bonne taille
  
  for(z in (length(finalBranch)+1):(length(spanningtree[,1])+1))
  {
    finalBranch <- c(finalBranch,0)
  }
  
  
  if(verbose == TRUE){print(finalBranch)}
  
  returnList <- list("Branch"=finalBranch,"Treated"=NotAvailablePairs)
  
  returnList
}