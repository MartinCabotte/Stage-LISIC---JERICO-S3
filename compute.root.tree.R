#' @title Calcule les racines de l'arbre
#' @author Martin CABOTTE
#' @description 
#' @param nbBranch nombre de branches finales.
#' @param spanningtree objet de la fonction spanningtree.

compute.root.tree <- function(nbBranch,spanningtree){
  currentK <- 0
  branch <- c()
  cost <- c()
  KFinal <- nbBranch
  
  ##On créé le bon nombre de racine
  while(currentK < KFinal)
  {
    ##On détermine la racine de la prochaine souche
    minMembership <- which.min(spanningtree$Cost)
    
    ##On prend les pairs
    pairs <- c(spanningtree$ArcDepart[minMembership],spanningtree$ArcArrive[minMembership])
    
    ##Si on a pairs qui est dans branch, dans ce cas, on retire la racine
    if(spanningtree$ArcDepart[minMembership] %in% branch){pairs <- pairs[-1]}
    if(spanningtree$ArcArrive[minMembership] %in% branch){
      if(length(pairs) == 1){pairs <- pairs[-1]}
      else{pairs <- pairs[-2]}
    }
    
    ##Si on a trop de racines, dans ce cas, on en selectionne une
    if((length(branch)+length(pairs)) > KFinal){
      if(length(pairs) == 1){pairs <- c()}
      else{
        if(length(branch)+1 == KFinal){pairs <- pairs[1]}
        else{pairs <- c()}
      }
    }
    
    if(length(pairs) == 2){
      cost <- c(cost,spanningtree$Cost[minMembership],spanningtree$Cost[minMembership])
    }else if(length(pairs) == 1){
      cost <- c(cost,spanningtree$Cost[minMembership])
    }
    
    ##On affecte les nouvelles racines
    branch <- c(branch,pairs)
    
    ##On mets à 1 le cost de la racine courante
    spanningtree$Cost[minMembership] <- 1
    
    ##CurrentK représente le nombre de racines
    currentK <- length(branch)
  }
  
  branches <- data.frame("branch"=branch,"cost"=cost)
  
  branches
}