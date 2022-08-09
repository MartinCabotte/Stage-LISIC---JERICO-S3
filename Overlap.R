#' @title Compute overlap criterion
#' @author Martin CABOTTE
#' @description 
#' @param Vrai true labels.
#' @param Pred predicted labels.
#' @return returns non-overlap criterion value

#Function based on the rand.index function from the package fossil
#We want only one part of the rand index which is the one where two predicted points 
#of a cluster are in 2 different classes.

#We want here to count all pairs of points that match the following formula :
#                   Vrai && (!Pred)
#This means that we want to count all pairs where we have the value 1 in the
#true classes matching a 0 in the prediction. This gives us all the overlapping 
#pairs


#We want to criterion to maximize so we are going to do (1 - overlap) which will
#give us a "non-overlapping" criterion
Overlap <- function (Vrai, Pred) 
{
  group1 <- Vrai
  group2 <- Pred
  
  #We transform our groups of labels into binary matrices : 0 means that the labels 
  #are the same and 1 means that labels are different
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  #Same logic on the second group
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 1] <- 1
  
  #We transform x into logical array (0->FALSE, 1->TRUE) to be able to manipulate it
  logicalx <- as.logical(x)
  noty <- !y
  
  sg <- sum(logicalx[noty==TRUE])/2
  
  #Total of pairs we can do
  bc <- choose(dim(x)[1], 2)
  
  #Non overlapping criterion
  overlap <- 1 - sg/bc
  return(overlap)
}