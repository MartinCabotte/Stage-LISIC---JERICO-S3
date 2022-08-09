#' @title Extract relevant value of a list of quality criteria
#' @author Martin CABOTTE
#' @description 
#' @param list the list of arrays.
#' @param relevant what to compute (Mean, Min, Max, EC).

extract.relevant.values <- function(quality,relevant){

  mean <- c()
  min <- c()
  max <- c()
  ec <- c()
  final <- c()
  
  if("Mean" %in% relevant){
    for(i in 1:length(quality))
    {
      current <- list(mean(quality[[i]]))
      names(current) <- paste("Mean_",names(quality[i]),sep="")
      mean <- c(mean, current)
    }
  }

  if("Min" %in% relevant){
    for(i in 1:length(quality))
    {
      current <- list(min(quality[[i]]))
      names(current) <- paste("Min_",names(quality[i]),sep="")
      min <- c(min, current)
    }
  }
  
  if("Max" %in% relevant){
    for(i in 1:length(quality))
    {
      current <- list(max(quality[[i]]))
      names(current) <- paste("Max_",names(quality[i]),sep="")
      max <- c(max, current)
    }
  }
  
  if("EC" %in% relevant){
    for(i in 1:length(quality))
    {
      current <- list(sqrt(mean(quality[[i]]^2)-mean(quality[[i]])^2))
      names(current) <- paste("EC_",names(quality[i]),sep="")
      ec <- c(ec, current)
    }
  }
  
  final <- c(mean,min,max,ec)
  
  final
}