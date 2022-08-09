#' @title AssignVote
#' @author SClustWeb
#' @description 
#' @param labelVrai true labels.
#' @param label clustering labels to be assigned.
#' @return returns labels assigned according to majority vote

assignVote<-function(labelVrai,label){
  t=table(labelVrai,label)
  n=rownames(t)
  v=apply(t,MARGIN=2,FUN=which.max)
  res=label;
  for (lab in 1:ncol(t)){
    ind=which(label==lab);
    res[ind]=v[lab]
  }
  return (res)
}