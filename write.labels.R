#' @title Write current labels into a label file
#' @author Martin CABOTTE
#' @description 
#' @param labels the labels.
#' @param filenameLabel the name of the file.
#' @param iteration the current iteration.

write.labels <- function(labels,filenameLabel,iteration){
  if(iteration == 1)
  {
    write.csv(labels,filenameLabel,row.names = FALSE)
  }
  else
  {
    previousLabels <- read.csv(filenameLabel)
    unlink(filenameLabel)
    Labels <- cbind(previousLabels, labels)
    write.csv(Labels,filenameLabel,row.names = FALSE)
  }
}