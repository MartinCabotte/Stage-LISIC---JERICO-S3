#' @title Load a specific dataset
#' @author Martin CABOTTE
#' @description load the dataset according to his name
#' @param name Name can be "Aggregation", "Coumpound", "Carre", "Serpentins", "Bananas", "AmbiguousCercle", "DS3"
#' @return The dataset with the following framework : X, Y, Labels

load.dataframe <- function(name,workingDirectory,verbose=FALSE){
  
  df <- NULL
  if(name == "Aggregation"){
    df <- read.csv(paste(workingDirectory,"/df.aggregation_1.csv",sep = ""))
  }else if(name == "Coumpound"){
    df <- read.csv(paste(workingDirectory,"/df.coumpound.csv",sep = ""))
  }else if(name == "Carre"){
    df <- read.csv(paste(workingDirectory,"/df.carres.csv",sep = ""))
  }else if(name == "Serpentins"){
    df <- read.csv(paste(workingDirectory,"/df.serpentins.csv",sep = ""))
  }else if(name == "Bananas"){
    df <- read.csv(paste(workingDirectory,"/df.bananas.csv",sep = ""))
    df <- df[,-1]
  }else if(name == "AmbiguousCercle"){
    df <- read.csv(paste(workingDirectory,"/df.AmbiguousCercle.csv",sep = ""))
    df <- df[,-1]
  }else if(name == "DS3"){
    df <- read.csv(paste(workingDirectory,"/df.DS3Resample.csv",sep = ""))
    df <- df[,-1]
  }else if(name == "Simulated"){
    df <- read.csv(paste(workingDirectory,"/df.Simulated.csv",sep = ""))
    df <- df[,-1]
    df <- df[,-3]
  }else{
    stop("Unknown dataset/working directory file!")
  }
    
  
  df
}