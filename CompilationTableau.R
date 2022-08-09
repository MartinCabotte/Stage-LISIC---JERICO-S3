#' @title Compile les résultats dans un tableau
#' @author Martin CABOTTE
#' @description 
#' @param saveDirectory path to directory to save.
#' @param nameDf name of dataframe in the load.dataframe function.
#' @param spectral TRUE or FALSE.
#' @param nbcriteria as described in BatterieAnalyse.
#' @param algorithme as described in BatterieAnalyse.
#' @return sauvegarde un .csv dans le fichier demandé.

CompilationTableau <- function(saveDirectory,nameDf,spectral,algorithme,nbcriteria,algorithmeHierarchique,algorithmeNonIteratif){

  if(verbose){message("-- Tableau en cours de réalisation --")}
  dir.create(file.path(saveDirectory,nameDf,"Tableaux"), showWarnings = FALSE)
  
  for(i in 1:length(spectral))
  {
    ##Init list names using last filenameResult
    initTableau <- readRDS(filenameResult)
    
    Tableau <- vector("list", (length(initTableau)+1))
    Tableau.names <- c("Algorithme")
    for(m in 1:length(initTableau))
    {
      Tableau.names <- c(Tableau.names,names(initTableau[m]))
    }
    names(Tableau) <- Tableau.names
    ##Tableau done, compilation des résultats
    
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel"))
    ##On commence avec les algorithmes multilevel
    for(j in 1:length(algorithme))
    {
      setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"MultiLevel",algorithme[[j]]))
      for(k in 1:nbcriteria[j])
      {
        filenameResult <- paste("ML_",spectral[[i]],"_",criteria[[k]],".Rds",sep = "", collapse = NULL)
        values <- readRDS(filenameResult)
        
        cpt <- 0
        for(l in 1:(length(values)+1))
        {
          if(l == 1){
            cpt <- cpt + 1
            Tableau[[cpt]] <- c(Tableau[[cpt]], paste(algorithme[[j]],"_",criteria[[k]],sep=""))
          }
          else{
            if(names(values[l-1]) != "Mean_SilMin" && names(values[l-1]) != "Min_SilMin" && names(values[l-1]) != "Max_SilMin" && names(values[l-1]) != "EC_SilMin")
            {
              cpt <- cpt + 1
              Tableau[[cpt]] <- c(Tableau[[cpt]], values[[l-1]])
            }
          }
        }
      }
    }
    
    ##On passe ensuite au hierarchique
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique"))
    for(j in 1:length(algorithmeHierarchique))
    {
      setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"Hierarchique",algorithmeHierarchique[[j]]))
      filenameResult <- paste("Hierarchical_",spectral[[i]],"_",algorithmeHierarchique[[j]],".Rds",sep = "", collapse = NULL)
      
      values <- readRDS(filenameResult)
      
      cpt <- 0
      for(l in 1:(length(values)+1))
      {
        if(l == 1){
          cpt <- cpt + 1
          Tableau[[cpt]] <- c(Tableau[[cpt]], paste(algorithmeHierarchique[[j]],sep=""))
        }
        else{
          if(names(values[l-1]) != "Mean_minPts" && names(values[l-1]) != "Min_minPts" && names(values[l-1]) != "Max_minPts" && names(values[l-1]) != "EC_minPts")
          {
            cpt <- cpt + 1
            Tableau[[cpt]] <- c(Tableau[[cpt]], values[[l-1]])
          }
        }
      }
    }
    
    ##On termine avec le noniteratif
    setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif"))
    for(j in 1:length(algorithmeNonIteratif))
    {
      setwd(dir=file.path(saveDirectory,nameDf,spectral[[i]],"NonIteratif",algorithmeNonIteratif[[j]]))
      filenameResult <- paste("NonIteratif_",spectral[[i]],"_",algorithmeNonIteratif[[j]],".Rds",sep = "", collapse = NULL)
      
      values <- readRDS(filenameResult)
      
      cpt <- 0
      for(l in 1:(length(values)+1))
      {
        if(l == 1){
          cpt <- cpt + 1
          Tableau[[cpt]] <- c(Tableau[[cpt]], paste(algorithmeNonIteratif[[j]],sep=""))
        }
        else{
          if(names(values[l-1]) != "Mean_SilMin" && names(values[l-1]) != "Min_SilMin" && names(values[l-1]) != "Max_SilMin" && names(values[l-1]) != "EC_SilMin")
          {
            cpt <- cpt + 1
            Tableau[[cpt]] <- c(Tableau[[cpt]], values[[l-1]])
          }
        }
      }
    }
    
    write.csv(Tableau,paste(saveDirectory,nameDf,"/Tableaux/",spectral[[i]],".csv",sep=""))
    message("Tableau terminée", spectral[[i]])
  }

}