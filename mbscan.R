mbscan <- function(iteration,df,KFinal,facteurClustering=5,verbose=FALSE,gap=0.3,computeMembership=TRUE,criteria){
  
  bestclustering <- c()
  minMass <- -3 ## Lowest returned value is -2, meaning points are missing to compute criteria
  minknn <- -3 ## Maximum returned value is 1, meaning points are all well assigned
  
  if(length(df[,1]) > (facteurClustering*KFinal))
  {
    for(i in 1:iteration)
    {
      list <- compute_cmeans_fusion(df,KFinal,facteurClustering = facteurClustering,verbose=verbose,gap=gap,computeMembership=computeMembership)
      
      if(criteria == "Mass25" && minMass < min(list$Mass25)){
        minMass <- min(list$Mass25)
        bestclustering <- list
        print(paste("Iteration",i,", current min(Mass25) :",min(list$Mass25),", best min(Mass25) :",minMass))
        
      }else if(criteria == "Mass50" && minMass < min(list$Mass50)){
        minMass <- min(list$Mass50)
        bestclustering <- list
        print(paste("Iteration",i,", current min(Mass50) :",min(list$Mass50),", best min(Mass50) :",minMass))
  
      }else if(criteria == "Mass100" && minMass < min(list$Mass100)){
        minMass <- min(list$Mass100)
        bestclustering <- list
        print(paste("Iteration",i,", current min(Mass100) :",min(list$Mass100),", best min(Mass100) :",minMass))
      }else if(criteria == "knn" && minknn < min(list$knn)){
        minknn <- min(list$knn)
        bestclustering <- list
        print(paste("Iteration",i,", current min(knn) :",min(list$knn),", best min(knn) :",minknn))
      }
      
      
      if(verbose){plot(list$df[,-3],col=list$labels)}
    } 
  }    
  
  ##No convergence, le dataset est trop petit
  if(criteria == "Mass25" || criteria == "Mass50" || criteria == "Mass100"){
    if(minMass == -2){list("labels"=rep(0,length(df[,1])),"Silhouette"=-2,"df"=df,"membership"=NA,"Mass25"=-2,"Mass50"=-2,"Mass100"=-2,"knn"=1,"Stabilite"=0)}
  }else if(criteria == "knn"){
    if(minknn == -3){list("labels"=rep(0,length(df[,1])),"Silhouette"=-2,"df"=df,"membership"=NA,"Mass25"=-2,"Mass50"=-2,"Mass100"=-2,"knn"=1,"Stabilite"=0)}
  }
  
  bestclustering
}