##Script qui compile tous les .Rds dans un grand .csv pour ensuite traitement dans un tableau .ods

#Les paramètres à régler sont : racine: racine de la compilation du tableau
#spectral : si l'analyse est dans le domaine spectral ou non
#label_directory: non des dossiers dans lesquels se trouvent les résultats.

#Si tout l'analyse ne veut pas être compilée, le i sert à restreindre les directories
#Le j sert à restreindre les algorithmes (noniteratif, hierarchique, cmean, kmean, ecm)

##ATTENTION

#Les dossiers d'analyse doivent avoir la structure suivante :

#racine/label_directory/label_dataset/

#Pour le noniteratif et le hierarchique, un chiffre est ajouté à la suite du nom de fichier correspondant à
#l'emplacement du directory dans label_directory. Par exemple, pour banana, nous aurions : kmean2.csv

racine <- "~/Stage/Tests/Analyse_Propre/Domaine_Initial/"
spectral <- FALSE
setwd(dir=racine)

directory <- matrix(0,ncol=6,nrow=2)
label_directory <- list("Aggregation","Banana","Cercle","DS3","Coumpound","Sinus")

for(i in 1:length(label_directory))
{
  directory[1,i] <- i
  directory[2,i] <- label_directory[[i]]
}

label_dataset <- list("NonIteratif","Hierarchical","kmean","cmean","ecm")
label_kmean <- list("CardSil.Rds","Silhouette.Rds")
label_cmean <- list("CardSil.Rds","Silhouette.Rds","FuzzySilhouette.Rds","masslowbel25.Rds","masslowbel50.Rds","masslowbel100.Rds")

Final <- list()

for(i in 1:(length(label_directory)))
{
  Final <- list()
  
  
  label_nonite <- list(paste("kmean",i,".Rds",sep = ""),paste("cmean",i,".Rds",sep = ""),paste("ecmbel",i,".Rds",sep = ""))
  
  if(spectral == TRUE)
  {
    label_hierarchical <- list(paste("shcFALSE",i,".Rds",sep = ""),paste("shcTRUE",i,".Rds",sep = ""),paste("hdbscanFALSE",i,".Rds",sep = ""),paste("hdbscanTRUE",i,".Rds",sep = ""))
  }
  else
  {
    label_hierarchical <- list(paste("hcFALSE",i,".Rds",sep = ""),paste("hcTRUE",i,".Rds",sep = ""),paste("hdbscanFALSE",i,".Rds",sep = ""),paste("hdbscanTRUE",i,".Rds",sep = ""))
  }
  
  for(j in 1:length(label_dataset))
  {
    setwd(dir=paste(racine,"/",label_directory[[i]],"/",label_dataset[[j]],sep = ""))
    # setwd(dir=paste(racine,"/",label_dataset[[j]],sep = ""))
    
    if(j == 1)
    {
      label <- label_nonite
    }
    if(j == 2)
    {
      label <- label_hierarchical
    }
    if(j == 3)
    {
      label <- label_kmean
    }
    if(j == 5 || j == 4)
    {
      label <- label_cmean
    }
    
    for(k in 1:length(label))
    {
      values <- readRDS(label[[k]])
      
      
      Final <- list("avgSil_Mean"=rbind(Final$avgSil_Mean,values$avgSil_Mean),
                      "avgSil_Min"=rbind(Final$avgSil_Min,values$avgSil_Min),
                      "avgSil_Max"=rbind(Final$avgSil_Max,values$avgSil_Max),
                      "ari_Mean"=rbind(Final$ari_Mean,values$ari_Mean),
                      "ari_Min"=rbind(Final$ari_Min,values$ari_Min),
                      "ari_Max"=rbind(Final$ari_Max,values$ari_Max),
                      "Vmes_Mean"=rbind(Final$Vmes_Mean,values$Vmes_Mean),
                      "Vmes_Min"=rbind(Final$Vmes_Min,values$Vmes_Min),
                      "Vmes_Max"=rbind(Final$Vmes_Max,values$Vmes_Max),
                      "FM_Mean"=rbind(Final$FM_Mean,values$FM_Mean),
                      "FM_Min"=rbind(Final$FM_Min,values$FM_Min),
                      "FM_Max"=rbind(Final$FM_Max,values$FM_Max),
                      "F1_Mean"=rbind(Final$F1_Mean,values$F1_Mean),
                      "F1_Min"=rbind(Final$F1_Min,values$F1_Min),
                      "F1_Max"=rbind(Final$F1_Max,values$F1_Max),
                      "Accuracy_Mean"=rbind(Final$Accuracy_Mean,values$Accuracy_Mean),
                      "Accuracy_Min"=rbind(Final$Accuracy_Min,values$Accuracy_Min),
                      "Accuracy_Max"=rbind(Final$Accuracy_Max,values$Accuracy_Max),
                      "Precision_Mean"=rbind(Final$Precision_Mean,values$Precision_Mean),
                      "Precision_Min"=rbind(Final$Precision_Min,values$Precision_Min),
                      "Precision_Max"=rbind(Final$Precision_Max,values$Precision_Max),
                      "Recall_Mean"=rbind(Final$Recall_Mean,values$Recall_Mean),
                      "Recall_Min"=rbind(Final$Recall_Min,values$Recall_Min),
                      "Recall_Max"=rbind(Final$Recall_Max,values$Recall_Max),
                      "Overlap_Mean"=rbind(Final$Overlap_Mean,values$Overlap_Mean),
                      "Overlap_Min"=rbind(Final$Overlap_Min,values$Overlap_Min),
                      "Overlap_Max"=rbind(Final$Overlap_Max,values$Overlap_Max),
                      "Cluster_nbclusters"=rbind(Final$Cluster_nbclusters,values$Cluster_nbclusters),
                      "Cluster_MaxCluster"=rbind(Final$Cluster_MaxCluster,values$Cluster_MaxCluster),
                      "Cluster_MinCluster"=rbind(Final$Cluster_MinCluster,values$Cluster_MinCluster),
                      "Cluster_nbMax"=rbind(Final$Cluster_nbMax,values$Cluster_nbMax),
                      "Cluster_nbMin"=rbind(Final$Cluster_nbMin,values$Cluster_nbMin))
        
       
    }
  }
  
  setwd(dir = racine)
  write.csv(Final,paste(label_directory[[i]],".csv",sep = ""))
}