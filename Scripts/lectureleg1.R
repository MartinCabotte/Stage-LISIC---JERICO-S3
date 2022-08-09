#0-vider mon environnement
rm(list=ls(all=T))
graphics.off()
setwd("~/Stage/Tests/AnalysisFiles/")
print("mon repertoire courant de travail")
getwd()
print(getwd())


#1-importer mes données
df=rio::import("Leg1_for_interface.txt")
summary(df)
date = paste(df$Dates, df$Hours)
date = strptime(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
plot(date, df$Chl_OF)

longitude = df$Longitude
latitude = df$Latitude
plot(longitude, latitude)





#lecture cgfs

#composition
classe=rep(0,nrow(df))
classe=classe+(df$Algues.vertes.OF>0)
classe=classe+10*(df$Algues.brunes.OF>0)
classe=classe+100*(df$Cryptophyta.OF>0)
classe=classe+1000*(df$Algues.bleu.vert.OF>0)
print("remember (BCDG :blueCryptoDiatGreen) :1 unit= Blue algae- 1 dozen=cryptophyta - 1 hundred =diatomees - a thousand=Green algae")
table(classe)

#number of algae types : 0 to 4
classeNB=(df$Algues.vertes.OF>0)+(df$Algues.bleu.vert.OF>0)+(df$Algues.brunes.OF>0)+(df$Cryptophyta.OF>0)

ligNA=which(lig <- apply(df, 1, function(x) any(is.na(x))))

print(table(classeNB))
print(table(classe,classeNB))
print("correlation- Algae Number vs composition")
print(cor(classe[-ligNA],classeNB[-ligNA]))

#view by composition
label=as.factor(classe)
label=as.numeric(label)+1 #label=1 unclassed
label[is.na(label)]=1
mycolor=c("grey","blue","red","yellow","cyan","forestgreen","orange","pink","violet","brown","black","green",
          "#004949","#009292","#ff6db6","#ffb6db","#b6dbff",
          "#924900","#db6d00","#004949","#009292",
          "#490092")
plot(x=longitude,y=latitude,cex=0.5,col=mycolor[label],pch=as.character(label),main="CGFS course- algae composition")
plot(x=date,y=label,cex=0.5,col=mycolor[label],pch=as.character(label),main="CGFS time sequence")
axis(side=4, at=1:length(unique(classe)), labels=c("NA",names(table(classe))))


#save(df,vlongitude,vlatitude,vDateTime,classe, classeNB,file="reduceData.Rdata")

############"


summary(df)
id=which(sapply(df, FUN = class)== "numeric")
cor=cor(df[,id],use="pairwise.complete.obs")
corrplot::corrplot(cor)
df = df[,c(8,9,13,18,19,20,21)]
summary(df)

nbNA.ligne=apply(df,MARGIN=1,FUN=function(x){sum(is.na(x))});
indNA =which(nbNA.ligne>0)
df = df[-indNA, ]
data = scale(df)
label = label[-indNA]
label <- label-1
truelabel <- label



