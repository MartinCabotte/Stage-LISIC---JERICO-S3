##Script - !!!NOT A FUNCTION!!!

plot(data,col=c("white"))
plot(data,col=clustering$y.pl,pch=as.character(clustering$y.pl))
plot(clustering,X=data)

cluster1 <- data[which(clustering$y.pl == 1),]
cluster2 <- data[which(clustering$y.pl == 2),]
cluster3 <- data[which(clustering$y.pl == 3),]
member1 <- clustering$bel[which(clustering$y.pl==1),1]
member2 <- clustering$bel[which(clustering$y.pl==2),2]
member3 <- clustering$bel[which(clustering$y.pl==3),3]

plot(cluster1)
plot(cluster2)
plot(cluster3)

colfunc<-colorRampPalette(c("white","black"))
colfunc<-colorRampPalette(c("white","red"))
colfunc<-colorRampPalette(c("white","green"))

colors <- (colfunc(300))

colors <- colors[rank(member1)]
colors <- colors[rank(member2)]
colors <- colors[rank(member3)]

points(cluster1,col=colors,pch=16)
points(cluster2,col=colors,pch=16)
points(cluster3,col=colors,pch=16)
