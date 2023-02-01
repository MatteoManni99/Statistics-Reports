data <- read.csv("tabella.csv",na.strings=c(":"))
data1 <- data[rowSums(is.na(data)) == 0,]
rownames(data1)=NULL
data2 <- scale(data1[,-(1)])

#tolgo l'oss united kingdom
data1.22 <- data1[-22,]
rownames(data1.22)=NULL
data3 <- scale(data1.22[,-(1)])

library(cluster)
library(MASS)

#METODO k-means------------------------------------------------------

#Osserviamo l’andamento della somma totale dei quadrati delle distanze di ogni cluster dal metodo k-means.
wss=rep(0,10)
for(k in 2:10){
  wss[k]=kmeans(data2,k,nstart=15)$tot.withinss
}
plot(2:10,wss[2:10],type="b",pch=20)

#L’andamento della silhouette
as=matrix(ncol=2,nrow=10)
for(k in 2:10){
  cl=kmeans(data2,k,nstart=15)$cluster
  as[k,1]=mean(silhouette(cl,dist(data2))[,3])
  as[k,2]=sd(silhouette(cl,dist(data2))[,3])
}

as2=as[2:10,]
ymin=min(as2[,1])-max(as2[,2])
ymax=max(as2[,1])+max(as2[,2])
plot(2:10,as2[,1],type="b",pch=20,ylim=c(ymin,ymax))
segments(2:10,as2[,1]-as2[,2],2:10,as2[,1]+as2[,2])

#Esaminiamo la silhouette nei casi k=2,3,4
plot(silhouette(kmeans(data2,2,nstart=15)$cluster,dist(data2)),col=heat.colors(2),border=par("fg"))
plot(silhouette(kmeans(data2,3,nstart=15)$cluster,dist(data2)),col=heat.colors(3),border=par("fg"))

#pca k=2 data2
k=2
data2.km=kmeans(data2,k,nstart=15)
data2.pca=princomp(data2)
plot(data2.pca$scores,col=1+data2.km$cluster,pch=20,cex=2)
points(predict(data2.pca,data2.km$centers),col=2:(k+1),pch=19)
text(data2.pca$scores,labels=as.character(data1$geo),pos=3)
biplot(data2.pca,col=c("gray","red"))

parcoord(data2,col=as.numeric(data2.km$cluster))

#silhouette k=3 data3
plot(silhouette(kmeans(data3,3,nstart=15)$cluster,dist(data3)),col=heat.colors(3),border=par("fg"))

#pca k=3 data3
k=3
data3.km=kmeans(data3,k,nstart=15)
data3.pca=princomp(data3)
plot(data3.pca$scores,col=1+data3.km$cluster,pch=20,cex=2)
points(predict(data3.pca,data3.km$centers),col=2:(k+1),pch=19)
text(data3.pca$scores,labels=as.character(data1.22$geo),pos=3)
biplot(data3.pca,col=c("gray","red"))

parcoord(data3,col=as.numeric(data3.km$cluster))

#METODO pam------------------------------------------------------

c=rep(0,10)
for(i in 2:10){
  c[i]=pam(data2,i)$silinfo$avg.width
}
plot(2:10,c[2:10],type="b",pch=19)

data2.pam=pam(data2,2)
plot(data2.pam)

#proviamo a cambiare la tipologia di distanza - manhattan

c=rep(0,10)
for(i in 2:10){
  c[i]=pam(data2,i,metric="manhattan")$silinfo$avg.width
}
plot(2:10,c[2:10],type="b",pch=19)

data2.pam2=pam(data2,2,metric="manhattan")
plot(data2.pam2) #meglio perchè l'osservazione 7 viene ceduta all'altro cluster
#con 3 cluster e data3 non vengono risultati sensati come per la distanza euclidea

data3.pam3=pam(data2,2,metric="manhattan") #stesso problema con l'oss 22
plot(data3.pam3)

data3.pam3=pam(data3,3) #proviamo a toglierla
plot(data3.pam3)
data3.pam3=pam(data3,4) #il risultato con 3 non era buono e con 4 neppure lo è
plot(data3.pam3)

plot(data2.pca$scores,col=1+data2.pam2$clustering,pch=20,cex=2) #qui vediamo che funziona come il kmenas con due cluster

parcoord(data2,col=as.numeric(data2.pam2$cluster))

#biplot
data3.pca=princomp(data3)
biplot(data3.pca)
#proviamo a dare un senso
parcoord(data2.pca$scores,col=as.numeric(data3.pam2$clustering))
parcoord(data3.pca$scores[,1:2],col=as.numeric(data3.pam2$clustering))

#METODI GERARCHICI---------------------------------------------------
d<-dist(data2) # distanza euclidea
#d<-dist(data2)^2 # distanza euclidea quadrata
#d<-dist(data2,method="maximum")   # distanza del massimo
#d<-dist(data2,method="manhattan") # distanza manhattan

data.hc=hclust(d) # complete linkage
#data.hc=hclust(d,method="single") # single linkage
#data.hc=hclust(d,method="average") # average linkage

plot(data.hc,hang=-1,cex=0.8)

k=2
data.cut=cutree(data.hc,k)
plot(silhouette(data.cut,d),col=heat.colors(k),border=par("fg"))

#------------------------------------------
#d<-dist(data3) # distanza euclidea
#d<-dist(data3)^2 # distanza euclidea quadrata
#d<-dist(data3,method="maximum")   # distanza del massimo
d<-dist(data3,method="manhattan") # distanza manhattan

#data.hc=hclust(d) # complete linkage
#data.hc=hclust(d,method="single") # single linkage
data.hc=hclust(d,method="average") # average linkage

plot(data.hc,hang=-1,cex=0.8)

k=3
data.cut=cutree(data.hc,k)
plot(silhouette(data.cut,d),col=heat.colors(k),border=par("fg"))
#------------------------------------------------------------------
k=3
d<-dist(data3,method="manhattan")
data.hc=hclust(d,method="average")
data.cut=cutree(data.hc,k)

data3.pca=princomp(data3)
plot(data3.pca$scores,col=1+data.cut,pch=20,cex=2)
#points(data3.pca$scores,col=1+as.integer(iris$Species),pch=20,cex=2)
text(data3.pca$scores,labels=as.character(data1[-22,]$geo),pos=3,cex=1)

parcoord(data3,col=data.cut)




