#caricamento in memoria
biblio <- read.csv("tabella.csv")
biblio2 <- biblio[,-1:-2]

#correlazione
library(corrplot)
plot(biblio2,pch=20)
corrplot(cor(biblio2),"square")
corrplot.mixed(cor(biblio2),lower = "number",upper = "square", tl.pos = "lt")

biblio3 = biblio2[,-7]  #tolgo spese_gestione

#PCA
biblio3.pca = princomp(biblio3, cor=T)

summary(biblio3.pca)

plot(biblio3.pca, main = "")
plot(cumsum(biblio3.pca$sdev ^ 2) / sum(biblio3.pca$sdev ^ 2),type = "b",ylim = c(0, 1),xlab = "Principal Components",ylab = "Percentage Cumulative Variance")
segments(1, 0.8, 6, col = "red")

#loading senza rotazione
biblio.ld=loadings(biblio3.pca)
corrplot(biblio.ld[,1:2], is.corr=FALSE, method="color",addCoef.col = "black", number.digits = 2)

#loadings con rotazione
biblio.rot=varimax(biblio.ld[,1:2])$loadings
corrplot(biblio.rot, is.corr=FALSE, method="color",addCoef.col = "black", number.digits = 2)

#biplot
biplot(biblio3.pca, col=c("gray","red"))

#componente riassuntiva
costi=biblio[,9]
plot(costi[order(biblio3.pca$scores[,1])],type="l")

#plot delle componenti dei record sul piano principale
grad=colorRampPalette(c("grey","black"))
scol=grad(10)
x=(costi-min(costi))/(max(costi)-min(costi))
sidx=1+floor(10*0.99*x)
pred=biblio3.pca$scores[,1:2]
plot(pred,pch=19,cex=2,col=scol[sidx],main=ncol)

#REGRESSIONE
#regressione lineare con lettori
biblio.lm=lm(spese_gestione~lettori,data=biblio2)
plot(biblio2$lettori,biblio2$spese_gestione,pch=20)
abline(biblio.lm,lwd=2,col="red")
summary(biblio.lm)

#regressione lineare con posti
biblio.lm=lm(spese_gestione~posti_per_lettori,data=biblio2)
plot(biblio2$posti_per_lettori,biblio2$spese_gestione,pch=20,xlab = "posti_per_lettori",ylab = "spese_gestione")
abline(biblio.lm,lwd=2,col="red")
summary(biblio.lm)

#regressione multivariata
biblio.lm=lm(spese_gestione ~ .,data=biblio2)
summary(biblio.lm)

#riduzione del modello
biblio.lm=lm(spese_gestione ~ posti_per_lettori + opere_consultate,data=biblio2)
summary(biblio.lm)

#previsione
m=500
semp_err=rep(0,m)
multi_err=rep(0,m)

for(i in 1:m){
  idx=sample(28,3)
  biblio_train=biblio2[-idx,]
  biblio_test=biblio2[idx,]
  biblio_train.lm.semp=lm(spese_gestione ~ posti_per_lettori, data=biblio_train)
  biblio_train.lm.multi=lm(spese_gestione ~ posti_per_lettori + opere_consultate, data=biblio_train)
  
  biblio.lm.p.semp=predict(biblio_train.lm.semp,biblio_test)
  biblio.lm.p.multi=predict(biblio_train.lm.multi,biblio_test)
  
  semp_err[i] = mean((biblio.lm.p.semp - biblio_test$spese_gestione)^2)
  multi_err[i] = mean((biblio.lm.p.multi - biblio_test$spese_gestione)^2)
}

sqrt(mean(semp_err))
sqrt(mean(multi_err))

plot(sqrt(multi_err),type = "l", col = "red", xlab = "Iteration", ylab = "Average Error")
lines(sqrt(semp_err), col = "blue")
segments(0, sqrt(mean(multi_err)), m, sqrt(mean(multi_err)), col = "darkRed")
segments(0, sqrt(mean(semp_err)), m, sqrt(mean(semp_err)), col = "darkBlue")

(100/sqrt(mean(multi_err)))*sqrt(mean(semp_err)) #percentuale di differenza

#residui
biblio.lm=lm(spese_gestione ~ posti_per_lettori + opere_consultate,data=biblio2)
biblio.lm.r=residuals(biblio.lm)
plot(predict(biblio.lm),biblio.lm.r,pch=20)

1-var(biblio.lm.r)/var(biblio$spese_gestione) # coincide con la varianza spiegata
plot(fitted(biblio.lm),biblio.lm.r,pch=20)
hist(biblio.lm.r,20,freq=F)

hist(biblio.lm.r,freq=F,20)
lines(density(biblio.lm.r),col="red")
lines(sort(biblio.lm.r),dnorm(sort(biblio.lm.r),m=mean(biblio.lm.r),sd(biblio.lm.r)))

qqnorm(biblio.lm.r, pch=15)
qqline(biblio.lm.r, col="red")

shapiro.test(biblio.lm.r)

#Stima delle incertezze nella previsione
alpha=0.95
idx=sample(28,3)
biblio_train=biblio2[-idx,]
biblio_test=biblio2[idx,]
biblio_train.lm=lm(spese_gestione ~ posti_per_lettori + opere_consultate,data=biblio_train)

#intervalli di confidenza
biblio_test.ci=predict(biblio_train.lm,biblio_test,interval="confidence",level=alpha)
#intervalli di predizione
biblio_test.pi=predict(biblio_train.lm,biblio_test,interval="prediction",level=alpha)
#intervalli di confidenza empirici
biblio_train.r=resid(biblio_train.lm)
qi=quantile(biblio_train.r,(1-alpha)/2)
qs=quantile(biblio_train.r,(1+alpha)/2)


ymin=min(c(biblio_test.pi[,2],biblio_test.ci[,2]))
ymax=max(c(biblio_test.pi[,3],biblio_test.ci[,3]))
plot(biblio_test$spese_gestione,ylim=c(ymin,ymax))
x=1:3

#intervalli di predizione
segments(x,biblio_test.pi[,2],x,biblio_test.pi[,3],col="blue",lwd=18)
#intervalli di confidenza empirici
segments(x,biblio_test.pi[,1]+qi,x,biblio_test.pi[,1]+qs,col="green4",lwd=12)
#intervalli di confidenza parametrici
segments(x,biblio_test.ci[,2],x,biblio_test.ci[,3],col="red",lwd=6)
#valori stimati
points(x,biblio_test.pi[,1],pch=19,col="cyan",cex=1.5)
points(biblio_test$spese_gestione,pch=19,col="yellow",cex=1.5)
