#library(tseries)


arimaRet=function(ser){
ser=ts(ser)
ret=diff(log(ser))

ret.ar=ar(ret,order.max=30,aic=T)
ret.arima=arima(ret,order=c(ret.ar$order,0,0))

par(mfrow=c(2,4))
par(mar=c(2,1.2,3,1)+0.1)

plot(ret,main="Returns")
resid=ret.arima$resid

hist(resid,prob=T,col=4,main="Histograma dels residus")
curve(dnorm(x,mean(resid,na.rm=T),sd(resid,na.rm=T)),col=2,add=T)

acf(resid,ylim=c(-0.5,0.5),lag.max=30,main="ACF dels residus",na.action=na.omit)

acf(resid^2,ylim=c(-0.5,0.5),lag.max=30,main="ACF dels residus^2",na.action=na.omit)

plot(resid,main="Residus")
abline(h=0,col=2)
qqnorm(resid,main="Normal-Plot dels residus")
qqline(resid,col=2)
pacf(resid,ylim=c(-0.5,0.5),lag.max=30,main="PACF dels residus",na.action=na.omit)
pacf(resid^2,ylim=c(-0.5,0.5),lag.max=30,main="PACF dels residus^2",na.action=na.omit)


cat("Per a la sèrie dels rendiments s'ha ajustat un AR(p).\n",sep="")
cat("El valor de p que minimitza l'AIC ha estat p=",ret.ar$order,".\nPosteriorment, s'ha estimat per màxima versemblança el model.\n")
cat("Els plots que permeten validar el model s'inclouen en la representació gràfica.\n") 
cat("S'ha detectat possible volatilitat en els residus degut a que l'autocorrelació dels residus al quadrat és significativa.\n")
cat("\nModel AR(p) Yule-Walker:\n")
print(ret.ar)
cat("\nModel AR(p) Máxima-versemblança:\n")
print(ret.arima)
list(serie=ser,return=ret,ar=ret.arima)
}

garchRet=function(ser){

library(tseries)
ser=ts(ser)
ret=diff(log(ser))
ret.ar=ar(ret,order.max=30,aic=T)
ret.arima=arima(ret,order=c(ret.ar$order,0,0))
par(mfrow=c(2,4))
par(mar=c(2,1.2,3,1)+0.1)

plot(ret,main=paste("resid_AR(",ret.ar$order,")",sep=""))
resid=ret.arima$resid

res.garch=garch(resid)
resid=res.garch$resid[-1]

hist(resid,prob=T,col=4,main="Histograma dels residus")
curve(dnorm(x,mean(resid,na.rm=T),sd(resid,na.rm=T)),col=2,add=T)

acf(resid,ylim=c(-0.5,0.5),lag.max=30,main="ACF dels residus",na.action=na.omit)

acf(resid^2,ylim=c(-0.5,0.5),lag.max=30,main="ACF dels residus^2",na.action=na.omit)

ts.plot(resid,main="Residus")
abline(h=0,col=2)
qqnorm(resid,main="Normal-Plot dels residus")
qqline(resid,col=2)
pacf(resid,ylim=c(-0.5,0.5),lag.max=30,main="PACF dels residus",na.action=na.omit)
pacf(resid^2,ylim=c(-0.5,0.5),lag.max=30,main="PACF dels residus^2",na.action=na.omit)

cat("Per a la sèrie dels rendiments  s'ha ajustat un AR(p).\n",sep="")
cat("El valor de p que minimitza l'AIC ha estat p=",ret.ar$order,".\nPosteriorment, s'ha estimat per màxima versemblança el model.\n")
cat("Els plots que permeten validar el model s'inclouen en la representació gràfica.\n") 
cat("S'ha detectat possible volatilitat en els residus degut a que l'autocorrelació dels residus al quadrat és significativa.\n")
cat("\nModel AR(p) Yule-Walker:\n")
print(ret.ar)
cat("\nModel AR(p) Máxima-versemblança:\n")
print(ret.arima)
cat("\nModel GARCH(1,1) pels residus del model anterior:\n")
print(res.garch)
list(serie=ser,return=ret,ar=ret.arima,garch=res.garch)
}


