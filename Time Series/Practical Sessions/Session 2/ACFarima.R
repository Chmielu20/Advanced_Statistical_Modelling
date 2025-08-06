# Paràmetres del model

# Obtenim les valors de l'acf del model teòric fins al retard 60
model.ma<-c(rep(0,11),0.6)
model.acf<-ARMAacf(ma=model.ma,lag.max=60)

model.ar<-c(rep(0,11),-0.6)
model.ma<-c(0.6)
model.acf<-ARMAacf(ar=model.ar,ma=model.ma,lag.max=60)

model.ar=c(0.6)
model.ma<-c(0, rep(0,10),0.6)
model.acf<-ARMAacf(ar=model.ar,ma=model.ma,lag.max=60)
#
#Dibuixem els valors com a barres verticals (type="h") i limitem l'eix #vertical als valors entre 1 i -1. A més dibuixem una línea horitzontal que #farà d'eix d'abscises.
plot(model.acf,type="h",ylim=c(-1,1),col=c(2,rep(1,11)),lwd=2)
abline(h=0)
win.graph()

#Fem el mateix amb la pacf
model.pacf<-ARMAacf(ar=model.ar,ma=model.ma,lag.max=60,pacf=T)
plot(model.pacf,type="h",ylim=c(-1,1),col=c(rep("black",11),"red"),lwd=2)
abline(h=0)
