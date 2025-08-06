

# instaling the library R2JAGS

#install.packages('R2jags', repos = "http://cran.fhcrc.org/")
library(R2jags)


#################  HOSPITALS  (Surgical: Institutional ranking)  ###################

# random sample of 12 hospitals from around the country

# The objective of this study is to know the probability of death around all the 
# hospitals in the country, not only in the hospitals that are in the sample

# y[i]: the number of deaths performing a specific cardiac surgery in hospital i-th
# n[i]: number of operations in hospital i-th

y <- c(0, 18, 8, 46, 8, 13, 9, 31, 14, 8, 29, 24)
n <- c(47, 148, 119, 810, 211, 196, 148, 215, 207, 97, 256, 360)

p.obs <- y/n

hosp <- c("A","B","C","D","E","F","G","H","I","J","K","L")

t <- length(y) # sample size

Data <- cbind(y,n)
rownames(Data) <- hosp
Data

#################  MODELS  
# Models follows the binomial distributions

HospitalA.bug <- "
model {
	for (i in 1:t)  {
    y[i] ~ dbin(p, n[i])
	}
  p ~ dbeta(1, 1)

}
"

HospitalB.bug <- "
model {
	for (i in 1:t)  {
    y[i] ~ dbin(p[i], n[i])
    p[i] ~ dbeta(1, 1)
	}
}
"

HospitalC.bug <- "
model {
	for (i in 1:t)  {
    y[i] ~ dbin(p[i], n[i])
    p[i] ~ dbeta(a, b)
	}
  a ~ dgamma(0.01, 0.001)
  b ~ dgamma(0.01, 0.001)
}
"



### discarded "Burn", save "Iter", chains "Chain"

Iter <- 2500
Burn <- 500
Chain <- 2
Thin <- 1

### discarded "Burn", save "Iter" every "Thin", 
### it means WinBUGS will simulate Burn+Iter*Thin for every chain,
### the number of chains is"Chain"


#### MODEL HospitalA   ######################


dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=0.9),list(p=0.1))
parametres <- c("p")


HospitalA <- jags(dades, inicials, parameters.to.save=parametres, 
 			 model=textConnection(HospitalA.bug),
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain)


traceplot(HospitalA , mfrow = c(1,1), varname = c("p"))


print(HospitalA, digits=4)





#### MODEL HospitalB   ######################

dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=rep(0.9,t)),list(p=rep(0.1,t)))
parametres <- c("p")


HospitalB <- jags(dades, inicials, parameters.to.save=parametres, 
 			 model=textConnection(HospitalB.bug),
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=F)

traceplot(HospitalB , mfrow = c(3,4), varname = c("p"))


print(HospitalB, digits=4)



#### MODEL HospitalC   ######################

Thin <- 10 # 1
dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=rep(0.9,t),a=1,b=1),list(p=rep(0.1,t),a=1,b=1))
parametres <- c("p","a","b")

HospitalC <- jags(dades, inicials, parameters.to.save=parametres, 
 			 model=textConnection(HospitalC.bug),
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=F)

traceplot(HospitalC , mfrow = c(3,5), varname = parametres )


print(HospitalC, digits=4)





####  MODELS A B I C

layout(matrix(c(1,2,3,4,4,4),2,3,byrow=T))

P <- matrix(0,nrow=t, ncol=3)

attach.jags(HospitalA)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model A", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p, outline = F, col = "green", add = T, at = i)
 P[i,1] <- mean(p)
}
detach.jags()

attach.jags(HospitalB)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model B", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
 P[i,2] <- mean(p[,i])
}
detach.jags()

attach.jags(HospitalC)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model C", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
 P[i,3] <- mean(p[,i])
}
detach.jags()


p.col <- rep(0,t)

plot(c(0.8,3.3),c(0,0.15),ty="n",xaxt="n", xlab="model", ylab="p", ylim=c(0,0.2))
 for ( i in 1:t) {
  p.col="red"
  if (n[i]>150) p.col="blue"
  lines(1:3,P[i,],col=p.col)
 }
  axis(side=1, at=1:3, labels=c("A","B","C"), cex.axis=1.5, tcl=0)

attach.jags(HospitalA)
 boxplot(p, boxwex = 0.1, width = NULL, outline = F, col = "green", add = T, at = 0.9)
detach.jags()

attach.jags(HospitalC)
p.sim.p <- rbeta(Iter*Chain, a, b)
 boxplot(p.sim.p, boxwex = 0.1, width = NULL, outline = F, col = "green", add = T, at = 3.12)
detach.jags()








###################### posterior distribution of the ranks


attach.jags(HospitalC)

RANK <- t(apply(p,1,rank))

par(mfrow=c(3,4))
for(i in 1:12) {
 plot(table(RANK[,i])/length(a), ylab="", main=hosp[i])
}

detach.jags()

colnames(RANK) <- hosp
apply(RANK,2,function(x)quantile(x,prob=c(0.1,0.9)))










################### reparamatrized like a Generalized Linear Model


HospitalC2.bug <- "
model {
	for (i in 1:t)  {
		y[i] ~ dbin(p[i], n[i])
		logit(p[i]) <- b[i]
		b[i] ~ dnorm(mu,tau)
	}
	mu ~ dnorm(0, 0.001)
	tau ~ dgamma(0.01, 0.01)
}
"



#### MODEL HospitalC2: alternative parameterization  ######################


dades <- list(t=t, y = y, n = n)
inicials <- list(list(tau=1),list(tau=1))
parametres <- c("p","mu","tau")


HospitalC2 <- jags(dades, inicials, parameters.to.save=parametres, 
 			 model=textConnection(HospitalC2.bug),
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=F)



par(mfrow=c(1,2))

attach.jags(HospitalC)
plot(1:t, p.obs, ylab="p", xlab="type", main="Model C", pch=19, ylim=c(0,0.2), ty="n")
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.jags()


attach.jags(HospitalC2)
plot(1:t, p.obs, ylab="p", xlab="type", main="Model C2", pch=19, ylim=c(0,0.2), ty="n")
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.jags()
















