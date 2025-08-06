
# instaling the library R2jags

#install.packages('R2jags')
library(R2jags)


####  CHANGE POINT  BINOMIAL  ANALYSIS  ####

# n1 number of samples before the change point
# n2 number of samples after the change point
# p1 head probability before the change point
# p2 head probability after the change point
# N  sample size for the samples

############### Tria dels par�metres

n1 <- 30
n2 <- 20
n <- n1+n2
N <- 20

# r is n1+1

p1 <- 0.5
p2 <- 0.7

cat("\n",
    "head probability 1st coin  ", p1, "\n",
    "head probability 2nd coin  ", p2, "\n",
    "change point               ", n1+1,  "\n")


############## DATA GENERATION



y1 <- rbinom(n1, size=N, prob=p1)
y2 <- rbinom(n2, size=N, prob=p2)

cbind(c(y1,y2),rep(N, n))

par(mfrow=c(1,1))
plot(1:n,c(y1/N,y2/N), pch=19, ylab="% cares")




############## JAGS' model 


model.BinChangePoint <- "
model  {

 for (i in 1:n) { 

    Y[i] ~ dbin(theta[I[i]] , N[i])

    I[i]<-1+step(i-r) # r is the change.point

 }

 theta[1]~dbeta(a1[1],a1[2])
 theta[2]~dbeta(a2[1],a2[2])

 r~dcat(p[])

}

"


# PRIORS

a1 <- c(1,1) # valors per la beta de theta1
a2 <- c(1,1) # valors per la beta de theta2
prob.r <- rep(1/n,n) # probabilititats per r

# the prior distributions

par(mfrow=c(1,2))
layout(matrix(c(1,2,3,3),2,2,byrow=T))
 plot(function(x)dbeta(x,a1[1],a1[2]), ylab="", xlab=expression(theta[1]), main=expression(paste(pi,"(",theta[1],")")))
 plot(function(x)dbeta(x,a2[1],a2[2]), ylab="", xlab=expression(theta[2]), main=expression(paste(pi,"(",theta[2],")")))
 plot(1:n, prob.r, ty="h", lwd=3, main=expression(paste(pi,"(",r,")")), ylab="",xlab="r")





## INITIAL VALUES

dades <- list(n=n, Y = c(y1,y2), N = rep(N,n), 
         p = prob.r, a1 = a1, a2 = a2)


inits <- list(list(theta =c(0.5,0.5), r=trunc(n*0.5)),
		  list(theta =c(0.1,0.9), r=trunc(n*0.25)),
		  list(theta =c(0.9,0.1), r=trunc(n*0.75)))

parameters <- c("theta", "r")

Iter <- 1500
Burn <- 100
Thin <- 1
Chain <- 3

canvi <- jags(dades, inits, parameters.to.save=parameters,
 			 model=textConnection(model.BinChangePoint),
			n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)


traceplot(canvi , mfrow = c(1,1), varname = c("theta", "r"), col=c("black","red","yellow"))

print(canvi)


attach.jags(canvi)

 table(r)
 round(table(r)/sum(table(r)),4)

par(mfrow=c(1,2))
layout(matrix(c(1,2,3,3),2,2,byrow=T))
 plot(density(theta[,1], adj=2), ylab="", xlab=expression(theta[1]), main=expression(paste(pi,"(",theta[1],"|y)")))
 plot(density(theta[,2], adj=2), ylab="", xlab=expression(theta[2]), main=expression(paste(pi,"(",theta[2],"|y)")))
 plot(table(r)/sum(table(r)), main=expression(paste(pi,"(",r,"|y)")), lwd=3)

detach.jags()






