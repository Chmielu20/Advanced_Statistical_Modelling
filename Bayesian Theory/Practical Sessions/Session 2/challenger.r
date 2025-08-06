

# instaling the library R2jags

#install.packages('R2jags')
library(R2jags)



##################  CHALLENGER

challenger <- read.table("challenger.dat", header=TRUE)

challenger

plot(challenger$T, challenger$incident, pch=19, xlim=c(min(challenger$T),max(challenger$T)), xlab="T", ylab="incident")

# y^{~} | P_i ~ Bernoulli(P in (0, 1))
# R = Beta_0 + Beta_1 * T_i
# Beta_0 ~ Normal(0, ...)  |  Beta_1 ~ Normal(0, ...)  =>  Huge variance
# P_i = (e^{Beta_0 + Beta_1 * T_i}) / (1 + e^{Beta_0 + Beta_1 * T_i})
model.bug <- "
model {
  for(i in 1:n) {
    y[i] ~ dbern(p[i])
    p[i] <- exp(b0 + b1 * x[i]) / (1 + exp(b0 + b1 * x[i]))
    # logit(p[i]) <- b0 + b1 * x[i]   =>   Means the same as the line above
  }
  # Priors
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
}
"


Iter <- 10000
Burn <- 500
Chain <- 2
Thin <- 1

data <- list(y = challenger$incident, x = challenger$T, n = dim(challenger)[1])

parameters <- c("b0", "b1")

initials=list(list(b0 = 15, b1 = -0.2),
 		  list(b0 = -15, b1 = 0.2))


challenger.sim <- jags(data, inits=initials, parameters.to.save=parameters, 
 		       model=textConnection(model.bug),  
		       n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=F)

traceplot(challenger.sim, mfrow = c(1,1), varname = c("b0", "b1"), col=c("black","red"))
   
print(challenger.sim)

attach.jags(challenger.sim)

 plot(challenger$T, challenger$incident, pch=19, xlim=c(min(challenger$T),max(challenger$T)))

 temp <- seq(20,90,1)

 b0 <- as.vector(b0)
 b1 <- as.vector(b1)

 p.pred <- exp(b0+ b1%*%t(temp))/(1+exp(b0+b1%*%t(temp)))
 p.pred.exp <- apply(p.pred,2,mean)

 lines(temp, p.pred.exp, type="l",lwd=2)

 p.pred.CI <- apply(p.pred,2,quantile,c(0.025,0.975))
  lines(temp, p.pred.CI[1,], lty=2)
  lines(temp, p.pred.CI[2,], lty=2)

 plot(temp, p.pred.exp, type="l",lwd=2)
  lines(temp, p.pred.CI[1,], lty=2)
  lines(temp, p.pred.CI[2,], lty=2)
  abline(v=28, lty=3, col="blue")

 # a credible interval for the probability of incident when the temperature is 28

predict.p.28 <- exp(b0 + b1 * 28) / (1 + exp(b0 + b1 * 28))
quantile(predict.p.28, c(0.025, 0.975))

detach.jags()

