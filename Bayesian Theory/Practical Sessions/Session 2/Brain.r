### ANIMALS' BRAIN WEIGHT EXERCISE 2.2 ###


# setting working directory

setwd("C:/Users/fchmi/Desktop/ASM/2. Bayesian Theory/Practical Sessions/Session 2")


# instaling the library R2jags

#install.packages('R2jags')
library(R2jags)


## exercise: brain weight

brain <- read.table("brain.txt")

y <- brain[,2]
x <- brain[,1]

n <- length(y)

par(mfrow=c(1,1))

# Model: y = Beta_0 + Beta_1 * x_i
plot(x,y,xlab="body (kilogramms)",ylab="brain (gramms)", pch=19, cex=1.5)
# The model is useless

# Model: ln(y) = Beta_0 + Beta_1 * ln(x_i) + E_i
#        y = e^{Beta_0} * (x_i)^{Beta_1} * e^{E_i}
plot(log(x),log(y),xlab="log of body (kilogramms)",ylab="log of brain (gramms)", pch=19, cex=1.5)
# The model is good now


#####  Bayesian Model  #####

regression <- "
model {
 for (i in 1:n) {
  y[i] ~ dnorm(b0 + b1 * x[i], tau)
 }
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
  sigma ~ dunif(0, 10000)
  tau = pow(sigma, -2)
}
"
# "~" sign means "follows"
# "tau = pow(sigma, -2)" means the same as "tau = 1/(sigma*sigma)"


### discarded "Burn", save "Iter", chains "Chain"

Iter <- 10000
Burn <- 1000
Chain <- 2
Thin <- 1

data <- list(y=log(y), x=log(x), n=n)

parameters <- c("b0", "b1", "sigma")

brain.model  <-jags(data, inits=NULL, parameters.to.save=parameters,
 		      model=textConnection(regression),
		      n.iter=(Iter*Thin+Burn), n.burnin=Burn, n.thin=Thin, n.chains=Chain)

traceplot(brain.model, mfrow = c(length(parameters),1), varname = parameters)

print(brain.model)

attach.jags(brain.model)
 b0 <- b0
 b1 <- b1
 sigma <- sigma
detach.jags()


  par(mfrow=c(2,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" )
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= ""  )
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" )


# prediction

M = length(b0)

logy.x100 = rnorm(M, b0+b1*log(100), sigma)
y.x100 = exp(logy.x100)

par(mfrow = c(2, 1))
plot(density(logy.x100))
plot(density(y.x100), xlab = "brain weight")

quantile(y.x100, c(0.025, 0.975))