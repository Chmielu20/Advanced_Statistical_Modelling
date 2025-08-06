# instaling the library R2jags

install.packages('R2jags')
library(R2jags)


############################################ 
################## DUGONG ################## 
############################################ 


dugong <- read.table("dugong.dat")

par(mfrow=c(1,1))
plot(dugong$age, dugong$length, pch=19)


plot(function(x){3 - 1*exp(-1*x)}, xlim=c(0,100), xlab="age", ylab="length")
 plot(function(x){3 - 1*exp(-0.1*x)}, xlim=c(0,100), add=TRUE, lty=2)
 plot(function(x){3 - 1*exp(-0.01*x)}, xlim=c(0,100), add=TRUE, lty=2)
 legend("bottomright",c("b2=1","b2=0.1","b2=0.01"), lty=c(1,2,3))

# Beta_0 (Expected length of an adult):
# Beta_0 ~ Normal(2.5, sigma = 1)
# Beta_0 - Beta_1 (Expected length at birth => Beta_1 is the growth length along the life):
# Beta_1 ~ Unif(0, 2.5)
# Beta_2 (Growing rate):
# Beta_2 ~ Unif(0, 1)
# Sigma (Expresses the variability among dugongs with some usage):
# sigma ~ Unif(0, 1)
# '~' means 'follows', '<-' means 'is equal to'
dugong.bug <- "
model {
  for(i in 1:N)
  {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 - b1 * exp(-b2 * x[i])
  }
  
  # Priors
  b0 ~ dnorm(2.5, 1)          # Prior: mean around 2.5, sd = 1
  b1 ~ dunif(0, 2.5)          # Prior: mean around 0, sd = 2.5
  b2 ~ dunif(0, 1)            # Prior: mean around 0, sd = 1
  sigma ~ dunif(0, 1)         # Prior for sigma (standard deviation)
  tau <- 1 / (sigma * sigma)  # Precision = 1/variance
}
"


### discarded "Burn", save "Iter", chains "Chain"
 
Iter <- 5000
Burn <- 500
Chain <- 2
Thin <-  1 # 50

data <- list(y = dugong$length, x = dugong$age, n = dim(dugong)[1])

parameters <- c("b0", "b1", "b2", "sigma")

initials=list(list(b0 = 3, b1 = 1, b2= 0.1, sigma = 0.4),
 		  list(b0 = 2, b1 = 1.5, b2= 0.2, sigma = 0.1))


dugong.sim <- jags(data, inits=initials, parameters.to.save=parameters, 
		  n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, 
		  model=textConnection(dugong.bug))

traceplot(dugong.sim, mfrow = c(2,2), varname = c("b0", "b1", "b2", "sigma"), col=c("black","red"))
   

print(dugong.sim, digits=2)


attach.jags(dugong.sim)

# 95% CI age 50
y.pred.50 <- rnorm(length(b0), b0 - b1 * exp(-b2 * 50), sigma)
round(quantile(y.pred.50, c(0.025, 0.975)), 2)

detach.jags()






