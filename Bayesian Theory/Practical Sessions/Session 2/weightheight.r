### WEIGHT AND HEIGHT OF THE STUDENTS EXERCISE 5.2 ###


# setting working directory

setwd("C:/Users/fchmi/Desktop/ASM/2. Bayesian Theory/Practical Sessions/Session 2")


# instaling the library R2jags

install.packages('R2jags')
library(R2jags)


########### exercise 5.2   weight-height-gender

##### read data


Data <- read.table("WeightHeight.txt", dec=",", header=TRUE)

n <- dim(Data)[1] # simple size

Data

plot(Data$height, Data$weight, pch=19)


# parametres MCMC

Iter <- 10000
Burn <- 1000
Chain <- 2



########   linear regression

M1.bug <- "
model {
  for (i in 1:n) {
    y[i] ~ dnorm(b0 + b1 * x[i], tau)
  }
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
  tau ~ dgamma(0.01, 0.001)
}
"

data <- list(n=n, y = Data$weight, x = Data$height)

initials <- list(list(tau=1, b0=0, b1=0),list(tau=0.1, b0=10, b1=10))

parameters <- c("b0", "b1", "tau")

model.M1 <- jags(data, initials, parameters.to.save=parameters, 
 		      model=textConnection(M1.bug),             
      		n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M1, mfrow = c(length(parameters),1), varname = parameters)

print(model.M1)

attach.jags(model.M1)

 sigma <- sqrt(1/tau)

 par(mfrow=c(2,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)

      plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
 	 points(Data$height, Data$weight, pch=19, col="gray50")
 	 abline(coef=c(mean(b0),mean(b1)))

detach.jags()



########   linear regression with a dicothomy co-variable

# Model:           y ~ Normal( Beta_0 + Beta_1 * h_i + Beta_2 * S_i )
# Model for women: y = Beta_0 + Beta_1 * h_i
# Model for men:   y = Beta_0 + Beta_1 * h_i + Beta_2 = (Beta_0 + Beta_2) + Beta_1 * h_i
M2.bug <- "
model {
  for (i in 1:n) {
    y[i] ~ dnorm(b0 + b1 * x1[i] + b2 * x2[i], tau)
  }
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
  b2 ~ dnorm(0, 1.0E-7)
  tau ~ dgamma(0.01, 0.001)
}
"

data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initial <- list(list(tau=1, b0=0, b1=0, b2=10),list(tau=0.1, b0=10, b1=10, b2=-10))

parameters <- c("b0", "b1", "b2", "tau")

model.M2 <- jags(data, initials, parameters.to.save=parameters, 
 		      model=textConnection(M2.bug),                 
      		n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M2, mfrow = c(length(parameters),1), varname = parameters)

print(model.M2)

attach.jags(model.M2)

 sigma <- sqrt(1/tau)

 par(mfrow=c(3,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)

  plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
   points(Data$height[Data$sex==0], Data$weight[Data$sex==0], pch=19, col="gray80")
   points(Data$height[Data$sex==1], Data$weight[Data$sex==1], pch=19, col="gray50")
   abline(coef=c(mean(b0),mean(b1)), lty=2)
   abline(coef=c(mean(b0)+mean(b2),mean(b1)), lty=3)
   legend("topleft", c("women","men"), lty=c(2,3))

detach.jags()



########   linear regression with a dicothomy co-variable and interaction

# Model:           y ~ Normal( Beta_0 + Beta_1 * h_i + Beta_2 * S_i + Beta_3 * (h_i * S_i) )
# Model for women: y = Beta_0 + Beta_1 * h_i
# Model for men:   y = Beta_0 + Beta_1 * h_i + Beta_2 + Beta_3 * h_i =
#                    = (Beta_0 + Beta_2) + (Beta_1 + Beta_3) * h_i
M3.bug <- "
model {
  for (i in 1:n) {
    y[i] ~ dnorm(b0 + b1 * x1[i] + b2 * x2[i] + b3 * x1[i] * x2[i], tau)
  }
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
  b2 ~ dnorm(0, 1.0E-7)
  b3 ~ dnorm(0, 1.0E-7)
  tau ~ dgamma(0.01, 0.001)
}
"

data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initials <- list(list(tau=1, b0=0, b1=0, b2=10, b3=-1),list(tau=0.1, b0=10, b1=10, b2=-10, b3=1))

parameters <- c("b0", "b1", "b2", "b3", "tau")

model.M3 <- jags(data, initials, parameters.to.save=parameters, 
 		      model=textConnection(M3.bug),               
      		n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M3, mfrow = c(3,2), varname = parameters)
   
print(model.M3)

attach.jags(model.M3)

 sigma <- sqrt(1/tau)

 par(mfrow=c(3,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(b3, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"|y)  ; sex*height")), xlab= ""  ); abline(v=quantile(b3,c(0.025,0.975)),lty=3)
	plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "" ); abline(v=quantile(sigma,c(0.025,0.975)),lty=3)

# scatterplot wiht the two lines, men and women

plot(c(1.55,1.85), c(50,100), type = "n", xlab = "height", ylab = "weight" )
 points(Data$height[Data$sex==0], Data$weight[Data$sex==0], pch=19, col="gray80")
 points(Data$height[Data$sex==1], Data$weight[Data$sex==1], pch=19, col="gray50")
 abline(coef=c(mean(b0),mean(b1)), lty=2)
 abline(coef=c(mean(b0)+mean(b2),mean(b1)+mean(b3)), lty=3)
 legend("topleft", c("women","men"), lty=c(2,3))

detach.jags()



########   linear regression with a dicothomy co-variable and interaction, and different vairance

# Model:           y ~ Normal( Beta_0 + Beta_1 * h_i + Beta_2 * S_i + Beta_3 * (h_i * S_i), E_1 * S_i + E_2 * (1 - S_i))
M4.bug <- "
model {
  for (i in 1:n) {
    y[i] ~ dnorm(b0 + b1 * x1[i] + b2 * x2[i] + b3 * x1[i] * x2[i], tau1 * x2[i] + tau2 * (1 - x2[i]))
  }
  b0 ~ dnorm(0, 1.0E-7)
  b1 ~ dnorm(0, 1.0E-7)
  b2 ~ dnorm(0, 1.0E-7)
  b3 ~ dnorm(0, 1.0E-7)
  tau1 ~ dgamma(0.01, 0.001)
  tau2 ~ dgamma(0.01, 0.001)
}
"


data <- list(n=n, y = Data$weight, x1 = Data$height, x2 = Data$sex)

initials <- list(list(tau1=1, tau2=1, b0=0, b1=0, b2=10, b3=-1),list(tau1=0.1, tau2=0.1, b0=10, b1=10, b2=-10, b3=1))

parameters <- c("b0", "b1", "b2", "b3", "tau1", "tau2")

model.M4 <- jags(data, initials, parameters.to.save=parameters, 
 		      model=textConnection(M4.bug),                
     			n.iter=(Iter+Burn),n.burnin=Burn, n.thin=1, n.chains=Chain)       

traceplot(model.M4, mfrow = c(3,2), varname = parameters)
       
print(model.M4)

attach.jags(model.M4)


 sigma1 <- sqrt(1/tau1)
 sigma2 <- sqrt(1/tau2)


 par(mfrow=c(3,2))

	plot(density(b0, adjust = 1.5), main = expression(paste(pi,"(",beta[0],"|y)")), xlab= "" ); abline(v=quantile(b0,c(0.025,0.975)),lty=3)
	plot(density(b1, adjust = 1.5), main = expression(paste(pi,"(",beta[1],"|y)  ; height")), xlab= ""  ); abline(v=quantile(b1,c(0.025,0.975)),lty=3)
	plot(density(b2, adjust = 1.5), main = expression(paste(pi,"(",beta[2],"|y)  ; sex")), xlab= ""  ); abline(v=quantile(b2,c(0.025,0.975)),lty=3)
	plot(density(b3, adjust = 1.5), main = expression(paste(pi,"(",beta[3],"|y)  ; sex*height")), xlab= ""  ); abline(v=quantile(b3,c(0.025,0.975)),lty=3)
	plot(density(sigma1, adjust = 1.5), main = expression(paste(pi,"(",sigma[1],"|y) and ",pi,"(",sigma[2],"|y)")), xlab= "" )
	lines(density(sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[2],"|y)")), xlab= "" )
	plot(density(sigma1/sigma2, adjust = 1.5), main = expression(paste(pi,"(",sigma[1],"/",sigma[2],"|y)")), xlab= "" ); abline(v=quantile(sigma1/sigma2,c(0.025,0.975)),lty=3)

detach.jags()
