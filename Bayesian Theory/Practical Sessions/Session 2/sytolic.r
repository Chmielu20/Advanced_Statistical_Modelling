
# instaling the library R2WinBUGS
install.packages('R2jags', repos = "http://cran.fhcrc.org/")
library(R2jags)




##### exercise 2.1  systolyc blood pressure


# chosing the prior for mu and tau

par(mfrow=c(1,2))

plot(function(x)dnorm(x,125,10),xlim=c(75,175), ylab="",main = expression(paste(pi,"(",mu,")")), xlab= "" )
 abline(v=125,lty=2)

plot(function(x)dnorm(x,13,2),xlim=c(0,25), ylab="",main = expression(paste(pi,"(",sigma,")")), xlab= "" )
 abline(v=13,lty=2)



model.norm <- "
model
{
 # statistical model (likelihood)
 for (i in 1:n) {
  y[i]~ dnorm(mu, tau)
 }
 # priors
  mu ~ dnorm(m, t)
  sigma ~ dnorm(m.s, t.s)
  tau = 1/(sigma*sigma)
}
"


data <- list(y=c(98, 160, 136, 128, 130, 114, 123, 134, 128, 107, 123, 125, 129, 135, 154, 115, 126, 132, 136, 130),
             n=20, m=125, t=1/10^2, m.s=13, t.s=1/2^2)

parameters <- c("mu", "sigma")


model.systolyc  <-jags(data, parameters.to.save=parameters,
 			model=textConnection(model.norm))


print(model.systolyc)

attach.jags(model.systolyc)

 par(mfrow=c(2,2))

  plot(density(mu, adjust = 1.5), main = expression(paste(pi,"(",mu,"|y)")), xlab= "" )
   plot(function(x)dnorm(x,125,10),xlim=c(75,175), add=TRUE, lty=2 )

  plot(density(sigma, adjust = 1.5), main = expression(paste(pi,"(",sigma,"|y)")), xlab= "")
   plot(function(x)dnorm(x,13,2),xlim=c(0,25), add=TRUE, lty=2 )


  M <- length(mu)

  pre.post <- rnorm(M , mu, sigma)

  plot(density(pre.post, adjust = 1.5), main = "posterior predictive", xlab="")
  
  round(sum(mu>125)/M, 3)



detach.jags()



