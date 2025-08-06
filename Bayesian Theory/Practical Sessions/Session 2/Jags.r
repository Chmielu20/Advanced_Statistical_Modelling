
# instaling the library R2jags
install.packages('R2jags', repos = "http://cran.fhcrc.org/")
library(R2jags)


############################## example


# prior's parameters
a <- 50
b <- 50

plot(function(x)dbeta(x,a,b), xlab=expression(theta),  ylab=expression(paste(pi,"(",theta,")")))


n=25
y=11


model.bin <- "
 model
 { 
  y ~ dbin(theta, n)  
  theta ~ dbeta(a,b)
 }
"


parameters <- c("theta")

example <-jags(data=list(n=25, y=11, a=50, b=50), parameters.to.save=c("theta"),
 			model=textConnection(model.bin))


print(example)

theta
attach.jags(example)
theta

par(mfrow=c(1,1))

  plot(density(theta, adjust = 1.5), ylab=expression(paste(pi,"(",theta,"|y)")), main="", xlim=c(0,1))
  plot(function(x)dbeta(x,a,b),lty=2 ,add=TRUE)
   legend("topright", c("prior","posterior"), lty=c(2,1))

detach.jags()





