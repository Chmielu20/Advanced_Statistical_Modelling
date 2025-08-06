
####################  1.1 SEPIA  VERDA   ##############################

#####	prior distribution and prior predictive distribution

prior <- c(alpha = 40 , beta = 2)
#prior <- c(alpha = 10 , beta = 0.1)

plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), ylab="", xlab = expression(lambda))
title("prior distribution")


# simulated prior predictive distribution

M <- 1000000  # Number of simulations

# Simulate lambda values from the Gamma prior
lambda <- rgamma(M, shape = prior[1], rate = prior[2])

# Simulate Poisson-distributed visitors based on simulated lambda values
visitors <- rpois(M, lambda)

# Plot the simulated prior predictive distribution
hist(visitors, breaks = 50, probability = TRUE, col = "salmon",
     main = "Simulated Prior Predictive Distribution", xlab = "Number of Visitors")

plot(table(visitors)/M, col = "pink")
abline(v = c(5, 40), lty = 3, col = "blue")


###### data

y <- c(21,17,17,19,16,18,15,10,17,16) 
n <- length(y)
n
y


####### standardaized likelihood function  

sd.like <- function(th) {
 (th^sum(y)*exp(-n*th))/integrate(function(th)(th^sum(y)*exp(-n*th)), lower = 0, upper = 50)$value
}

par(mfrow=c(1, 1))
plot(function(x)sd.like(x), xlim=c(0,60), ylab="", xlab = expression(lambda))
 plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), add=T, lty=2)
 legend("topright", c("prior", "likelihood"), lty = c(2,1))


#########  posterior distribution

posterior <- c(a = prior[1] + sum(y), b = prior[2] + n)


# prior, likelihood and posterior

plot(function(x)dgamma(x, posterior[1], posterior[2]), xlim=c(0,60), lty=1)
 plot(function(x)sd.like(x), xlim=c(0,60), ylab="", xlab = expression(lambda), add=T, lty=3)
 plot(function(x)dgamma(x, prior[1], prior[2]), xlim=c(0,60), add=T, lty=2)
 legend("topright",  c("prior", "likelihood", "posterior"), lty = c(2,3,1))


#  summary results

results <- matrix(nrow = 7, ncol = 2)

colnames(results ) <- c('prior', 'posterior')
rownames(results ) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')

results [1:2, 1] <- prior
results [3, 1] <- prior[1]/prior[2]
results [4, 1] <- prior[1]/prior[2]^2
results [5, 1] <- qgamma(0.025, shape = prior[1], prior[2])
results [6, 1] <- qgamma(0.5, shape = prior[1], prior[2])
results [7, 1] <- qgamma(0.975, shape = prior[1], prior[2])

results [1:2, 2] <- posterior
results [3, 2] <- posterior[1]/posterior[2]
results [4, 2] <- posterior[1]/posterior[2]^2
results [5, 2] <- qgamma(0.025, shape = posterior[1], posterior[2])
results [6, 2] <- qgamma(0.5, shape = posterior[1], posterior[2])
results [7, 2] <- qgamma(0.975, shape = posterior[1], posterior[2])

round(results , 3)


#  priror and posterior predictive distribution

par(mfrow=c(2,1)) 

M <- 100000

prior.sim <- rgamma(M, prior[1], prior[2])
pre.prior.sim <- rpois(M, prior.sim)

post.sim <- rgamma(M, shape = posterior[1], posterior[2])
post.pre.sim <- rpois(M, post.sim)

plot(table(pre.prior.sim)/M, ty="h", xlim=c(0, 60), ylab = "", xlab = "",  col="blue", lwd=0.5)
 title("prior predictive distribution")

plot(table(post.pre.sim)/M, ty="h", xlim=c(0, 60), ylab = "", xlab = "",  col="blue", lwd=0.5)
title("posterior predictive distribution")