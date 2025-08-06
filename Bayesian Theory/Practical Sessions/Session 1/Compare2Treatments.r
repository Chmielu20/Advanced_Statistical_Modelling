

####	prior distribution 


#http://www.wolframalpha.com/
#solve( a/(a+b)=, sqrt((ab)/((a+b)^2*(a+b+1)))=)



prior.C <-c(alpha = 13.8 , beta = 9.2)

prior.E <-c(alpha = 24.25, beta = 8.1)



suport <- seq(from = 0, to = 1, length = 100)
dist.prior.C <- dbeta(suport, prior.C[1], prior.C[2])
dist.prior.E <- dbeta(suport, prior.E[1], prior.E[2])

par(mfrow=c(1,1))

plot(suport,dist.prior.C, ylab="", xlab = "theta", ty="l", ylim=c(0, max(dist.prior.C,dist.prior.E)))
 lines(suport, dist.prior.E, lty=2)
 title("prior distributions")
 legend("topleft",c("Tr. Conventional","Tr. Experimental"), lty=c(1,2))


#	data

N <- 40
y.C <- 24
y.E <- 30



#########  posterior distribution

posterior.C <- c(alpha = prior.C[1] + y.C, beta = prior.C[2] + (N-y.C))
posterior.E <- c(alpha = prior.E[1] + y.E, beta = prior.E[2] + (N-y.E))



####### graph: prior and posterior distribution, and standardaized likelihood function  


dist.posterior.C <- dbeta(suport,posterior.C[1],posterior.C[2])
dist.posterior.E <- dbeta(suport,posterior.E[1],posterior.E[2])


vers.C <- dbinom(y.C,N,suport)/integrate(function(x)dbinom(y.C,N,x), lower = 0, upper = 1, subdivisions=1000)$value
vers.E <- dbinom(y.E,N,suport)/integrate(function(x)dbinom(y.E,N,x), lower = 0, upper = 1, subdivisions=1000)$value


par(mfrow=c(1,2))

plot(suport,dist.prior.C, ylab="", xlab=expression(theta),type="l", ylim=c(0, max(dist.posterior.C,dist.posterior.E)), lty=2)
 lines(suport,dist.posterior.C)
 lines(suport,vers.C, lty=3)
 legend("topleft", c("prior","posterior","likelihood"), lty = c(2,1,3))
 title("Conventional Treatment")


plot(suport,dist.prior.E, ylab="", xlab=expression(theta),type="l", ylim=c(0, max(dist.posterior.C,dist.posterior.E)), lty=2)
 lines(suport,dist.posterior.E)
 lines(suport,vers.E, lty=3)
 legend("topleft", c("prior","posterior","likelihood"), lty = c(2,1,3))
 title("Experimental Treatment")



#  summary results

sortida.C <- matrix(nrow = 7, ncol = 2)

colnames(sortida.C) <- c('prior', 'posterior')
rownames(sortida.C) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')


sortida.C[1:2, 1] <- prior.C
sortida.C[3, 1] <- prior.C[1]/(prior.C[1] + prior.C[2])
sortida.C[4, 1] <- (prior.C[1]*prior.C[2])/(((prior.C[1]+prior.C[2])^2)*(prior.C[1]+prior.C[2]+1))
sortida.C[5, 1] <- qbeta(0.025, prior.C[1], prior.C[2])
sortida.C[6, 1] <- qbeta(0.5, prior.C[1], prior.C[2])
sortida.C[7, 1] <- qbeta(0.975, prior.C[1], prior.C[2])

sortida.C[1:2, 2] <- posterior.C
sortida.C[3, 2] <- posterior.C[1]/(posterior.C[1] + posterior.C[2])
sortida.C[4, 2] <- (posterior.C[1]*posterior.C[2])/(((posterior.C[1]+posterior.C[2])^2)*(posterior.C[1]+posterior.C[2]+1))
sortida.C[5, 2] <- qbeta(0.025, posterior.C[1], posterior.C[2])
sortida.C[6, 2] <- qbeta(0.5, posterior.C[1], posterior.C[2])
sortida.C[7, 2] <- qbeta(0.975, posterior.C[1], posterior.C[2])


sortida.E <- matrix(nrow = 7, ncol = 2)

colnames(sortida.E) <-  c('prior', 'posterior')
rownames(sortida.E) <- c('alpha', 'beta', 'mean', 'variance', '2,5%', 'median', '97.5%')


sortida.E[1:2, 1] <- prior.E
sortida.E[3, 1] <- prior.E[1]/(prior.E[1] + prior.E[2])
sortida.E[4, 1] <- (prior.E[1]*prior.E[2])/(((prior.E[1]+prior.E[2])^2)*(prior.E[1]+prior.E[2]+1))
sortida.E[5, 1] <- qbeta(0.025, prior.E[1], prior.E[2])
sortida.E[6, 1] <- qbeta(0.5, prior.E[1], prior.E[2])
sortida.E[7, 1] <- qbeta(0.975, prior.E[1], prior.E[2])

sortida.E[1:2, 2] <- posterior.E
sortida.E[3, 2] <- posterior.E[1]/(posterior.E[1] + posterior.E[2])
sortida.E[4, 2] <- (posterior.E[1]*posterior.E[2])/(((posterior.E[1]+posterior.E[2])^2)*(posterior.E[1]+posterior.E[2]+1))
sortida.E[5, 2] <- qbeta(0.025, posterior.E[1], posterior.E[2])
sortida.E[6, 2] <- qbeta(0.5, posterior.E[1], posterior.E[2])
sortida.E[7, 2] <- qbeta(0.975, posterior.E[1], posterior.E[2])


round(sortida.C, 3)
round(sortida.E, 3)




# comparing to proportions throug simulations


M <- 10000000

z.posterior.C <- rbeta(M, posterior.C[1], posterior.C[2])
z.posterior.E <- rbeta(M, posterior.E[1], posterior.E[2])

sum(z.posterior.E>z.posterior.C)/M




# computing OR

OR <- (z.posterior.E/(1-z.posterior.E))/(z.posterior.C/(1-z.posterior.C))

par(mfrow=c(1,1))
plot(density(OR), ylab = "", xlab = "", main = "distributior of the OR")

abline(v=c(quantile(OR, 0.025),quantile(OR, 0.975)))
abline(v=1, lwd=3)

quantile(OR, 0.025)
quantile(OR, 0.975)
mean(OR)

sum(OR>1)/M





