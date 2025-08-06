
# dice
plot(1:6, rep(1/6, 6), ty="h", xlab="x", ylab="p(x)", main="dice")

?distribution

#poisson
plot(0:10, dpois(0:10,2), ty="h", xlab="x", ylab="p(x)", main="Poisson(2)")

#binomial
plot(0:10, dbinom(0:10, 10, 0.2), ty="h", xlab="x", ylab="p(x)", main="Binomial(n=10, p=0.2)")
plot(0:50, dbinom(0:50, 50, 0.2), ty="h", xlab="x", ylab="p(x)", main="Binomial(n=10, p=0.2)")

#Normal
plot(function(x)dnorm(x, 0, 1), xlim=c(-5,5), ylab="f(x)")
plot(function(x)dnorm(x, 0, 2), xlim=c(-5,5), lty=2, ylab="f(x)", add=T)
legend("topright", c("Normal(0,1)","Normal(0,2)"), lty=c(1,2))


# probability x~normal(5,3), P(6<x<9)
# exact
pnorm(9,5,3)-pnorm(6,5,3)
# simulation
N <- 1000000
x <- rnorm(N,5,3)
sum(x>6 & x<9)/N

hist(x, breaks=100)
plot(density(x), xlab="x", ylab="f(x)",main="Normal(5,3)")
plot(function(x)dnorm(x, 5,3), xlim=c(-10,10), lty=2, add=T)
 legend("topright",c("simulation","exact"), lty=c(1,2))





## Exercise 0.2: COIN AND DICES


n.sim <- 100000

C <- sample(0:1, n.sim, rep=T)
B <- sample(1:6, n.sim, rep=T)
R <- sample(1:6,n.sim,rep=T, prob=c(2/9,1/9,2/9,1/9,2/9,1/9))  # equivalent option

round(table(C)/n.sim,2)
round(table(B)/n.sim,2)
round(table(R)/n.sim,2)

Z <- C*(B+R)

round(table(Z)/n.sim,2)


plot(as.numeric(names(table(Z))),table(Z)/n.sim , type = "h", ylab="p(z)", xlab = "Z", col="blue", lwd=3 ,ylim=c(0,1))
 text(as.numeric(names(table(Z))), 0.05 + table(Z)/n.sim, round(table(Z)/n.sim,2))
 title("probability distribution of  Z=C*(B+R)")

round(sum(Z>1)/n.sim,3)


