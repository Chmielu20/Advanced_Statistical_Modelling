# ============================== #
# Advanced Statistical Modelling #
# Class 1                        #
# Filip Chmielowski              #
# ============================== #


# ===== The Dice Rolling =====
plot(1:6, rep(1/6, 6), lwd = 2, type = "h", xlabel = "x", ylabel = "p(x)", main = "DICE")


# ===== The Poisson Distribution =====
?distribution
plot(0:10, dpois(0:10, 2), lwd = 2, type = "h", xlabel = "x", ylabel = "p(x)", main = "Poisson(2)")


# ===== The Binomial Distribution =====
?distribution
plot(0:10, dbinom(0:10, 10, 0.2), lwd = 2, type = "h", xlabel = "x", ylabel = "p(x)", main = "Binomial(n = 10, p = 0.2)")
plot(0:50, dbinom(0:50, 50, 0.2), lwd = 2, type = "h", xlabel = "x", ylabel = "p(x)", main = "Binomial(n = 50, p = 0.2)")


# ===== The Normal Distribution =====
?distribution
plot(function(x) dnorm(x, 0, 1), xlim = c(-5, 5), xlabel = "x", ylabel = "f(x)", main = "Normal(0, X)")
plot(function(x) dnorm(x, 0, 2), xlim = c(-5, 5), xlabel = "x", ylabel = "f(x)", add = TRUE, lty = 2)
legend("topright", c("Normal(0, 1)", "Normal(0, 2)"), lty = c(1, 2))


# ===== The Computing Probabilities [x~normal(5, 3), P(6 < x < 9)] =====
# Exact value: 0.278
pnorm(9, 5, 3) - pnorm(6, 5, 3)
# Simulated value
N = 1000000
x = rnorm(N, 5, 3)
sum(x>6 & x<9)/N

hist(x, breaks = 100)

plot(density(x), xlabel = "x", ylabel = "f(x)", main = "Normal(5, 3)")
plot(function(x) dnorm(x, 5, 3), xlim = c(-10, 10), add = TRUE, lty = 2)
legend("topright", c("Simulation value", "Exact value"), lty = c(1, 2))


# ===== The Monty Hall Problem =====
door = 1:3
n.sim = 100000
Car = sample(door, n.sim, replace = TRUE)
Chose = sample(door, n.sim, replace = TRUE)

Open = rep(NA, n.sim)
ChangeWin = rep(NA, n.sim)

for (i in 1 : n.sim)
{
  if (Car[i] != Chose[i]) Open[i] = door[-c(Car[i], Chose[i])]
  if (Car[i] == Chose[i]) Open[i] = sample(door[-c(Car[i])], 1)
}

for (i in 1 : n.sim)
{
  if (Car[i] != Chose [i]) ChangeWin[i] = 1
  if (Car[i] == Chose [i]) ChangeWin[i] = 0
}

cbind(Car, Chose, Open, ChangeWin)[1 : 10, ]

cat("\n",
    " Win probability after changing:     ", round(sum(ChangeWin)/n.sim, 3), "\n",
    " Win probability without changeing:  ", round((n.sim - sum(ChangeWin))/n.sim, 3), "\n")


# === The Exercise 0.2 - A coin, a blue dice and a special red dice ===
# Number of simulations
n.sim = 100000

# Coin Toss
C = sample(0:1, size = n.sim, prob = c(0.5, 0.5), replace = TRUE)

# Blue Dice
B = sample(1:6, size = n.sim, replace = TRUE)

# Red Dice
red_dice_probs = c(2/9, 1/9, 2/9, 1/9, 2/9, 1/9)
R = sample(1:6, size = n.sim, replace = TRUE, prob = red_dice_probs)

# Tables of values
round(table(C)/n.sim, 2)
round(table(B)/n.sim, 2)
round(table(R)/n.sim, 2)

# New Variable Z
Z = C * (B + R)

# Table of values
round(table(Z)/n.sim, 2)

# Probability Distribution of Z
plot(as.numeric(names(table(Z))), table(Z)/n.sim, type = "h", xlabel = "Z", ylabel = "p(z)", col = "blue", lwd = 3, ylim = c(0, 1))
text(as.numeric(names(table(Z))), 0.05 + table(Z)/n.sim, round(table(Z)/n.sim, 2))
title("Probability distribution of  Z = C * (B + R)")
#hist(Z, breaks = seq(min(Z)-0.5, max(Z)+0.5, by = 1), xlab = "Value of Z", main = "Probability Distribution of Z")

# Probability that Z gets values higher than 1
round(sum(Z > 1)/n.sim, 3)
sum(Z > 1)/n.sim