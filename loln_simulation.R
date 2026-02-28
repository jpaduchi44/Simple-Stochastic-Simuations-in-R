# Gamma Distribution
n <- 800
a <- 3
b <- 2
true_mean <- a / b
plot(seq(0.1, 3, 0.05), dgamma(seq(0.1, 3, 0.05), a, b), type="l")
data <- rgamma(n, a, b)
sample_means <- cumsum(data) / (1:n)
plot(sample_means, type="l")
abline(h=true_mean)
# Cauchy Distribution
n <- 1000
plot(seq(-3, 3, 0.05), dcauchy(seq(-3, 3, 0.05)), type="l")
data <- rcauchy(n)
sample_means <- cumsum(data) / (1:n)
plot(sample_means, type="l")
abline(h=0)