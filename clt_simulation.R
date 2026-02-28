# Gamma Distribution
total_samples <- 200
n <- 800
means <- rep(NA, total_samples)
a = 3
b = 4
true_mean <- a / b
std <- sqrt(a / b ^ 2)
for (i in 1:total_samples) {
  means[i] <- (sum(rgamma(n, a, b)) / n - true_mean) * sqrt(n) / std
}
hist(means)