n <- 1000
max_samples <- rep(NA, n)
for (i in 1:n) {
  max_samples[i] <- max(runif(2))
}
sqrt_samples <- sqrt(runif(n))
par(mfrow=c(1,2))
hist(max_samples)
hist(sqrt_samples)
ks.test(max_samples, sqrt_samples)
plot(density(max_samples))
plot(density(sqrt_samples))
