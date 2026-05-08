set.seed(1)

# 3-ball
d <- 3
exact_vol <- ball_volume.exact(d)

# Rejection Sampling Approximations
n <- c(10, 100, 1000, 10000, 100000)
size <- 80
monte_carlo_sample <- matrix(nrow=size, ncol=length(n))
for (i in 1:length(n)) {
  monte_carlo_sample[ ,i] <- ball_volume.mc_sample(ball_volume.rejection_sampling, size , n[i], d)
}
mc_mean <- colMeans(monte_carlo_sample)
mc_err <- apply(monte_carlo_sample, 2, sd)

# Plots
par(mfrow=c(1,2))
plot(n, mc_mean, ylim=range(c(mc_mean-mc_err, mc_mean+mc_err)), pch=19, 
     ylab='Mean +- std', xlab='number of samples', log='x', 
     main='Approximation using Rejection Sampling')
arrows(n, mc_mean-mc_err, n, mc_mean+mc_err, length=0.05, angle=90, code=3)
abline(h=exact_vol, lty='dotted')

plot(n, mc_err, type='l', log='xy', pch=19,
     ylab='MC error (log-scale)', xlab='number of samples',
     main='Monte-Carlo Error')
     
# Print Result
cat(paste(d,'-Ball Volume Approximation:\n', 
          round(mc_mean[length(n)], digits=6), ' +- ', round(mc_err[length(n)], digits=6), 
          sep=''))
