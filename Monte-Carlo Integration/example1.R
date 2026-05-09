# Integrate 1/(1+x) over [1, 0]
set.seed(0)

estimator1 <- function(x) { return(1 / (1 + x)) }
estimator2 <- function(x) { return(1 + x) }

n <- c(10, 100, 1000, 1500, 10000)
size <- 100

# Estimate mean and standard error of standard MC and Control Variates methods
mc_mean1 <- rep(NA, length(n))
mc_mean2 <- rep(NA, length(n))
mc_err1 <- rep(NA, length(n))
mc_err2 <- rep(NA, length(n))

for (i in 1:length(n)) {
  # Standard Monte-Carlo
  sample1 <- mc_int.mc_sample(mc_int.standard, size, n[i], estimator1, runif)
  mc_mean1[i] <- mean(sample1)
  mc_err1[i] <- sd(sample1)
  # Control Variates
  sample2 <- mc_int.mc_sample(mc_int.control_variates, size, n[i], estimator1, estimator2, runif, 1.5)  
  mc_mean2[i] <- mean(sample2)
  mc_err2[i] <- sd(sample2)
}

# Plots
par(mfrow=c(2,2))
mc_int.plot_mc(n, mc_mean1, mc_err1, log(2), 'Standard MC')
mc_int.plot_mc(n, mc_mean2, mc_err2, log(2), 'Control Variates')

plot(n, mc_err1, type='l', ylim=range(c(mc_err1, mc_err2)), pch=19, col='red',
     ylab='MC error', xlab='number of samples', lwd=2, log='xy',
     main='Monte-Carlo Error (log-log scale)')
lines(n, mc_err2, type='l', pch=19, col='blue', lwd=2)
legend('topright', c('Standard MC', 'Rejection'), col=c('red', 'blue'), lty=c(1, 1), lwd=c(2,2))
