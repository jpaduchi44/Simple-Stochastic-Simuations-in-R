# Combination of Importance Sampling and Antithetic Variables method for rare event

# Estimate the integral of ________________________ 

set.seed(0)

# Functions
f1 <- function(x) {
  res <- 0
  res[x > 5] <- x[x > 5]
  return(res)
}
f2 <- function(x) {
  res <- 0
  res[x > 5] <- -4 * x[x > 5]
  return(res)
}
ant_var <- function(x) { return(5 - x) }
gen <- function(num_sample) { return(rnorm(num_sample, 5, 1)) }

# Sample sizes and true value
n <- c(1000, 2000, 5000, 8000)
size <- 100
#true_val <- 2.87e-7

# Function that computes results of a given method 
method_results <- function(method, ...) {
  mc_mean <- rep(NA, length(n))
  mc_err <- rep(NA, length(n))
  for (i in 1:length(n)) {
    samp <- mc_int.mc_sample(method, size, n[i], ...)
    mc_mean[i] <- mean(samp)
    mc_err[i] <- sd(samp)
  }
  return(list(mc_mean, mc_err))
}
# Calculate means and errors for all methods and save results
results <- list()
results <- append(results, method_results(mc_int.standard, f1, rnorm))
results <- append(results, method_results(mc_int.standard, f2, gen))
#results[[3]] <- exp(25/2) * results[[3]]
results <- append(results, method_results(mc_int.antithetic, f2, ant_var, gen))
#results[[5]] <- exp(25/2) * results[[5]]

# Plots
par(mfrow=c(2, 2))
for (i in seq(1, 5, 2)) {
  mc_int.plot_mc(n, results[[i]], results[[i+1]])
}

plot(n, results[[2]], 
     ylim=range(c(results[[2]], results[[6]])), log='xy',
     type='l', col='red', lwd=2,
     ylab='MC error', xlab='number of samples', 
     main='Monte-Carlo Error (log-log scale)')
lines(n, results[[4]], type='l', pch=19, col='blue', lwd=2)
lines(n, results[[6]], type='l', pch=19, col='green', lwd=2)
legend('topright', c('Standard MC', 'importance', 'Import + Antith'), col=c('red', 'blue', 'green'), lty=c(1, 1, 1), lwd=c(2, 2,2))
