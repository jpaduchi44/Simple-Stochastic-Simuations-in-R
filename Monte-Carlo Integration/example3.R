# Comparison of Standard Monte-Carlo, Antithetic Variables method 
# and Importance Sampling

# Estimate the integral of 1 / (pi * (1 + x ^ 2)) from 2 to infinity

set.seed(0)

# Required functions
f1 <- function(x) { 
  res <- rep(0, length(x))
  res[x > 2] <- 1
  return(res)
}
ant_var <- function(x) { return(-x) }
f3 <- function(x) { return(2 * dcauchy(x)) }
f4 <- function(x) { return(dcauchy(x) / 2) }

gen3 <- function(num_sample){ return(runif(num_sample, min=0, max=2)) }
gen4 <- function(num_sample){ return(runif(num_sample, min=0, max=0.5)) }

# Sample sizes and true value
n <- c(100, 1000, 2000, 5000, 8000, 10000)
size <- 200
true_val <- 0.5 - atan(2) / pi

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
results <- append(results, method_results(mc_int.standard, f1, rcauchy))
results <- append(results, method_results(mc_int.antithetic, f1, ant_var, rcauchy))
results <- append(results, method_results(mc_int.standard, f3, gen3))
results[[5]] <- 0.5 - results[[5]]
results <- append(results, method_results(mc_int.standard, f4, gen4))

# Plots
par(mfrow=c(2, 2))
for (i in seq(1, 7, 2)) {
  mc_int.plot_mc(n, results[[i]], results[[i+1]], true_value=true_val)
}

par(mfrow=c(1, 1))
plot(n, results[[2]], 
     ylim=range(c(results[[2]], results[[8]])), log='xy',
     type='l', col='red', lwd=2,
     ylab='MC error', xlab='number of samples', 
     main='Monte-Carlo Error (log-log scale)')
lines(n, results[[4]], type='l', pch=19, col='blue', lwd=2)
lines(n, results[[6]], type='l', pch=19, col='green', lwd=2)
lines(n, results[[8]], type='l', pch=19, col='black', lwd=2)
legend('topright', c('Standard MC', 'Antithetic', 'Importance 1', 'Importance 2'), col=c('red', 'blue', 'green', 'black'), lty=c(1, 1, 1, 1), lwd=c(2, 2, 2,2))
