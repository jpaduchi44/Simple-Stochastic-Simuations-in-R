# Standard Monte-Carlo method
# Also used for importance sampling by appropriately
# changing the est parameter
mc_int.standard <- function(num_sample, est, gen_x) {
  x <- gen_x(num_sample)
  approx <- mean(est(x))
  return(approx)
}

# Method of Control Variates
mc_int.control_variates <- function(num_sample, est1, est2, gen_x, exp_val) {
  sample1 <- est1(gen_x(num_sample))
  sample2 <- est2(gen_x(num_sample))
  b <- cov(sample1, sample2) / var(sample2)
  print(b)
  approx <- mean(sample1) - b * (mean(sample2) - exp_val)
  return(approx)
}

# Method of Antithetic Variables
mc_int.antithetic <- function(num_sample, est, ant_var, gen_x) {
  samp <- gen_x(num_sample)
  approx <- 0.5 * mean(est(samp) + est(ant_var(samp)))
  return(approx)
}

# Uses one of the above methods to create a sample of Monte-Carlo approximations
# Can be used to estimate Monte-Carlo error of method
mc_int.mc_sample <- function(method, mc_sample_size, num_sample, ...) {
  samp <- rep(NA, mc_sample_size)
  for (i in 1:mc_sample_size) {
    samp[i] <- method(num_sample, ...)
  }
  return(samp)
}

# Plots Monte-Carlo mean and error for different sample sizes
mc_int.plot_mc<- function(n, mc_mean, mc_err, true_value=NULL, main='') {
  plot(n, mc_mean, ylim=range(c(mc_mean-mc_err, mc_mean+mc_err)), 
       pch=19, , log='x',
       ylab='mean +- std', xlab='number of samples', 
       main=main)
  arrows(n, mc_mean-mc_err, n, mc_mean+mc_err, 
         length=0.05, angle=90, code=3)
  if (!is.null(true_value )) {
    abline(h=true_value, lty='dotted')
  }
}