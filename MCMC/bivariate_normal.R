### Bivariate Normal Distribution simulation using Gibbs sampling
set.seed(2)

rho <- 0.5
f1 <- function(x2) {
  return(rnorm(1, rho * x2, 1 - rho ^ 2))
}

f2 <- function(x1) {
  return(rnorm(1, rho * x1, 1 - rho ^ 2))
}

cond <- list(f1, f2)
n <- 3000
par(mfrow=c(1,2))
gibbs_samp <- gibbs_sampling(c(0, 0), cond, n)
plot(gibbs_samp[1, ], gibbs_samp[2, ],
     main='Gibbs Sampling')

library(MASS)
norm_samp <- mvrnorm(n, c(0, 0), matrix(c(1, rho, rho, 1), nrow=2))
plot(norm_samp[, 1], norm_samp[, 2],
     main='MASS library')
