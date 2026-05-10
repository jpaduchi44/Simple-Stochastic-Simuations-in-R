set.seed(0)

n_param <- 50 
a <- 4
b <- 2

f1 <- function(theta) {
  return(rbinom(1, n_param, theta))
}

f2 <- function(x) {
  return(rbeta(1, a+x, b+n_param-x))
}

cond <- list(f1, f2)
n <- 6000
gibbs_samp <- gibbs_sampling(c(1, 0.5), cond, n)
plot(gibbs_samp[1, 2000:4000], gibbs_samp[2, 2000:4000])

par(mfrow=c(1, 2))
plot(seq(0, 1, 0.02), dbeta(seq(0, 1, 0.02), a, b), type='l')
hist(gibbs_samp[2, ])
