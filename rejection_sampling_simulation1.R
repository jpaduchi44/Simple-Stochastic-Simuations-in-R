acceptance_rejection_sampling <- function(sample_dist, accept_prob) {
  while (TRUE) {
    x <- sample_dist(1)
    u <- runif(1)
    if (u <= accept_prob(x)) {
      return(x)
    }
  }
}

dcosh_unnormalized <- function(x) {
  return(1 / cosh(x ^ 2 - 1))
}

cosh_accept <- function(x) {
  return(dcosh_unnormalized(x) / (6 * dnorm(x)))
}

rcosh <- function(n) {
  samples <- rep(NA, n)
  for (i in 1:n) {
    samples[i] <- acceptance_rejection_sampling(rnorm, cosh_accept)
  }
  return(samples)
}

x <- rcosh(5000)

plot(density(x))
hist(x, probability=TRUE, breaks=22, add=TRUE)