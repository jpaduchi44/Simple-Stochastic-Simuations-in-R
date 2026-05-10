# Gibbs sampling implementation

# init: initial position of chain 
# conditionals: list of functions that generate samples from
#               the full conditional distributions
# steps: number of steps

# Returns: vector with the values of the chain

gibbs_sampling <- function(init, conditionals, steps) {
  p <- length(init)
  x <- matrix(NA, nrow=p, ncol=steps)
  x[ , 1] <- init
  for (j in 2:steps) {
    x[1, j] <- conditionals[[1]](x[2:p, j-1])
    i <- 2
    while (i < p) {
      x[i, j] <- conditionals[[i]](c(x[1:(i-1), j], x[(i+1):p, j-1]))
      i <- i + 1
    } 
    x[p, j] <- conditionals[[p]](x[1:(p-1), j])
  }
  return(x)
}
