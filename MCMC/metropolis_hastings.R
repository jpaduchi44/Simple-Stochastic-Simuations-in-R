# Metropolis-Hastings implementation for 1-dimensional distributions

# init: starting point of the chain
# target: function for density of target distribution
# transition: function for transition kernel
# gen_prop: function for generating samples from the proposal distribution
# steps: number of steps

# Returns: a list with the values of the chain and 
# the acceptance rate of the algorigthm for fine-tuning of convergence 

metropolis_hastings <- function(init, target, transition, gen_prop, steps) {
  x <- rep(NA, steps)
  x[1] <- init
  acceptance_rate <- 0
  for (i in 2:steps) {
    x_prop <- gen_prop(x[i - 1])
    rho <- log(target(x_prop)) - log(target(x[i - 1])) + log(transition(x[i - 1], x_prop)) - log(transition(x_prop, x[i - 1]))  
    rho <- min(c(0, rho))
    if (runif(1) <= exp(rho)) {
      x[i] <- x_prop
      acceptance_rate <- acceptance_rate + 1
    }
    else {
      x[i] <- x[i - 1]
    }
  }
  return(list(x, acceptance_rate / steps))
}
