simulate_mc <- function(state_space, transition, initial_prob=NULL, N_fin) {
  states <- rep(NA, N_fin)
  if (is.null(initial_prob)) {
    states[1] <- sample(state_space, 1)
  }
  else {
    states[1] <- sample(state_space, 1, prob=initial_prob)
  }
  for (i in 2:N_fin) {
    prev_state_idx <- which(states[i - 1] == state_space)
    states[i] <- sample(state_space, 1, prob=transition[prev_state_idx, ])
  }
  return(states)
}

# Simulation 1
trans_P1 <- matrix(c(0, 0.3, 0.3, 0.4, 0, 0, 0.5, 0.5, 0.2, 0.2, 0.3, 0.3, 0.1, 0.1, 0.6, 0.2), nrow=4, ncol=4, byrow=TRUE)
mc1 <- simulate_mc(1:4, trans_P1, initial_prob=c(1, 0, 0, 0), N_fin=30)
plot(mc1)
# Simulation 2
trans_P2 <- matrix(c(0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0.5), nrow=4, ncol=4, byrow=TRUE)
par(mfrow=c(3,1))
for (i in 1:3) {
  plot(simulate_mc(1:4, trans_P2, initial_prob=c(1, 0, 0, 0), N_fin=30))  
}
N = 800
last_state <- rep(NA, N)
for (i in 1:N) {
  last_state[i] <- simulate_mc(1:4, trans_P2, initial_prob=c(1, 0, 0, 0), N_fin=30)[30]
}
par(mfrow=c(1,1))
hist(last_state)
# Simulation 3
trans_P3 <- matrix(rexp(7 * 7), nrow=7, ncol=7, byrow=TRUE)
trans_P3 <- trans_P3 / rowSums(trans_P3)
N = 800
last_state <- rep(NA, N)
for (i in 1:N) {
  last_state[i] <- simulate_mc(1:7, trans_P3, initial_prob=c(1, 0, 0, 0, 0, 0, 0), N_fin=50)[50]
}
par(mfrow=c(1,2))
hist(last_state, probability=TRUE)
last_state <- rep(NA, N)
for (i in 1:N) {
  last_state[i] <- simulate_mc(1:7, trans_P3, initial_prob=c(1, 0, 0, 0, 0, 0, 0), N_fin=50)[50]
}
hist(last_state, probability=TRUE)