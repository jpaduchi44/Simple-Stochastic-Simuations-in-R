simulateMC <- function(state_space, transition, initial_prob, N_fin) {
  states <- rep(NA, N_fin)
  states[1] <- sample(state_space, 1, prob=initial_prob)
  for (i in 2:N_fin) {
    prev_state_idx <- which(states[i - 1] == state_space)
    states[i] <- sample(state_space, 1, prob=transition[prev_state_idx, ])
  }
  return(states)
}

simulateDistribution <- function(state_space, transition, initial_prob, N_fin, sample_num) {
  dist_mat <- matrix(0, nrow=length(state_space), ncol=N_fin)
  for (n in 1:sample_num) {
    init_state <- sample(state_space, 1, prob=initial_prob)
    idx <- which(init_state == state_space)
    dist_mat[idx, 1] <- dist_mat[idx, 1] + 1
    for (i in 2:N_fin) {
      next_state <- sample(state_space, 1, prob=transition[idx, ])
      next_idx <- which(next_state == state_space)
      dist_mat[next_idx, i] <- dist_mat[next_idx, i] + 1
      
      idx <- next_idx
    }
  }
  dist_mat <- t(t(dist_mat) / colSums(dist_mat))
  return(dist_mat)
}