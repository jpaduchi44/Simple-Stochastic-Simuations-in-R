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

simulateRW <- function(start=0, N_fin, p=0.5) {
  states <- rep(NA, N_fin)
  states[1] <- start
  for (i in 2:N_fin) {
    d <- sample(c(-1, 1), 1, prob=c(1 - p, p))
    states[i] <- states[i - 1] + d
  }
  return(states)
}

simulateRW_2d <- function(x_start=0, y_start=0, N_fin) {
  directions <- list(c(1, 0), c(1, 1), c(0, 1), c(-1, 1),
                     c(-1, 0), c(-1, -1), c(0, -1), c(1, -1))
  x <- rep(NA, N_fin)
  y <- rep(NA, N_fin)
  x[1] <- x_start
  y[1] <- y_start
  
  for (i in 2:N_fin) {
    d <- sample(directions, 1)
    x[i] <- x[i - 1] + d[[1]][1]
    y[i] <- y[i - 1] + d[[1]][2]
  }
  return(list(x, y))
}

simulateURW <- function(x_start=0, y_start=0, N_fin, a=0.5) {
  x <- rep(NA, N_fin)
  y <- rep(NA, N_fin)
  x[1] <- x_start
  y[1] <- y_start
  
  for (i in 2:N_fin) {
    x[i] <- x[i - 1] + runif(1, -a, a)
    y[i] <- y[i - 1] + runif(1, -a, a)
  }
  return(list(x, y))
}