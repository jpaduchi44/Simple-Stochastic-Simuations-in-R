ball_volume.exact <- function(dims) {
  return(pi ^ (dims / 2) / gamma(dims / 2 + 1))
}

ball_volume.rejection_sampling <- function(num_sample, dims) {
  r <- rep(0, num_sample)
  for (i in 1:dims) {
    u <- runif(num_sample, -1, 1)
    r <- r + u ^ 2
  }
  in_ball <- rep(0, num_sample)
  in_ball[r <= 1] <- 1
  log_probability <- log(sum(in_ball)) - log(num_sample)
  volume_approx <- exp(log_probability + log(2 ^ dims))
  return(volume_approx)
}

ball_volume.mc_sample <- function(method, mc_sample_size, num_sample, dims) {
  samp <- rep(NA, mc_sample_size)
  for (i in 1:mc_sample_size) {
    samp[i] <- method(num_sample, dims)
  }
  return(samp)
}
