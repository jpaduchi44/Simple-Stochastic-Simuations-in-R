ball_volume <- function(num_sample, dims) {
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
exact_volume <- function(dims) {
  return(pi ^ (dims / 2) / gamma(dims / 2 + 1))
}
# Area approximation of unit circle
area_approx <- ball_volume(2000, 2)
area <- pi
area_error <- area - area_approx
# Volume approximation of unit sphere
vol_approx <- ball_volume(2000, 3)
vol <- 4 * pi / 3
vol_error <- vol - vol_approx
# Volume approximation of unit hyper-sphere in 4 dimensions
vol4_approx <- ball_volume(8000, 4)
vol4 <- exact_volume(4)
vol4_error <- vol4 - vol4_approx
# Volume approximation of unit hyper-sphere in 5 dimensions
vol5_approx <- ball_volume(8000, 5)
vol5 <- exact_volume(5)
vol5_error <- vol5 - vol5_approx

# Convergence of Monte-Carlo Approximation
# Unit circle
sample_sizes <- c(100, 1000, 10000, 20000)
approximations <- rep(NA, length(sample_sizes))
for (i in 1:length(sample_sizes)) {
  approximations[i] <- ball_volume(sample_sizes[i], 2)  
}
errors <- abs(pi - approximations)
plot(sample_sizes, errors, type="b")
# Unit 5-ball
approximations5 <- rep(NA, length(sample_sizes))
for (i in 1:length(sample_sizes)) {
  approximations5[i] <- ball_volume(sample_sizes[i], 5)  
}
errors5 <- abs(exact_volume(5) - approximations5)
plot(errors5, type="b")
