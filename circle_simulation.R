set.seed(0)

num_sample = 1000
u1 <- runif(num_sample, -1, 1)
u2 <- runif(num_sample, -1, 1)
r <- u1 ^ 2 + u2 ^ 2
success <- rep(0, num_sample)
success[r <= 1] <- 1
prob <- sum(success) / num_sample
plot(u1, u2, xlim=c(-1, 1), ylim=c(-1, 1))
polygon(cos(seq(0, 2*pi, 0.1)), sin(seq(0, 2*pi, 0.1)))
