N <- 12
bJ <- 0.3
bf <- exp(-2 * bJ)
par(mfrow=c(2, 2))
grid <- matrix(sample(c(-1, 1), N*N, replace=TRUE), nrow=N, ncol=N)
heatmap(grid, Colv=NA, Rowv=NA)
for (t in 1:100) {
  i <- sample(1:N, 1)
  j <- sample(1:N, 1)
  n_sum <- grid[i,j] * (grid[(i+1)%%N,j] + grid[(i-1)%%N,j] + grid[i,(j+1)%%N] + grid[i,(j-1)%%N])
  if (n_sum <= 0 || runif(1) < bf ^ n_sum) {
    grid[i, j] <- -grid[i,j]
  }
}
heatmap(grid, Colv=NA, Rowv=NA)