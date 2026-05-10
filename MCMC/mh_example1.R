### Example
f <- function(x) {
  return(0.8 * dnorm(x, 0, 1) + 0.2 * dnorm(x, 4, 0.5))
}
plot(seq(-4, 6.5, 0.05), f(seq(-4, 6.5, 0.05)), type='l')

g <- function(x, y) {
  return(dnorm(y, x, 0.9))
}

gen_g <- function(x) {
  return(rnorm(1, x, 2.45))
}

n <- 10000
chain1 <- metropolis_hastings(0, f, g, gen_g, n)
chain1[[2]]
chain2 <- metropolis_hastings(2, f, g, gen_g, n)
chain2[[2]]
chain3 <- metropolis_hastings(4, f, g, gen_g, n)
chain3[[2]]
par(mfrow=c(3,1))
plot(chain1[[1]], type='l')
plot(chain2[[1]], type='l')
plot(chain3[[1]], type='l')
par(mfrow=c(1,1))
plot(cumsum(chain1[[1]])/(1:n), col='red')
points(cumsum(chain2[[1]])/(1:n), col='blue')
points(cumsum(chain3[[1]])/(1:n), col='green')
abline(h=0.8)
b <- 3000
hist(chain3[[1]][b:n])
acf(chain3[[1]])
