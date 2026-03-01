# Monte-Carlo Integration 1
# Integrate f(x)=1/log(x) on [2, 5]
n <- 500
a <- 2
b <- 5
u <- runif(n, a, b)
f <- 1 / log(u)
I <- mean(f) * (b - a)
# Monte-Carlo Integration 2
# Integrate f(x)=log(log(x)) on [1.1, 3]
n <- 500
a <- 1.1
b <- 3
u <- runif(n, a, b)
f <- log(log(u))
I <- mean(f) * (b - a)
# Monte-Carlo Integration 3
# Integrate f(x)=sin(x^2) on [0, 3]
n <- 40000
a <- 0
b <- 3
u <- runif(n, a, b)
f <- sin(u ^ 2)
I <- mean(f) * (b - a)
# Monte-Carlo Integration 4
# Integrate f(x)=sqrt(1-x^4) on [-1, 1]
n <- 4000
a <- -1
b <- 1
u <- runif(n, a, b)
f <- sqrt(1 - u ^ 4)
I <- mean(f) * (b - a)