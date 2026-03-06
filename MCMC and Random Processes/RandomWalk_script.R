rw1 <- simulateRW(N_fin=1000)
plot(rw1, type="l")
abline(h=0)

rw_2d <- simulateRW_2d(N_fin=500)
plot(rw_2d[[1]], rw_2d[[2]], type="l")

rw2 <- simulateRW(start= -20, N_fin=1000, p=0.52)
plot(rw2, type="l")
abline(h=0)

urw <- simulateURW(N_fin=800, a=0.2)
plot(urw[[1]], urw[[2]], type="l")