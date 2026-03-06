state_space <- 1:6
prob_row <- c(0.2,0.4,0.4, 0, 0, 0,
0.6, 0, 0, 0.4, 0, 0, 
0.4, 0, 0.4, 0 ,0.2, 0, 
0, 0.2, 0.2, 0.3, 0.1,0.2, 
0, 0, 0.3, 0.4, 0.3, 0, 
0, 0, 0, 0.6, 0, 0.4)

trans_P <- matrix(prob_row, nrow=6, ncol=6, byrow=TRUE)

e <- eigen(t(trans_P))
e$values
pc <- e$vectors[ , 1]
pc <- pc / sum(pc)
pc

init_dist <- c(1, 0, 0, 0, 0, 0)
sim_mat <- simulateDistribution(state_space, trans_P, init_dist, 200, 1000)
heatmap(sim_mat,Rowv=NA, Colv=NA)
sim_mat[ , 80]