state_space <- 1:6
prob_row <- c(0.2,0.4,0.4, 0, 0, 0,
0.6, 0, 0, 0.4, 0, 0, 
0.4, 0, 0.4, 0 ,0.2, 0, 
0, 0.2, 0.2, 0.3, 0.1,0.2, 
0, 0, 0.3, 0.4, 0.3, 0, 
0, 0, 0, 0.6, 0, 0.4)
trans_P <- matrix(prob_row, 
                  nrow=6, ncol=6, byrow=TRUE)
init_dist <- c(1, 0, 0, 0, 0, 0)
sim_mat <- simulateDistribution(state_space, trans_P, init_dist, 30, 500)
heatmap(sim_mat,Rowv=NA, Colv=NA)
sim_mat