states_num = 6
transition_P = matrix(runif(states_num*states_num), nrow=states_num, ncol=states_num, byrow=TRUE)
transition_P = transition_P / rowSums(transition_P)
N_fin = 50
states = rep(NA, N_fin)
states[1] = sample(1:states_num)
for (i in 2:N_fin) {
  states[i] = sample(1:states_num, prob=transition_P[states[i - 1],])
}
plot(1:N_fin, states, type="l")
  