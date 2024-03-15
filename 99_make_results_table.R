# Make results table ------------------------------------------------------

make_results_table <- function(obs_mean, sim_means) {
  sim_mean <- mean(sim_means)
  sim_mean_sd <- sd(sim_means)
  tibble(obs_mean = obs_mean, 
         `Pr(<=obs.value)` = sum(sim_means <= obs_mean)/length(sim_means),
         `Pr(>=obs.value)` = sum(sim_means >= obs_mean)/length(sim_means),
         sim_mean = sim_mean, 
         sd = sim_mean_sd,
  )
}