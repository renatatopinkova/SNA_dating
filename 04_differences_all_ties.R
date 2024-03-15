
source("01_matrix_making.R")
source("03_permutations.R")
source("99_make_results_table.R")


# Calculate indegree differences ------------------------------------------



# Function that calculates standardized indegree sender - standardized indegree receiver
make_diff <- function(generated, observed) {
  # initiate empty vector
  output_vector <- c()
  for (source in rownames(generated)) {
    # only calculate difference for cells where tie exists
    present_ties <- colnames(generated)[which(generated[source, ] == 1)]
    for (tie in present_ties) {
      # calculate difference between observed indegree sender - generated indegree receiver
      # and put the differences to a vector
      output_vector <- c(
        output_vector,
        # difference between standardized (!) indegrees
        sum((observed[, source])/nrow(observed)) - (sum(generated[, tie]/nrow(generated)))
      )
    }
  }
  return(output_vector)
}


# Get means for simulations -------------------------------------------------


get_means_permutations <- function(to_permute, observed) {
  1:1000 |>
    # make n times permutations
    map(\(i) permute_rows(to_permute)) |> 
    # calculate differences for each permutation
    map(~make_diff(generated = .x, observed = observed)) |> 
    # calculate mean differences for each permutation & unlist to vector
    map_vec(mean) 
}


# Get SIMULATED means per run for men and women
sim_men_means <- get_means_permutations(to_permute = male_matrix_filled, observed = female_matrix_filled)
sim_women_means <- get_means_permutations(to_permute = female_matrix_filled, observed = male_matrix_filled)


# observed value of OBSERVED mean differences
obs_mean_men <- mean(make_diff(generated = male_matrix_filled, observed = female_matrix_filled))
obs_mean_women <- mean(make_diff(generated = female_matrix_filled, observed = male_matrix_filled))


## HISTOGRAMS
make_hist <- function(sim, obs, n) {
  tibble(mean = {{sim}}) |>
    ggplot(aes(x = mean)) + geom_histogram(binwidth = n) + 
    theme_classic() + 
    # scale_x_continuous(limits = c(-0.16, -0.03)) +
    geom_vline(xintercept = {{obs}}, linetype = 2, color = "gray55") +
    labs(x = "Value", y = "Frequency") + 
    theme(axis.title = element_text(size = 8))
}

make_hist(sim_women_means, obs_mean_women, n = 0.01) 
make_hist(sim_men_means, obs_mean_men, n = 0.01)



# Make results table ------------------------------------------------------


make_results_table(obs_mean_men, sim_men_means)
make_results_table(obs_mean_women, sim_women_means)

# Bind results into one table
results_table <- bind_rows(list(
  "men contact women" = make_results_table(obs_mean_men, sim_men_means), 
  "women contact men" = make_results_table(obs_mean_women, sim_women_means)), 
  .id = "id")

