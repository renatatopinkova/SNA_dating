library(tidyverse)
library(cowplot)
source("04_differences_all_ties.R")


## Histogram figs

make_hist <- function(sim, obs, n) {
  tibble(mean = {{sim}}) |>
    ggplot(aes(x = mean)) + geom_histogram(binwidth = n) + 
    theme_classic() + 
    # scale_x_continuous(limits = c(-0.16, -0.03)) +
    geom_vline(xintercept = {{obs}}, linetype = 2, color = "gray55") +
    labs(x = "Value", y = "Frequency") + 
    theme(axis.title = element_text(size = 8))
}


p1 <- make_hist(sim_women_means, obs_mean_women, n = 0.0001) 
p2 <- make_hist(sim_men_means, obs_mean_men, n = 0.0001)



plot_grid(p1, p2,
          labels = c("a) City1: Women to men", "b) City1: Men to women",
          label_size = 7,
          hjust = -1)

