library(tidyverse)
source("01_matrix_making.R")


# Plotting ----------------------------------------------------------------


tibble(gndr = "men", indeg = colSums(female_matrix_filled)) |> 
  bind_rows(tibble(gndr = "women",indeg = colSums(male_matrix_filled))) |> 
  # placeholder for city - run 01 for both to get objects for plotting
  write_csv("city1.csv")


city1 <- read_csv("city1")


## PLOTTING
bind_rows("a) Brno" = ###PLACEHOLDER CITY1, "b) Prague" = ###PLACEHOLDER CITY1, .id = "id") |> 
  ggplot(aes(indeg, color = gndr)) +
  stat_density(geom="line",position="identity") +
 # theme_classic() + 
  scale_color_manual(values = c("blue", "red")) + 
  labs(x = "Indegree", y = "Density", color = "") + 
  facet_grid(~id) + 
  theme(axis.title = element_text(size = 8),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x.top = element_text(),
        axis.line = element_line(linewidth = 0.3),
        legend.key = element_rect(fill = NA),
        legend.position = c(0.9, 0.95))
                                                   
                                                        