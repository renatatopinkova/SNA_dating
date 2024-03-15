library(tidyverse)
library(purrr)



# Toy example to run code with --------------------------------------------

female_vector <- c("a", "b", "c")
male_vector <- c("d", "e", "f", "g")


# REAL DATA
female_senders = tibble(source = c("a", "b", "b", "c", "c"),
                        target = c("d", "d", "e", "f", "g"))


male_senders = tibble(source = c("d", "d", "e", "f", "f", "g"),
                      target = c("a", "b", "c", "a", "c", "b"))





# Create empty matrices ---------------------------------------------------


# Create empty matrices (0) of desired lengths
male_matrix <- matrix(c(rep(0, length(male_vector)*length(female_vector))), 
                      nrow = length(male_vector), 
                      ncol = length(female_vector), 
                      dimnames = list(male_vector, female_vector),
                      byrow = TRUE)

female_matrix <- t(male_matrix)





# Fill in matrices with observed values -----------------------------------

# function to fill in the matrix with real values
fill_in_matrix <- function(vector_names, dataframe, blank_matrix) {
  
  output_matrix <- blank_matrix
  
  for (sender in vector_names) {
    ties <- dataframe |> filter(source == sender) |> pull(target)
    for (receiver in ties) {
      output_matrix[sender, receiver] <- 1
    }
  }
  return(output_matrix)
}



# MALE MATRIX (M -> F)
male_matrix_filled <- fill_in_matrix(vector_names = male_vector, 
                                     dataframe = male_senders, 
                                     blank_matrix = male_matrix)


# FEMALE MATRIX (F -> M)
female_matrix_filled <- fill_in_matrix(vector_names = female_vector, 
                                       dataframe = female_senders, 
                                       blank_matrix = female_matrix)



