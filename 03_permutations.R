source("01_matrix_making.R")
library(tidyverse)
library(purrr)

# Permutations ------------------------------------------------------------



permute_rows <- function(matrix) {
  # Get the number of rows and columns in the matrix
  # Iterate through each row and permute the entries
  for (i in 1:nrow(matrix)) {
    # Permute the entries in the current row
    matrix[i, ] <- sample(matrix[i, ])
  }
  
  return(matrix)
}


