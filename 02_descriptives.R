source("01_matrix_making.R")



# Descriptives ------------------------------------------------------------


## Density
density_dirBip <- function(men, women) {
  edges <- sum(men)+sum(women)
  density <- edges / (nrow(men) * nrow(women) * 2)
  return(density)
}


density_dirBip(male_matrix_filled, female_matrix_filled)


## Centralization

degcent_dirBip <- function(matrix) {
  star <- c(nrow(matrix), replicate(ncol(matrix) - 1, 0))
  degcentdirbipar <- sum((max(colSums(matrix)) - colSums(matrix)))/sum((max(star) - star))
  return(degcentdirbipar)
}

# MEN
degcent_dirBip(male_matrix_filled)
degcent_dirBip(female_matrix_filled)


# INDEGREE
## WOMEN
mean(unname(colSums(male_matrix_filled)))
sd(unname(colSums(male_matrix_filled)))

## MEN
mean(unname(colSums(female_matrix_filled)))
sd(unname(colSums(female_matrix_filled)))

## FULL NETWORK
mean(unname(c(colSums(female_matrix_filled), colSums(male_matrix_filled))))
sd(unname(c(colSums(female_matrix_filled), colSums(male_matrix_filled))))

# OUTDEGREE
## WOMEN
mean(unname(rowSums(female_matrix_filled)))
sd(unname(rowSums(female_matrix_filled)))

## MEN
mean(unname(rowSums(male_matrix_filled)))
sd(unname(rowSums(male_matrix_filled)))

## FULL NETWORK
mean(unname(c(rowSums(female_matrix_filled), rowSums(male_matrix_filled))))
sd(unname(c(rowSums(female_matrix_filled), rowSums(male_matrix_filled))))


