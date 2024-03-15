source("01_matrix_making.R")
source("03_permutations.R")
source("99_make_results_table.R")

# Reciprocal ties ---------------------------------------------------------


## Reciprocal only matrix


# Function that finds reciprocated ties (1 in matrix1 and 1 in t(matrix2))
find_reciprocal <- function(empty_matrix = male_matrix, 
                            matrix1, matrix2) {
  output_matrix <- empty_matrix
  for (i in rownames(matrix1)) {
    for(j in colnames(matrix1)) {
      output_matrix[i, j] <- ifelse(matrix1[i, j] == 1 & t(matrix2)[i, j] == 1, 1, 0)
    }
  }
  return(output_matrix)
}

combined <- find_reciprocal(matrix1 = male_matrix_filled, matrix2 = female_matrix_filled)

# Reciprocity % calculated
(sum(combined)/(sum(male_matrix_filled)+sum(female_matrix_filled)))*2


## calculate differences where 1 exists
# indegree men
male_indeg <- colSums(female_matrix_filled)/nrow(female_matrix_filled)


# indegree women
women_indegree <- colSums(male_matrix_filled)/nrow(male_matrix_filled)

# calculate 
recip_diff <- c()

for (i in rownames(combined)) {
  present_ties <- colnames(combined)[which(combined[i, ] == 1)]
  for(j in present_ties) {
    recip_diff <- c(recip_diff, 
                    male_indeg[i] - women_indegree[j])
  }
}



length(recip_diff) == sum(combined)

mean(recip_diff)


# 1) Permute both M and F
# 2) Get combined matrix - reciprocal ties
# 3) calculate differences between indegrees 

# permute rows for women
female_sim_matrix <- 1:1000 |>
  # make n times permutations
  map(\(i) permute_rows(female_matrix_filled))


# permute rows for men
male_sim_matrix <-  1:1000 |>
  # make n times permutations
  map(\(i) permute_rows(male_matrix_filled))


# find reciprocal ties in simulated/permuted matrices
combined_sim_matrix <- map(1:1000, 
                           ~find_reciprocal(matrix1 = male_sim_matrix[[.x]], 
                                            matrix2 = female_sim_matrix[[.x]]))

# reciprocity ~2-3% in simulated (vs. 27% in real)
map_vec(1:1000, 
        ~(sum(combined_sim_matrix[[.x]])/(sum(male_sim_matrix[[.x]])+sum(female_sim_matrix[[.x]])))*2)


# get std. indegrees of men and women for each original matric
male_sim_indeg <- map(female_sim_matrix, ~colSums(.x)/nrow(.x))
female_sim_indeg <- map(male_sim_matrix, ~colSums(.x)/nrow(.x))


# function: calculate difference b/w std. indegrees 
calc_diff <- function(matrix, male, female) {
  recip_diff_sim <- c()
  for (i in rownames(matrix)) {
    present_ties <- colnames(matrix)[which(matrix[i, ] == 1)]
    for (j in present_ties) {
      recip_diff_sim <- c(
        recip_diff_sim,
        male[i] - female[j]
      )
    }
  }
  return(recip_diff_sim)
}


# calculate difference 
diff_recip_sim <- map(1:1000, 
                      ~calc_diff(combined_sim_matrix[[.x]], male_sim_indeg[[.x]], female_sim_indeg[[.x]]))

# get means for each iteration, unlist to vector
sim_recip_means <- diff_recip_sim |> map_vec(mean)



# create results table
make_results_table(mean(recip_diff), sim_recip_means)

# make histogram
make_hist(sim_recip_means, mean(recip_diff), n = 0.0005)

