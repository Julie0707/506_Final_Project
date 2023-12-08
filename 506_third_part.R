## The second part
library(pls)
load("/Users/jiaqizhu/Downloads/Real.2.rda")
dim(Real.2)
any(is.na(Real.2))

Y <- as.matrix(Real.2$y)
X <- as.matrix(Real.2[,-1])

X <- scale(X)
n <- nrow(X)
p <- ncol(X)
ncomp <- 10

p_values <- seq(100, 3*p, by = 100)
p_n_ratios <- numeric(length(p_values))
results_df <- data.frame() # Initialize an empty data frame for results

# Define a threshold for importance
loading_threshold <- 0.12

for (j in seq_along(p_values)) {
  P <- p_values[j] 
  
  # Generate noise matrix N
  N  <- matrix(runif(n*P, min = 0, max = 1), nrow = n, ncol = P)
  # Combine N and X matrix in new matrix Z
  Z  <- cbind(X, N)
  
  # Fit pls model
  Z_dim <- min(nrow(Z), ncol(Z)) -1 
  ncomp_adj <- min(Z_dim, ncomp)
  pls_model <- plsr(Y ~ Z,  ncomp = ncomp_adj)
  
  loadings <- loadings(pls_model)
  variable_names <- rownames(loadings)
  important_variables <- variable_names[apply(loadings, 1, function(x) max(abs(x)) > loading_threshold)]
  p_n_ratios[j] <- (P+p)/n
  
  # Create a temporary data frame for this iteration
  temp_df <- data.frame(p_value = rep(P, length(important_variables)),
                        p_n_ratio = rep(p_n_ratios[j], length(important_variables)),
                        variable_name = important_variables)
  
  # Bind the temporary data frame to the main results data frame
  results_df <- rbind(results_df, temp_df)
}

library(dplyr)

overall_frequency <- results_df %>%
  count(variable_name) %>%
  arrange(desc(n))

# View the table
print(overall_frequency)

library(tidyr)
combined_table <- results_df %>%
  group_by(variable_name) %>%
  summarise(
    frequency = n(),
    p_values = list(unique(p_value))
  )

# View the table
print(combined_table)
