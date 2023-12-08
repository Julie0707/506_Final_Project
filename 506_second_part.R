## The first part

library(pls)

# RMSE calculation function 
rmse <- function(x, y) {
  sqrt(mean((x - y)^2))
}

# MAE calculation function
mae <- function(x, y) {
  mean(abs(x - y))
}

# Function to calculate R-squared
r_squared <- function(actual, predicted) {
  cor(actual, predicted)^2
}

load("/Users/jiaqizhu/Downloads/Real.2.rda")
X <- scale(as.matrix(Real.2[,-1]))  # Optional: normalize X
Y <- as.matrix(Real.2$y)
n <- nrow(X)
p <- ncol(X)
ncomp <- 20

S <- 1000
p_values <- seq(50, 3*p, by = 50)
metrics_train <- matrix(0, ncol = 3, nrow = length(p_values))
metrics_test <- matrix(0, ncol = 3, nrow = length(p_values))
p_n_ratios <- numeric(length(p_values))
colnames(metrics_train) <- colnames(metrics_test) <- c("RMSE", "MAE", "R2")

for (j in seq_along(p_values)) {
  P <- p_values[j]
  rmse_train <- rmse_test <- mae_train <- mae_test <- r2_train <- r2_test <- numeric(S)
  
  for (i in 1:S) {
    N <- matrix(runif(n * P, min = 0, max = 1), nrow = n, ncol = P)
    Z <- cbind(X, N)
    train_indices <- sample(n, round(n * 0.75))
    
    # Split into train and test sets
    Z_train <- Z[train_indices, ]
    Y_train <- Y[train_indices]
    Z_test <- Z[-train_indices, ]
    Y_test <- Y[-train_indices]
    
    # Fit PLS model
    ncomp_adj <- min(min(nrow(Z_train), ncol(Z_train)) - 1, ncomp)
    pls_model <- plsr(Y_train ~ Z_train, ncomp = ncomp_adj)
    
    # Calculate metrics for training data
    predictions_train <- predict(pls_model, newdata = Z_train, ncomp = ncomp_adj)
    rmse_train[i] <- rmse(Y_train, predictions_train)
    mae_train[i] <- mae(Y_train, predictions_train)
    r2_train[i] <- r_squared(Y_train, predictions_train)
    
    # Calculate metrics for test data
    predictions_test <- predict(pls_model, newdata = Z_test, ncomp = ncomp_adj)
    rmse_test[i] <- rmse(Y_test, predictions_test)
    mae_test[i] <- mae(Y_test, predictions_test)
    r2_test[i] <- r_squared(Y_test, predictions_test)
  }
  
  metrics_train[j, ] <- c(mean(rmse_train), mean(mae_train), mean(r2_train))
  metrics_test[j, ] <- c(mean(rmse_test), mean(mae_test), mean(r2_test))
  p_n_ratios[j] <- (P + p) / n
}

# Plotting metrics for Training and Test Data
plot_types <- c("RMSE", "MAE", "R2")
colors <- c("blue", "red", "green")
for (k in 1:length(plot_types)) {
  plot(p_n_ratios, metrics_train[, plot_types[k]], type = "b", col = colors[k], xlab = "P/n ratio", ylab = paste("Mean", plot_types[k]), main = paste("P/n Ratio vs.", plot_types[k], "(Training Data)"))
  legend("topright", legend = paste(plot_types[k], "Training"), col = colors[k], lty = 1)
}

for (k in 1:length(plot_types)) {
  plot(p_n_ratios, metrics_test[, plot_types[k]], type = "b", col = colors[k], xlab = "P/n ratio", ylab = paste("Mean", plot_types[k]), main = paste("P/n Ratio vs.", plot_types[k], "(Test Data)"))
  legend("topright", legend = paste(plot_types[k], "Test"), col = colors[k], lty = 1)
}
