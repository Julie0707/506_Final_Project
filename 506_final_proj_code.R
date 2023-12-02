library(pls)
load("/Users/jiaqizhu/Downloads/Real.2.rda")
dim(Real.2)

S <- 50
Y <- as.matrix(Real.2$y)
X <- as.matrix(Real.2[,-1])

# RMSE calculation function 
rmse <- function(x, y){
  sqrt(mean((x-y)^2))
}

# Optional scaling to normalize X
X <- scale(X)
n <- nrow(X)
p <- ncol(X)
ncomp <- 8

p_values <- seq(40, 2*p, by = 40)
RMSE_train_result <- numeric(length(p_values))
RMSE_test_result <- numeric(length(p_values))
p_n_ratios <- numeric(length(p_values))

for (j in seq_along(p_values)) {
  P <- p_values[j] 
  
  RMSE_train <- numeric(S)
  RMSE_test <- numeric(S)
  
  for (i in 1:S) {
    # Generate noise matrix N
    N  <- matrix(runif(n*P, min = 0, max = 1), nrow = n, ncol = P)
    # Combine N and X matrix in new matrix Z
    Z  <- cbind(X, N)
    
    # Split the data set to train data and test data
    train <- sample(n, round(n*0.75)) 
    Y_train <- Y[train,]  
    Z_train <- Z[train, ]
    Y_test <- Y[-train,]
    Z_test <- Z[-train, ]
    
    # Fit pls model
    Z_train_dim <- min(nrow(Z_train), ncol(Z_train)) -1 
    ncomp_adj <- min(Z_train_dim, ncomp)
    pls_model <- plsr(Y_train ~ Z_train,  ncomp = ncomp_adj)
    # Making predictions on the training data
    predictions_train <- predict(pls_model, newdata = Z_train)
    # RMSE on the training data set
    RMSE_train[i] <- rmse(Y_train, predictions_train)
    # RMSE on the test data set
    Z_test_dim <- min(ncol(Z_test), nrow(Z_test)) - 1
    ncomp_adj2 <- min(Z_test_dim, ncomp)
    # Making predictions on the test data
    predictions <- predict(pls_model, newdata = Z_test, ncomp = ncomp_adj2)
    RMSE_test[i] <- rmse(predictions, Y_test)
    
  }
  
  RMSE_train_result[j] <- mean(RMSE_train)
  RMSE_test_result[j] <- mean(RMSE_test)
  p_n_ratios[j] <- (P+p)/n
  
}

# Plotting the relationship between P/n ratio and RMSE
# Plot for Training Data RMSE
plot(p_n_ratios, RMSE_train_result, type = "b", col = "blue", xlab = "P/n ratio", ylab = "Mean RMSE", main = "P/n Ratio vs. RMSE (Training Data)")
legend("topright", legend = c("Training RMSE"), col = c("blue"), lty = 1)

# Plot for Test Data RMSE
plot(p_n_ratios, RMSE_test_result, type = "b", col = "red", xlab = "P/n ratio", ylab = "Mean RMSE", main = "P/n Ratio vs. RMSE (Test Data)")
legend("topright", legend = c("Test RMSE"), col = c("red"), lty = 1)


