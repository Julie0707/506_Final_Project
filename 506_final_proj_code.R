library(pls)
load("/Users/jiaqizhu/Downloads/Real.2.rda")
dim(Real.2)

S <- 100
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
ncomp <- 10

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
    ncomp <- min((nrow(Z_train)-1),ncomp)
    pls_model <- plsr(Y_train ~ Z_train,  ncomp = ncomp, validation = "CV")
    pls_rmseCV <- RMSEP(pls_model,estimate="CV")
    optimal_ncomp <- which.min(pls_rmseCV$val)
    
    # RMSE on the training data set
    RMSE_train[i] <- min(pls_rmseCV$val)
    # RMSE on the test data set
    optimal_ncomp <- min(optimal_ncomp,(nrow(Z_test)-1))
    predictions <- predict(pls_model, newdata = Z_test, ncomp = optimal_ncomp)
    RMSE_test[i] <- rmse(predictions, Y_test)
    
  }
  
  RMSE_train_result <- mean(RMSE_train)
  RMSE_test_result <- mean(RMSE_test)
  p_n_ratios[j] <- P/n
  
  
}

# Plotting the relationship between P/n ratio and RMSE
plot(p_n_ratios, RMSE_train_means, type = "b", col = "blue", xlab = "P/n ratio", ylab = "Mean RMSE", main = "P/n Ratio vs. RMSE")
points(p_n_ratios, RMSE_test_means, type = "b", col = "red")
legend("topright", legend = c("Training", "Test"), col = c("blue", "red"), lty = 1)








