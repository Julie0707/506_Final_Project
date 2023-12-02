#
rmse <- function(x, y){
  sqrt(mean((x-y)^2))
}

library(pls)
load("/Users/jiaqizhu/Downloads/Real.2.rda")
dim(Real.2)

S <- 100
Y <- as.matrix(Real.2$y)
X <- as.matrix(Real.2[,-1])
# Optional scaling to normalize X
X <- scale(X)
n <- nrow(X)
p <- ncol(X)
ncomp <- 10

RMSE_train <- numeric(S)
RMSE_test <- numeric(S)

for (i in 1:S) {
  # Generate noise matrix N
  N  <- matrix(runif(n*10, min = 0, max = 1), nrow = n, ncol = 10)
  # Combine N and X matrix in new matrix Z
  Z  <- cbind(X, N)
  
  # Split the data set to train data and test data
  train <- sample(n,round(n*0.75)) 
  Y_train <- Y[train,]  
  Z_train <- Z[train, ]
  Y_test <- Y[-train,]
  Z_test <- Z[-train, ]
  
  # Fit pls model
  ncomp <- min(nrow(Z_train),ncomp)
  pls_model <- plsr(Y_train ~ Z_train,  ncomp = ncomp, validation = "CV")
  pls_rmseCV <- RMSEP(pls_model,estimate="CV")
  optimal_ncomp <- which.min(pls_rmseCV$val)

  # RMSE on the training data set
  RMSE_train[i] <- min(pls_rmseCV$val)
  # RMSE on the test data set
  optimal_ncomp <- min(optimal_ncomp,nrow(Z_test))
  predictions <- predict(pls_model, newdata = Z_test, ncomp = optimal_ncomp)
  RMSE_test[i] <- rmse(predictions, Y_test)
  
}

RMSE_train_mean <- mean(RMSE_train)
RMSE_test_mean <- mean(RMSE_test)
RMSE_train_mean
RMSE_test_mean









