#devtools::install_github("rizbicki/FlexCoDE")

library(FlexCoDE)
library(mixtools)
library(np)

generate_data <- function(n_draws, d) {
  x <- matrix(rnorm(n_draws * d, mean = 0, sd = 1), nrow = n_draws, ncol = d)
  y <- rnorm(n_draws, mean = x[,1], sd = 1)
  list(x = x, y = matrix(y, ncol = 1))
}

set.seed(12109)
d <- 5
data_train <- generate_data(1000, d)
data_validation <- generate_data(1000, d)
data_test <- generate_data(1000, d)

##FlexCode approximation
# Fit nearest neighbors FlexCoDE
fit=fitFlexCoDE(data_train$x,data_train$y,data_validation$x,data_validation$y,
                data_test$x,data_test$y,
                nIMax = 20, regressionFunction = regressionFunction.NN,
                regressionFunction.extra=list(nCores=8))
fit$estimatedRisk
print(fit)
plot(fit,data_test$x,data_test$y)

fitb=FlexZBoost(data_train$x,data_train$y,data_validation$x,data_validation$y,
               data_test$x,data_test$y,
               nIMax = 30, regressionFunction.extra=list(nCores=8))
fitb$bestAlpha
fitb$estimatedRisk
print(fitb)
plot(fitb,data_test$x,data_test$y)

#Mixture Gaussian approximation
predict.mixEM <- function(mix, test, addIntercept = TRUE){
  if(addIntercept) test <- cbind(1,test)
  n <- nrow(test)
  y <- rep(0,n)
  for(i in 1:n){
        y[i] <- sum(mix$lambda * (test[i,] %*% mix$beta))
  }
  return(y)
}


dmixEM <- function(mix, test, n=5 ,addIntercept = TRUE){
  for(i in 1:n){
    y[i] <- sum(mix$lambda * dnorm(y[i],mean=test[i,] %*% mix$beta, sd=mix$sigma))
  }
  return(y)
}

mix=regmixEM(as.vector(data_train$y), data_train$x, lambda = NULL, beta = NULL, sigma = NULL, k = 5,
         addintercept = TRUE, arbmean = TRUE, arbvar = TRUE,
         epsilon = 1e-08, maxit = 10000, verb = FALSE)
summary(mix)
yhat <- predict(mix, data_test$x)

## kernel approximation
bw <- npcdensbw(xdat=data_train$x, ydat=data_train$y)
fhat <- npcdens(bws=bw)

summary(fhat)

x11()
plot(bw)
