## This study compares predictive methods, conformal prediction, and classical linear models—by evaluating their ability to generate reliable prediction intervals using simulated data.
## 
## The methods include:
##  
## - Linear Model: A standard linear regression model fit using ordinary least squares, with prediction intervals assuming normally distributed residuals.
##
## - Linear Model with Conformal Prediction: A conformal approach applied to the linear model’s predictions, providing intervals that offer coverage guarantees without distributional assumptions.
##
## - Plug-in Method: A method that models the data using a Gaussian distribution, estimating the mean and variance from the training data, and generating symmetric prediction intervals.
##
## - Oracle: Idealized prediction intervals that assume perfect knowledge of the underlying data distribution, used as a benchmark for comparison.
##
## We assess performance as sample sizes change.

library(tidyverse)
library(glmnet)
library(mvtnorm)
library(extraDistr)
library(rsample)

#create synthetic data from a homoscedastic Gaussian distribution
generate_hom_gaussian <- function(n, d, x = NULL) {
  if (is.null(x)) {
    x = matrix(runif(n * d, -5, 5), n, d)
  }
  mean_g <- 5 * x[, 1]
  y = mean_g + rnorm(nrow(x), 0, 1)
  return(list(x = x, y = y, mean = mean_g))
}

#Calculate the lambda path for fitting linear models with glmnet, controlling overfitting with regularization
lambda_path <- function(x, y) {
  sx <- scale(x, scale = apply(x, 2, sd))
  lambda_max <- max(abs(colSums(sx * y))) / length(y)
  lambdapath <- exp(seq(log(lambda_max), log(lambda_max * 0.0001), length.out = 100))
  return(lambdapath)
}


#For conformal prediction, we use the regression-split method 
conformal <- function(fit,xCal,yCal,alpha=0.1)
{
  predictions <- predict(fit,xCal)
  if("pred"%in%names(predictions))
  {
    predictions=predictions$pred 
  }
  residuals <- abs(predictions-yCal)
  output <- list(cutoff=quantile(residuals,probs = 1-alpha),fit=fit)
  class(output) <- "conformal"
  return(output)
}

predict.conformal <- function(fit,xNew)
{
  pred <- predict(fit$fit,xNew)
  if("pred"%in%names(pred))
  {
    pred=pred$pred 
  }
  lower <- pred-fit$cutoff
  upper <- pred+fit$cutoff
  return(list(pred=pred,
              lower=lower,
              upper=upper)
  )
}

#main simulation
run_simulation <- function(seed) {
  set.seed(seed)  # Set the seed
  
  d <- 65
  l <- 10  # prior hyperparameter
  alpha <- 0.1  # 1-alpha is the nominal coverage of the prediction sets
  simulator <- generate_hom_gaussian
  
  n_test <- 1
  data_test <- simulator(n_test, d)
  pred_oracle <- data.frame(pred = data_test$mean,
                            lower = qnorm(alpha / 2, data_test$mean, , data_test$sd),
                            upper = qnorm(1 - alpha / 2, data_test$mean, , data_test$sd))
  
  n_values <- c(100, 200, 500, 1000, 2000, 5000)
  combined_df_all <- tibble()
  
  for (n in n_values) {
    print(n)
    
    data_train_and_val <- simulator(n, d)
    ids <- sample(c("train", "val"), size = n, prob = c(0.7, 0.3), replace = TRUE)
    xTrain <- data_train_and_val$x[ids == "train", ]
    yTrain <- data_train_and_val$y[ids == "train", drop = FALSE]
    xVal <- data_train_and_val$x[ids == "val", ]
    yVal <- data_train_and_val$y[ids == "val", drop = FALSE]
    
    # LM Conformal
    fitted_lm <- glmnet(x = xTrain, y = yTrain, alpha = 0, lambda = 0)
    fitted_lm_conformal <- conformal(fitted_lm, xVal, yVal, alpha = alpha)
    pred_lm_conformal <- predict(fitted_lm_conformal, data_test$x)
    
    # LM Exact predictive
    data <- data.frame(y = data_train_and_val$y, x = data_train_and_val$x)
    fitted_lm_exact <- lm(y ~ ., data = data)
    data_full_test <- data.frame(y = data_test$y, x = data_test$x)
    pred_lm_exact <- predict(fitted_lm_exact, data_full_test, interval = "prediction", level = 1 - alpha) %>%
      as.data.frame()
    colnames(pred_lm_exact) <- c("pred", "lower", "upper")
    
     pred_lm_conformal <- data.frame(
      pred = c(pred_lm_conformal$pred),
      lower = c(pred_lm_conformal$lower),
      upper = c(pred_lm_conformal$upper)
    )
    
    pred_plugin <- data.frame(pred = c(pred_lm_exact$pred),
                              lower = qnorm(alpha / 2, pred_lm_exact$pred, sigma(fitted_lm)),
                              upper = qnorm(1 - alpha / 2, pred_lm_exact$pred, sigma(fitted_lm)))
    
    combined_df <- rbind(
      data.frame(method = "pred_lm_exact", n = n, pred_lm_exact),
      data.frame(method = "pred_lm_conformal_exact", n = n, pred_lm_conformal),
      data.frame(method = "pred_plugin", n = n, pred_plugin),
      data.frame(method = "pred_oracle", n = n, pred_oracle)
    )
    
    combined_df_all <- bind_rows(combined_df_all, combined_df)
  }
  
  combined_df_all$method <- factor(combined_df_all$method,
                                   levels = c("pred_lm_exact",
                                              "pred_lm_conformal_exact",
                                              "pred_plugin",
                                              "pred_oracle"))
  
  custom_colors <- c("#44CA2E", "#FFC107", "#8E24AA", "#000000")
  
  # Plot
  g <- ggplot(combined_df_all, aes(x = method, color = method, y = pred)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1.5,
                  position = position_jitter(width = 0.1)) +
    facet_wrap(~ n, ncol = 2, labeller = labeller(n = function(x) paste0("n = ", x))) +
    labs(x = '', y = '90% Confidence Interval') +
    theme_bw(base_size = 16) +  # Use theme_bw for a clean, grid-based look
    theme(legend.position = "none",  # Remove redundant legend
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank(),  # Remove grey facet boxes
          panel.border = element_rect(color = "black")) +  # Keep black panel border
    scale_x_discrete(labels = c("pred_lm_conformal_exact" = "Linear Model Conformal",
                                "pred_lm_exact" = "Linear Model",
                                "pred_plugin" = "Plug-in",
                                "pred_oracle" = "Oracle")) +  # Rename method labels
    scale_color_manual(values = custom_colors)  # Apply custom colors
  
  print(g)
}

#run simulation
run_simulation(1)

## Quando o tamanho da amostra de treinamento é pequeno, os métodos que levam em conta 
## a incerteza epistêmica – ou seja, os intervalos conformes e o modelo linear – produzem 
## intervalos mais amplos do que o oráculo, refletindo sua capacidade de capturar a 
## incerteza adicional de dados limitados.

## Em contraste, o intervalo de plug-in tenta emular diretamente o oráculo, frequentemente 
## mostrando larguras semelhantes ou até mais estreitas, mas ao custo de cobertura reduzida, 
## caindo abaixo do nível nominal.

## À medida que o tamanho da amostra aumenta, todos os métodos convergem gradualmente 
## para os intervalos de previsão do oráculo.