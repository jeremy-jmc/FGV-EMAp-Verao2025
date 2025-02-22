library(ggplot2)
library(MASS)
library(BART)

# Set parameters for the simulation
n <- 50  # Number of data points

# Simulate data
set.seed(2) # For reproducibility
which <- (runif(n) > 0.5)
X <- which * rbeta(n, 5, 1) + (1 - which) * rbeta(n, 1, 10)
Y <- sapply(2 * X^2, function(x) rnorm(1, mean = x, sd = 0.2))  # Y ~ N(x, 0.1)

# Prepare data 
X_mat <- matrix(X, ncol = 1)
X_new <- seq(0, 1, length.out = 100)
X_new_mat <- matrix(X_new, ncol = 1)

# Set seed for reproducibility
set.seed(99)

# Fit BART model
post <- wbart(X_mat, Y, X_new_mat, ndpost = 1000)

# Calculate means and prediction intervals
means <- post$yhat.test.mean
std_dev <- sqrt(mean(post$sigma)^2 + apply(post$yhat.test, 2, sd)^2)
lower_bound <- means - 1.96 * std_dev
upper_bound <- means + 1.96 * std_dev

# Plot the original data, fitted regression line, and prediction bands
ggplot() +
  geom_point(aes(x = X, y = Y), colour = "#1E88E5", size = 3) + # Original data
  geom_line(aes(x = X_new, y = means), colour = "#D81B60", linewidth = 2) + # Fitted regression
  geom_ribbon(aes(x = X_new, ymin = lower_bound, ymax = upper_bound), 
              fill = "grey20", alpha = 0.2) + # Prediction bands
  labs(x = "x", y = "y") +
  theme_bw() + 
  theme(text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.position = "top") + 
  coord_cartesian(ylim = c(-1, 3))

#Epsitemic uncertainty
std_dev <- apply(post$yhat.test, 2, sd)
lower_bound <- means - 1.96 * std_dev
upper_bound <- means + 1.96 * std_dev

# Plot the original data, fitted regression line, and new prediction bands
ggplot() +
  geom_point(aes(x = X, y = Y), colour = "#1E88E5", size = 3) + # Original data
  geom_line(aes(x = X_new, y = means), colour = "#D81B60", linewidth = 2) + # Fitted regression
  geom_ribbon(aes(x = X_new, ymin = lower_bound, ymax = upper_bound), 
              fill = "grey20", alpha = 0.2) + # Prediction bands
  labs(x = "x", y = "y") +
  theme_bw() + 
  theme(text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.position = "top") + 
  coord_cartesian(ylim = c(-1, 3))