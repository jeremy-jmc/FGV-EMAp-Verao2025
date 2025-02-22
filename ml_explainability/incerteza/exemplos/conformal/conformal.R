library(FNN)
library(ggplot2)
library(dplyr)

theme_set(theme_bw(base_size = 20)) 

#generate data
set.seed(123)
n <- 500
x <- runif(n)
y <- sin(2 * pi * x) + rnorm(n, sd = 0.1)
data <- data.frame(x = x, y = y)

#separate data
train_indices <- sample(seq_len(n), size = 0.7 * n)
train_data <- data[train_indices, ]
calibration_data <- data[-train_indices, ]

#perform fit
predictions <- FNN::knn.reg(
  train = as.matrix(train_data$x),
  test = as.matrix(calibration_data$x),
  y = train_data$y,
  k = 10
)

#set t 
t <- quantile(abs(predictions$pred - calibration_data$y), probs = 0.95)

#create a grid for x
grid_data <- data.frame(x_grid = seq(min(data$x), max(data$x), length.out = 500))

#do prediction over the grid
grid_data <- grid_data %>%
  mutate(y_pred = FNN::knn.reg(
    train = as.matrix(train_data$x),
    test = as.matrix(grid_data$x_grid),
    y = train_data$y,
    k = 10
  )$pred)

#PLOT
ggplot(data, aes(x = x, y = y)) +
  geom_point(aes(color = "Data"), alpha = 0.5, size = 1.6) +
  geom_line(data = grid_data, aes(x = x_grid, y = y_pred, color = "KNN Prediction"), linewidth = 1) +
  geom_ribbon(data = grid_data, aes(x = x_grid, ymin = y_pred - t, ymax = y_pred + t, 
                                    fill = "Prediction Band (95%)"), alpha = 0.5) +
  theme(strip.background = element_blank(),  # Remove grey facet boxes
        strip.text = element_text(size = 22, face = "bold"),
        panel.spacing.x = unit(4, "lines")) +
  scale_color_manual(name = "", values = c("Data" = "red", "KNN Prediction" = "blue")) +
  scale_fill_manual(name = "", values = c("Prediction Band (95%)" = "lightblue")) +
  labs(x = "X", y = "Y")