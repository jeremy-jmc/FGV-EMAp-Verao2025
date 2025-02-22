library("iml")
library("randomForest")
library("ggplot2")

data("Boston", package = "MASS")
head(Boston)

set.seed(42)
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, ntree = 50)

X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

lime.explain <- LocalModel$new(predictor, x.interest = X[1, ])

lime.explain$results

plot(lime.explain)

lime.explain$explain(X[2, ])
plot(lime.explain)


shapley <- Shapley$new(predictor, x.interest = X[1, ], sample.size = 50)
shapley$plot()

shapley$explain(x.interest = X[2, ])
shapley$plot()

results <- shapley$results
head(results)