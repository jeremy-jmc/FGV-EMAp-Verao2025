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

##Importância do Input
imp = FeatureImp$new(predictor, loss = 'mae')
plot(imp)
imp$results

##PDP
pdp <- FeatureEffect$new(predictor, feature = "lstat", grid.size = 10, method = "pdp")
pdp$plot()

##ALE
ale <- FeatureEffect$new(predictor, feature = "lstat", grid.size = 10, method = "ale")
ale$plot()

ale$set.feature("rm")
ale$plot()

##Modelo Substituto
tree <- TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)