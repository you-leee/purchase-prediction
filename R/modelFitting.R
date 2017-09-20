library(caret)
library(doParallel)


setClassUnion("DfOrNULL", c("data.frame", "NULL"))

ModelTrainingParameters <- setClass("ModelTrainingParameters",
         representation = list(
           method = "character",
           ctrl = "list",
           tuning_grid = "DfOrNULL"
         ),
         prototype=prototype(method = "lm", 
                             ctrl = trainControl(method = "repeatedcv", 
                                                 number = 10, 
                                                 repeats = 5), 
                             tuning_grid=NULL))


# Train
trainOne <- function(model_training_parameter, train_set, target_feature) {
  model_formula <- as.formula(paste(target_feature , "~ ."))
  trainOneWithFormula(model_training_parameter, train_set, model_formula)
}

trainOneWithFormula <- function(model_training_parameter, train_set, formula) {
  # Parallel computation
  cl <- makePSOCKcluster(max(1, detectCores() - 1))
  registerDoParallel(cl)
  
  set.seed(32)
  cat( "Fitting model: ", model_training_parameter@method, " ... ")
  
  fit <- train(
    form = formula,
    data = train_set,
    method = model_training_parameter@method,
    trControl = model_training_parameter@ctrl,
    tuneGrid = model_training_parameter@tuning_grid
  )
  
  registerDoSEQ()
  stopCluster(cl)
  
  fit
}

trainMore <- function(train_set, target_feature, model_training_parameters) {
  fits <- lapply(X = model_training_parameters, FUN = trainOne, train_set, target_feature)
}


# Test prediction
testNextYearsSales <- function(model, test_set) {
  predicted_data <- predictNextYearsSales(model, test_set)
  
  pred_MSE <- mean((predicted_data - test_set$nexty_total_sales) ^ 2)
  pred_RMSE <- sqrt(pred_MSE)
  cat("\nMethod:", model$method, "\nPrediction MSE:", pred_MSE, ", prediciton RMSE:", pred_RMSE)
  
  test_predictions <- list(predicted_data = predicted_data, RMSE = pred_RMSE)
  names(test_predictions) <- c(paste0(model$method, ".prediction"), paste0(model$method, ".RMSE"))
  
  test_predictions
}

# Predict
predictNextYearsSales <- function(model, new_dataset) {
  predict(model, newdata = new_dataset)
}

# Train and test
trainAndTestOne <- function(model_training_parameter, target_feature, dataset, trainPerc = .8) {
  set.seed(3456)

  trainIndex <- createDataPartition(dataset[[target_feature]], 
                                    p = trainPerc, 
                                    list = FALSE, 
                                    times = 1)
  
  train_set <- dataset[ trainIndex,]
  test_set  <- dataset[-trainIndex,]
  
  fit <- trainOne(model_training_parameter, train_set, target_feature)
  predicted_data <- testNextYearsSales(fit, test_set)
  
  data.frame(predicted = predicted_data$predicted_data, actual = test_set[[target_feature]], MSE = predicted_data$MSE)
}

trainAndTestMore <- function(model_training_parameters, target_feature, dataset, trainPerc = .8) {
  set.seed(3456)
  
  trainIndex <- createDataPartition(dataset[[target_feature]], 
                                    p = trainPerc, 
                                    list = FALSE, 
                                    times = 1)
  
  train_set <- dataset[ trainIndex,]
  test_set  <- dataset[-trainIndex,]
  
  fits <- trainMore(train_set, target_feature, model_training_parameters)
  predictions <- lapply(fits, testNextYearsSales, test_set)
  
  list(predictions = predictions, actual = test_set[[target_feature]], fits = fits)
}

# Measures
calculateDiffFromModel <- function(fitted_values) {
  sum(fitted_values$fit - fitted_values$nexty_total_sales)
}

calculateRMSEFromFit <- function(fitted_values) {
  calculateRMSE(fitted_values$fit, fitted_values$nexty_total_sales)
}

calculateRMSE <- function(fitted, actual) {
  sqrt(mean((fitted - actual) ^ 2))
}

# Ensemble optimising
optimiseEnsemble <- function(fitted1, fitted2, fitted3, fitted4, actual, weights = rep(1,4)) {
  f <- function(weights, fitted1, fitted2, fitted3, fitted4, actual) {
    fitted_ensebmle <- ensembleFun(fitted1, fitted2, fitted3, fitted4, weights)
    calculateRMSE(fitted_ensebmle, actual)
  }
  
  fitted_weights <- optim(weights, f, NULL, fitted1, fitted2, fitted3, fitted4, actual, 
                          method= "L-BFGS-B", lower = rep(0.5, 4), upper = rep(1.0, 4), 
                          control = list(factr = 1e15, ndeps = rep(1e-5, 4)))
  fitted_weights
}

ensembleFun <- function(fitted1, fitted2, fitted3, fitted4, weights) {
  fitted_ensebmle <- (fitted1*weights[1] + fitted2*weights[2] + fitted3*weights[3] + fitted4*weights[4]) / sum(weights)
  fitted_ensebmle
}
