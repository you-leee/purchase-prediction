library(mfp)
library(splines)
library(kknn)

source("R/utils.R")
source("R/modelFitting.R")

# Loading all required data into global environment for the ensemble model
{
  # Load preprocessed datasets
  customers_2012 <- loadPreprocessedRFM(2012)
  customers_2013 <- loadPreprocessedRFM(2013)
  
  # Load models
  glm_model <- readRDS("data/model/final_glm.rds")
  mfp_model <- readRDS("data/model/final_mfp.rds")
  spline_model <- readRDS("data/model/final_spline.rds")
  kknn_model <- readRDS("data/model/final_kknn.rds")
  
  # Load weights
  ens_weights <- readRDS("data/model/ens_weights.rds")
  
  # Load purchases (for filtering customer groups)
  purchases <- loadPurchases()
}


# Function for testing the next years (2013) revenue of customers, who purchased in 2012
# Arguments:
#   - customer_ids: vector of customer/contact ids
#   - verbose: if TRUE, it prints the RMSE, sum of differences, the percent of it and the predicted revenue
# Value:
#   - data.frame with 3 columns:
#     - "contact_id": the customer ids of the input
#     - "actual", the actual data and 
#     - "fit" with the fitted data
testRevenue <- function(customer_ids, verbose = TRUE) {
  customer_group <- customers_2012 %>%
    filter(contact_id %in% customer_ids)
  
  group_nexty_total_sales <- customer_group$nexty_total_sales
  
  testData_glm <- data.frame(fit = predict(glm_model, customer_group, type = "response"))
  testData_mfp <- data.frame(fit = predict(mfp_model, customer_group, type = "response"))
  testData_spline <- data.frame(fit = predict(spline_model, customer_group))
  testData_kknn <- data.frame(fit = predict(kknn_model, customer_group))
  
  testData_ensemble <- data.frame(contact_id = customer_ids, 
                                  actual = group_nexty_total_sales,
                                  fit = ensembleFun(testData_glm$fit, testData_mfp$fit, testData_spline$fit, testData_kknn$fit,
                                                     weights = ens_weights))
  
  if(verbose) {
    ens_test_RMSE <- calculateRMSE(fitted = testData_ensemble$fit, actual = group_nexty_total_sales)
    ens_test_diff <- sum(testData_ensemble$fit - group_nexty_total_sales)
    ens_test_diff_perc <- ens_test_diff * 100 / sum(group_nexty_total_sales)
    cat(paste0("Predicted revenue: ", sum(testData_ensemble$fit), 
                 "\nRMSE: ", ens_test_RMSE, ", diff: ", ens_test_diff, ", diff(%): ", ens_test_diff_perc))
  }
  
  testData_ensemble
}

# Function for predicting the next years (2014) revenue of customers, who purchased in 2013
# Arguments:
#   - customer_ids: vector of customer/contact ids
#   - verbose: if TRUE, it prints the predicted revenue
# Value:
#   - data.frame with 3 columns:
#     - "contact_id": the customer ids of the input
#     - "fit" with the fitted data
predictRevenue <- function(customer_ids, verbose = TRUE) {
  customer_group <- customers_2013 %>%
    filter(contact_id %in% customer_ids)
  
  predictedData_glm <- data.frame(fit = predict(glm_model, customer_group, type = "response"))
  predictedData_mfp <- data.frame(fit = predict(mfp_model, customer_group, type = "response"))
  predictedData_spline <- data.frame(fit = predict(spline_model, customer_group))
  predictedData_kknn <- data.frame(fit = predict(kknn_model, customer_group))
  
  predictedData_ensemble <- data.frame(customer_ids, 
                                       fit = ensembleFun(predictedData_glm$fit, predictedData_mfp$fit, predictedData_spline$fit, predictedData_kknn$fit,
                                                     weights = ens_weights))
  if(verbose) {
    cat(paste0("Predicted revenue: ", sum(predictedData_ensemble$fit)))
  }
  
  predictedData_ensemble
}

