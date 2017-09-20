library(lubridate)
library(gvlma)
library(pastecs)
library(earth)
library(gbm)
library(plyr)

source("R/utils.R")
source("R/RFM.R")
source("R/customerClustering.R")
source("R/modelFitting.R")
source("R/featureScaling.R")

# Load purchases
purchases <- loadPurchases()
# Remove negative values (for easier handling of data for transformations.. etc)
purchases <- purchases %>%
  filter(sales_amount >= 0 | quantity >= 0)

### Customer features
# We will use RFM values for describing the customers
  # Recency: How long ago was last purchase? (days): days_since_last_purchase
  # Frequency: How many orders in analysis period?: order_num
  # Monetary: What is total $ value of all orders in analysis period?: total_sales
  # Breadth: How many different products purchased?: product_num
  # Tenure: How long has a customer been with us? (days since first purchase): active_days
customer_rfm <- calculateRFMs(purchases)
summary(customer_rfm)
  
# Plot customer RFMs
plotRfms(customer_rfm, c(104, 300, 300, 300, 600))


### Customer RFMs with next years total sales amount
# Since we want to predict next years total revenue, we will add it to the features of a customer, so we will only have the 2012
# subset of the purchases for training
purchases_2012 <- purchases %>%
  filter(year(purchase_date) == 2012)
purchases_2013 <- purchases %>%
  filter(year(purchase_date) == 2013)


# We can see, that except the recency, all the features are severely skewed, but for the first try, we won't scale, nor transform them
customer_rfm_2012 <- calculateRFMs(purchases_2012)
customer_rfm_2013 <- calculateRFMs(purchases_2013)
plotRfms(customer_rfm_2012, c(100, 100, 100, 100, 100), title = "2012 customers")


# Customer data of spendings in 2012
# We only want to predict the next years revenue and for that we woudn't need the customers, who didn't spent any money next year (0 amount does not change the result in any way)
# on the other hand, how can we know, what the model will predict for these customers, if we didn't included them in the training set?
# So we will consider all customers, not just, who stayed active in 2013 too 
customer_total_sales_2013 <- calculateTotalSales(purchases_2013) %>% 
  rename(nexty_total_sales = total_sales)
all_customers_2012 <- left_join(customer_rfm_2012, customer_total_sales_2013, by = "contact_id") %>% 
  mutate_each(funs(replace(., which(is.na(.)), 0)))
summary(all_customers_2012)

### Correlations
# We see, that the next years revenue is highly (positively) correlated with all the features, except the recency and tenure
cors_all <- cor(all_customers_2012[, -1])
cors_all

# On the other hand order_num, product_num and total_sales are also correlated with each other
# We won't remove them in the first run, since we have a relative small set of features compared to the training data
cors_features <- cor(all_customers_2012[, -c(1, 7)])
cors_features
findCorrelation(cors_features, cutoff = 0.75)

### Zero covariates, we can see, that the tenure is a near zero variance predictor
# We won't remove it for the first run for the same reasons as above
nearZeroVar(all_customers_2012[, -c(1, 7)], saveMetrics=TRUE)


### Relashioship between features and target. 
# Between order_num, product_num and total_sales(for both year), there is an approximatelly linear relationship, but with a wide range, especially near 0
pairs(all_customers_2012[, -1])


### Model fitting
## Predict the spendings of customers in 2013 based on training set from 2012
## For the first run, we won't scale or clean the features and use them all

## Create train and test set
set.seed(3456)
trainIndex <- createDataPartition(all_customers_2012$nexty_total_sales, p = .8, 
                                  list = FALSE, 
                                  times = 1)

customer_purchase_train <- all_customers_2012[ trainIndex,]
customer_purchase_test  <- all_customers_2012[-trainIndex,]


## Try some models
target_param = "nexty_total_sales"

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5)

# Boosting with trees
gbm_parameters <- ModelTrainingParameters(method = "gbm", ctrl = ctrl, tuning_grid = NULL)

# (Simple) Linear regression
lm_parameters <- ModelTrainingParameters(method = "lm", ctrl = ctrl, tuning_grid = NULL)

# Multivariate adaptive regression splines
mars_parameters <- ModelTrainingParameters(method = "earth", ctrl = ctrl, tuning_grid = NULL)


model_params <- list(ModelTrainingParameters())
model_params[[1]] <- lm_parameters
model_params[[2]] <- mars_parameters
model_params[[3]] <- gbm_parameters

fits <- trainMore(customer_purchase_train[, -c(1)], target_param, model_params)

summary(lm_fit <- fits[[1]])
summary(mars_fit <- fits[[2]])
summary(gbm_fit <- fits[[3]], plotit = F)

results <- resamples(list(MARS = mars_fit,
                          LR = lm_fit,
                          GBM = gbm_fit))

summary(results)


# Importance of features
# We can see
lm_importance <- varImp(lm_fit, scale=FALSE)
lm_importance

mars_importance <- varImp(mars_fit, scale=FALSE)
mars_importance

gbm_importance <- varImp(gbm_fit, scale=FALSE)
gbm_importance



### Test 
# Boosting with trees
gbm_pred <- testNextYearsSales(gbm_fit, customer_purchase_test)

print(gbm_RMSE <- gbm_pred$gbm.RMSE)
print(gbm_total_diff <- sum(gbm_pred$gbm.prediction - customer_purchase_test$nexty_total_sales))
print(gbm_total_diff_perc <- gbm_total_diff * 100 / sum(customer_purchase_test$nexty_total_sales))


# (Simple) Linear regression
lm_pred <- testNextYearsSales(lm_fit, customer_purchase_test)

print(lm_RMSE <- lm_pred$lm.RMSE)
print(lm_total_diff <- sum(lm_pred$lm.prediction - customer_purchase_test$nexty_total_sales))
print(lm_total_diff_perc <- lm_total_diff * 100 / sum(customer_purchase_test$nexty_total_sales))

# Mars
mars_pred <- testNextYearsSales(mars_fit, customer_purchase_test)

print(mars_RMSE <- mars_pred$earth.RMSE)
print(mars_total_diff <- sum(mars_pred$earth.prediction - customer_purchase_test$nexty_total_sales))
print(mars_total_diff_perc <- mars_total_diff * 100 / sum(customer_purchase_test$nexty_total_sales))


### Train for whole 2012 dataset and test customer spendings for customers with only 1 order and for who purchased in december 
final_fits <- trainMore(all_customers_2012[, -c(1)], target_param, model_params)

final_lm_fit <- final_fits[[1]]
summary(final_lm_fit)

final_mars_fit <- final_fits[[2]]
summary(final_mars_fit)

final_gbm_fit <- final_fits[[3]]
summary(final_gbm_fit)


dec_customer_ids <- purchases %>%
  filter(year(purchase_date) == 2012 & month(purchase_date)  ==  12) %>%
  dplyr::select(contact_id) %>%
  unique

dec_customers <- all_customers_2012 %>%
  filter(contact_id %in% dec_customer_ids$contact_id)

onetime_customers <- all_customers_2012 %>%
  filter(order_num == 1)

# Gbm
gbm_pred_dec <- testNextYearsSales(final_gbm_fit, dec_customers)
print(gbm_dec_RMSE <- gbm_pred_dec$gbm.RMSE)
print(gbm_dec_total_diff <- sum(gbm_pred_dec$gbm.prediction - dec_customers$nexty_total_sales))
print(gbm_dec_total_diff_perc <- gbm_dec_total_diff * 100 / sum(dec_customers$nexty_total_sales))

gbm_pred_onetime <- testNextYearsSales(final_gbm_fit, onetime_customers)
print(gbm_onetime_RMSE <- gbm_pred_onetime$gbm.RMSE)
print(gbm_onetime_total_diff <- sum(gbm_pred_onetime$gbm.prediction - onetime_customers$nexty_total_sales))
print(gbm_onetime_total_diff_perc <- gbm_onetime_total_diff * 100 / sum(onetime_customers$nexty_total_sales))


# Lm
lm_pred_dec <- testNextYearsSales(final_lm_fit, dec_customers)
print(lm_dec_RMSE <- lm_pred_dec$lm.RMSE)
print(lm_dec_total_diff <- sum(lm_pred_dec$lm.prediction - dec_customers$nexty_total_sales))
print(lm_dec_total_diff_perc <- lm_dec_total_diff * 100 / sum(dec_customers$nexty_total_sales))

lm_pred_onetime <- testNextYearsSales(final_lm_fit, onetime_customers)
print(lm_onetime_RMSE <- lm_pred_onetime$lm.RMSE)
print(lm_onetime_total_diff <- sum(lm_pred_onetime$lm.prediction - onetime_customers$nexty_total_sales))
print(lm_onetime_total_diff_perc <- lm_onetime_total_diff * 100 / sum(onetime_customers$nexty_total_sales))

# Mars
mars_pred_dec <- testNextYearsSales(final_mars_fit, dec_customers)
print(mars_dec_RMSE <- mars_pred_dec$earth.RMSE)
print(mars_dec_total_diff <- sum(mars_pred_dec$earth.prediction - dec_customers$nexty_total_sales))
print(mars_dec_total_diff_perc <- mars_dec_total_diff * 100 / sum(dec_customers$nexty_total_sales))

mars_pred_onetime <- testNextYearsSales(final_mars_fit, onetime_customers)
print(mars_onetime_RMSE <- mars_pred_onetime$earth.RMSE)
print(mars_onetime_total_diff <- sum(mars_pred_onetime$earth.prediction - onetime_customers$nexty_total_sales))
print(mars_onetime_total_diff_perc <- mars_onetime_total_diff * 100 / sum(onetime_customers$nexty_total_sales))



### We can see, that non of the models is precise enough
#   1.) Do we need to separate customers?
#     - Maybe group customers by the RFM features to get a more precise prediction and train each group independently
#   2.) Scale data and/or remove/add some features and try to fit again
#   3.) Do we need an extra model, separate from customer features?
#     - Unfortunatelly forecasting won't work in this case, because seasonability is mainly in yearly data and 
#       we only have 2 years of data (2 periods), so can't test it.


### Customer grouping / Clustering with scaled data: Does adding cluster groups help?

# Let's log transform first to reduce skewness
all_customers_2012_scaled <- cbind(contact_id = all_customers_2012[, 1], logTransformation(all_customers_2012[, -1], exp(1), 1))

# Let's see again the RFMs, but for the scaled customer data
# We can see, that the total_sales_amount is appr. gaussian.. so should the nexty_total_sales be,
# so maybe we can try glm fitting too (later)
summary(all_customers_2012_scaled[, c(-1, -7)])
plotRfms(all_customers_2012_scaled, c(200, 200, 200, 200, 200, 200), "rfmtbn")

# Determine number of cluster
set.seed(25)

# From the plot, we can see, that the "elbow" is at 9-11, but there was a break at 2 and 3 clusters too
plotSoSByClusterNum(all_customers_2012_scaled[, c(-1, -7)])


# If we fit 9-11 groups, we maybe won't have enough data for training each group
# From the 2 cluster groups, we can see, that 
#   There is one group, which has high total_sales, active_days and order_num and a low days_since_last_purchase
#     In other words it has high monetary, tenure, frequency and high recency, so these are the "best customers"
#   There is one group with every RFM being much lower, except the recency, these can be maybe the one-time customers
customer_groups <- fitCluster(all_customers_2012_scaled[, c(-1, -7)], 2)


# Add grouping to customer data and separate them by cluster group
all_customers_2012_grouped <- data.frame(all_customers_2012_scaled, cluster = as.factor(customer_groups$cluster))
best_2012 <- all_customers_2012_grouped %>% 
  filter(cluster == "1") %>%
  dplyr::select(-cluster)
plotRfms(best_2012, c(200, 200, 200, 200, 200, 200), "rfmtbn", title = "Best customers")

other_2012 <- all_customers_2012_grouped %>% 
  filter(cluster == "2") %>%
  dplyr::select(-cluster)
plotRfms(other_2012, c(200, 200, 200, 200, 200, 200), "rfmtbn", title = "Other customers")


# Train and test each group (we will calculate RMSE with rescaled data)
# Best customers
best_2012_outcome <- trainAndTestMore(model_params, "nexty_total_sales", best_2012[, -1])

best_2012_pred_lm <- reverseLogTransformation(best_2012_outcome$predictions[[1]]$lm.prediction, exp(1), 1)$data
best_2012_pred_mars <- reverseLogTransformation(best_2012_outcome$predictions[[2]]$earth.prediction[, 1], exp(1), 1)$data
best_2012_pred_gbm <- reverseLogTransformation(best_2012_outcome$predictions[[3]]$gbm.prediction, exp(1), 1)$data
best_2012_actual <- reverseLogTransformation(data.frame(best_2012_outcome$actual), exp(1), 1)

# Lr
print(best_2012_RMSE_lm <- calculateRMSE(best_2012_pred_lm, best_2012_actual))
print(best_2012_pred_diff_lm <- sum(best_2012_pred_lm - best_2012_actual))
print(best_2012_pred_diff_perc_lm <- best_2012_pred_diff_lm * 100 / sum(best_2012_actual))

# Gbm
print(best_2012_RMSE_gbm <- calculateRMSE(best_2012_pred_gbm, best_2012_actual))
print(best_2012_pred_diff_gbm <- sum(best_2012_pred_gbm - best_2012_actual))
print(best_2012_pred_diff_perc_gbm <- best_2012_pred_diff_gbm * 100 / sum(best_2012_actual))

# Mars
print(best_2012_RMSE_mars <- calculateRMSE(best_2012_pred_mars, best_2012_actual))
print(best_2012_pred_diff_mars <- sum(best_2012_pred_mars - best_2012_actual))
print(best_2012_pred_diff_perc_mars <- best_2012_pred_diff_mars * 100 / sum(best_2012_actual))



# Other customers
other_2012_outcome <- trainAndTestMore(model_params, "nexty_total_sales", other_2012[, -1])

other_2012_pred_lm <- reverseLogTransformation(other_2012_outcome$predictions[[1]]$lm.prediction, exp(1), 1)$data
other_2012_pred_mars <- reverseLogTransformation(other_2012_outcome$predictions[[2]]$earth.prediction[, 1], exp(1), 1)$data
other_2012_pred_gbm <- reverseLogTransformation(other_2012_outcome$predictions[[3]]$gbm.prediction, exp(1), 1)$data
other_2012_actual <- reverseLogTransformation(data.frame(other_2012_outcome$actual), exp(1), 1)

# Lr
print(other_2012_RMSE_lm <- calculateRMSE(other_2012_pred_lm, other_2012_actual))
print(other_2012_pred_diff_lm <- sum(other_2012_pred_lm - other_2012_actual))
print(other_2012_pred_diff_perc_lm <- other_2012_pred_diff_lm * 100 / sum(other_2012_actual))

# Gbm
print(other_2012_RMSE_gbm <- calculateRMSE(other_2012_pred_gbm, other_2012_actual))
print(other_2012_pred_diff_gbm <- sum(other_2012_pred_gbm - other_2012_actual))
print(other_2012_pred_diff_perc_gbm <- other_2012_pred_diff_gbm * 100 / sum(other_2012_actual))

# Mars
print(other_2012_RMSE_mars <- calculateRMSE(other_2012_pred_mars, other_2012_actual))
print(other_2012_pred_diff_mars <- sum(other_2012_pred_mars - other_2012_actual))
print(other_2012_pred_diff_perc_mars <- other_2012_pred_diff_mars * 100 / sum(other_2012_actual))


## Customer grouping and scaling didn't help much, but we could see, that order_num, product_num and total_sales are
# appr. linear, and the log transformation helped the skewness of total_sales, so a gaussian linear model with log link could be a good choise

# Let's save the customer RFMs by year and the model and test results for later
write.csv(customer_rfm_2012, "data/RFM_2012.csv", row.names = F, quote = F)
write.csv(customer_rfm_2013, "data/RFM_2013.csv", row.names = F, quote = F)

# Save training and test results
lm_results <- createModelSummary(description = "Linear regression, no scale, no transform, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                 test_RMSE = lm_RMSE,
                                 test_diff = lm_total_diff,
                                 test_diff_percent = lm_total_diff_perc,
                                 dec_test_RMSE = lm_dec_RMSE,
                                 dec_test_diff = lm_dec_total_diff,
                                 dec_test_diff_percent = lm_dec_total_diff_perc,
                                 onetime_test_RMSE = lm_onetime_RMSE,
                                 onetime_test_diff = lm_onetime_total_diff,
                                 onetime_test_diff_percent = lm_onetime_total_diff_perc)
saveRDS(lm_results, file = "data/results/lm.rds")

gbm_results <- createModelSummary(description = "Gradient boosting, no scale, no transform, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                 test_RMSE = gbm_RMSE,
                                 test_diff = gbm_total_diff,
                                 test_diff_percent = gbm_total_diff_perc,
                                 dec_test_RMSE = gbm_dec_RMSE,
                                 dec_test_diff = gbm_dec_total_diff,
                                 dec_test_diff_percent = gbm_dec_total_diff_perc,
                                 onetime_test_RMSE = gbm_onetime_RMSE,
                                 onetime_test_diff = gbm_onetime_total_diff,
                                 onetime_test_diff_percent = gbm_onetime_total_diff_perc)
saveRDS(gbm_results, file = "data/results/gbm.rds")

mars_results <- createModelSummary(description = "Multivariet adaptive regression spline, no scale, no transform, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                  test_RMSE = mars_dec_RMSE,
                                  test_diff = mars_total_diff,
                                  test_diff_percent = mars_total_diff_perc,
                                  dec_test_RMSE = mars_dec_RMSE,
                                  dec_test_diff = mars_dec_total_diff,
                                  dec_test_diff_percent = mars_dec_total_diff_perc,
                                  onetime_test_RMSE = mars_onetime_RMSE,
                                  onetime_test_diff = mars_onetime_total_diff,
                                  onetime_test_diff_percent = mars_onetime_total_diff_perc)
saveRDS(mars_results, file = "data/results/mars.rds")

bestc_lm_results <- createModelSummary(description = "Linear regression for \"best customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                      test_RMSE = best_2012_RMSE_lm,
                                      test_diff = best_2012_pred_diff_lm,
                                      test_diff_percent = best_2012_pred_diff_perc_lm)
saveRDS(bestc_lm_results, file = "data/results/bestc_lm.rds")

bestc_gbm_results <- createModelSummary(description = "Gradient boosting for \"best customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                       test_RMSE = best_2012_RMSE_gbm,
                                       test_diff = best_2012_pred_diff_gbm,
                                       test_diff_percent = best_2012_pred_diff_perc_gbm)
saveRDS(bestc_gbm_results, file = "data/results/bestc_gbm.rds")

bestc_mars_results <- createModelSummary(description = "Multivariet adaptive regression spline for \"best customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                       test_RMSE = best_2012_RMSE_mars,
                                       test_diff = best_2012_pred_diff_mars,
                                       test_diff_percent = best_2012_pred_diff_perc_mars)
saveRDS(bestc_mars_results, file = "data/results/bestc_mars.rds")

otherc_lm_results <- createModelSummary(description = "Linear regression for \"other customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                       test_RMSE = other_2012_RMSE_lm,
                                       test_diff = other_2012_pred_diff_lm,
                                       test_diff_percent = other_2012_pred_diff_perc_lm)
saveRDS(otherc_lm_results, file = "data/results/otherc_lm.rds")

otherc_gbm_results <- createModelSummary(description = "Gradient boosting for \"other customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                        test_RMSE = other_2012_RMSE_gbm,
                                        test_diff = other_2012_pred_diff_gbm,
                                        test_diff_percent = other_2012_pred_diff_perc_gbm)
saveRDS(otherc_gbm_results, file = "data/results/otherc_gbm.rds")

otherc_mars_results <- createModelSummary(description = "Multivariet adaptive regression spline for \"other customers\", no scale, log transformation for both dependent and independent variables, cross validated, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                         test_RMSE = other_2012_RMSE_mars,
                                         test_diff = other_2012_pred_diff_mars,
                                         test_diff_percent = other_2012_pred_diff_perc_mars)
saveRDS(otherc_mars_results, file = "data/results/otherc_mars.rds")
