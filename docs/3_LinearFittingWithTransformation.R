library(car)
library(MASS)
library(fitdistrplus)

source("R/utils.R")
source("R/RFM.R")
source("R/featureScaling.R")
source("R/modelFitting.R")

### Gaussian fitting with log link and Box-Tidwell transformation for the features
# In the customer fitting exploration, we could see, that after the log transformation, total_sales looked approximatelly gaussian,
# so we will try to fit a gaussian, linear model with a log link

# Load customer rfm values
customer_rfm_2012 <- loadRFM(2012)
customer_rfm_2013 <- loadRFM(2013)

# Customer data, of spendings in 2012
# To prevent zero-inflation in nexty_total_sales, we only use customers, who also purchased in 2013 for this model
customer_total_sales_2013 <- customer_rfm_2013 %>% 
  dplyr::select(contact_id, total_sales) %>%
  rename(nexty_total_sales = total_sales)

customers_2012 <- inner_join(customer_rfm_2012, customer_total_sales_2013, by = "contact_id")
summary(customers_2012)

# We can see, that except recency, the data is highly skewed
pairs(customers_2012[, -1])
plotRfms(customers_2012, c(100, 100, 100, 100, 100, 100), "rfmbn", "Customers 2012")

# If we want to fit to a linear model,
# First we transform next years revenue and see, what distribution it fits
# After that, we transform the predictors to have a linear relashionship

# Transform dependent variable
# Transformation overview
par(mfrow = c(3,2))
hist(customers_2012$nexty_total_sales, main = "Untransformed")
hist(log(customers_2012$nexty_total_sales), main = "Log")
hist(sqrt(customers_2012$nexty_total_sales), main = "y^1/2")
hist((customers_2012$nexty_total_sales)^0.3, main = "y^0.3")
hist((customers_2012$nexty_total_sales)^-0.5, main = "1/y^1/2")
hist((customers_2012$nexty_total_sales)^-1.5, main = "1/y^1.5")
par(mfrow = c(1,1))

# We can see, that after log transformations (and after some others too, but not that clearly), the distribution got
# closer to normal
# Let's test that
log_customers_nexty_ts <- log(customers_2012$nexty_total_sales)
descdist(log_customers_nexty_ts, boot = 200)

# Weibull and normal seems to be a good fit
norm_fit <- fitdist(log_customers_nexty_ts, "norm")
weibull_fit <- fitdist(log_customers_nexty_ts, "weibull")
# Normal fit
plot(norm_fit)
summary(norm_fit)
# Weibull fit
plot(weibull_fit)
summary(weibull_fit)

# Normal distributions seems to fit a little bit better, so we will fit for normal distribution with log link function (won't transform the whole response)
set.seed(32)
summary(l_model <- glm(nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days,
                       data = customers_2012,
                       family = gaussian(link = "log")))

boxCox(l_model, lambda = seq(-1, 2, 1/10), plotit = TRUE)

# As it turns out, for this model, the best lambda is around 0.5
nexty_lambda = 0.5
customers_2012_dep_transformed <- data.frame(customers_2012[, -c(1, 7)], nexty_total_sales = bcPower(customers_2012$nexty_total_sales, nexty_lambda))


# Transform independent variables
# We can see from the pairs, that order_num, product_num and total_sales are somewhat linear, but really centered, 
# active_days needs probably a < 1 power and days_since_last_purchase needs probably also a power < 1
pairs(customers_2012_dep_transformed)

# Let's calculate lambdas with the Box-Tidweel transformation
# In order to do that, we add 1 to recency and tenure, so all the independent variables only contain positive values
customers_2012_dep_transformed$days_since_last_purchase <- customers_2012_dep_transformed$days_since_last_purchase + 1
customers_2012_dep_transformed$active_days <- customers_2012_dep_transformed$active_days + 1

# Unfortunatelly, we can't express the log link in this transformation, so we will use the default, identity link
# Since our predictors are highly corralted, we will do the transformation separately for product_num
set.seed(32)
print(bt_ <- boxTidwell(nexty_total_sales ~ days_since_last_purchase + active_days  + order_num + total_sales,
                                data = customers_2012_dep_transformed))
lambda_ <- bt_$result[, 3]

print(bt_product_num <- boxTidwell(nexty_total_sales ~ product_num,
                              data = customers_2012_dep_transformed))
lambda_product_num <- bt_product_num$result[, 3]

customers_2012_transformed <- data.frame(days_since_last_purchase = (customers_2012_dep_transformed$days_since_last_purchase)^lambda_[1],
                                         order_num = (customers_2012_dep_transformed$order_num)^lambda_[3], 
                                         total_sales =  (customers_2012_dep_transformed$total_sales)^lambda_[4], 
                                         product_num = (customers_2012_dep_transformed$product_num)^lambda_product_num,
                                         active_days = (customers_2012_dep_transformed$active_days)^lambda_[2],
                                         nexty_total_sales = customers_2012_dep_transformed$nexty_total_sales)
pairs(customers_2012_transformed)

# Train and test
# Clean data: Discarde points with too high leverage, influence
set.seed(32)
summary(l_model <- glm(nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days,
                       data = customers_2012_transformed,
                       family = gaussian(link = "log")))

lev <- hat(model.matrix(l_model))
plot(lev)
high_leverage_points <- as.numeric(row.names(customers_2012_transformed[lev > 0.04,]))

cook <- cooks.distance(l_model)
plot(cook)
points(high_leverage_points, cook[high_leverage_points], col = 'red')
high_influence_points <- as.numeric(row.names(customers_2012_transformed[cook > 10,]))

outliners <- union(high_influence_points, high_leverage_points)
customers_2012_transformed[outliners,]

customers_2012_pp <- customers_2012_transformed[-outliners,]

# Train
set.seed(3456)
trainIndex <- createDataPartition(customers_2012_pp$nexty_total_sales, p = .8, 
                                  list = FALSE, 
                                  times = 1)

customer_purchase_train <- customers_2012_pp[ trainIndex,]
customer_purchase_test  <- customers_2012_pp[-trainIndex,]

set.seed(32)
summary(l_model <- glm(nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days,
                       data = customer_purchase_train,
                       family = gaussian(link = "log")))


# Test
fitted_l_model <- cbind(customer_purchase_test[], predict(l_model, customer_purchase_test, se.fit = T, type = "response"))

# Inverse bc transformation for dependent variable
fitted_l_model$nexty_total_sales <- customers_2012[-outliners,][-trainIndex,]$nexty_total_sales
fitted_l_model$fit <- invBcPower(fitted_l_model$fit, nexty_lambda)


# Plot fit
# We can see, that there is 1 point, where the difference is really high
fitted_l_model <- within(fitted_l_model, {
  LL <- fit - 10 * se.fit
  UL <- fit + 10 * se.fit
})

ggplot(fitted_l_model, aes(nexty_total_sales, fit)) +
  geom_point(size = 1, color = 'red') + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  geom_line(aes(nexty_total_sales, fit)) +
  labs(x = "Actual", y = "Predicted")

# Let's remove the point for the plot and see, whether the other predicted values are correct or not
# We can see, that there are still some points, that still is really far from a y = x line
fitted_l_model_cleaned <- fitted_l_model %>% 
  filter(se.fit != max(se.fit))
ggplot(fitted_l_model_cleaned , aes(nexty_total_sales, fit)) +
  geom_point(size = 1, color = 'red') + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  labs(x = "Actual", y = "Predicted")


# Differences and RMSE
print(test_RMSE <- calculateRMSEFromFit(fitted_l_model))
print(test_diff <- calculateDiffFromModel(fitted_l_model))
print(test_diff_perc <- calculateDiffFromModel(fitted_l_model) * 100 / sum(fitted_l_model$nexty_total_sales))

# Without the highly unaccurate point
print(calculateRMSEFromFit(fitted_l_model_cleaned))
print(calculateDiffFromModel(fitted_l_model_cleaned))
print(calculateDiffFromModel(fitted_l_model_cleaned) * 100 / sum(fitted_l_model_cleaned$nexty_total_sales))

# Unfortunatelly, we can see from the plot and the error measures, that the transformation of the predictors and the dependent data did not help much in refining the model,
# so we won't investigate this approach further

# Save training and test results
glm_results <- createModelSummary(description = "Gaussian linear regression with log link, Boxcox transformation of dependent and Box-Tidwell transformation of independent variables, formula = nexty_total_sales ~ order_num + product_num + total_sales + days_since_last_purchase + active_days",
                                   test_RMSE = test_RMSE,
                                   test_diff = test_diff,
                                   test_diff_percent = test_diff_perc)
saveRDS(glm_results, file = "data/results/glm_log.rds")
