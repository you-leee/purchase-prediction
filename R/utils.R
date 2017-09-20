library(dplyr)

# Data loading
loadPurchases <- function() {
  training_file <- paste0(getwd(), "/data/training.csv")
  purchases <- read.csv(
    training_file,
    quote = "",
    col.names = c(
      "order_id",
      "contact_id",
      "purchase_date",
      "product_id",
      "quantity",
      "sales_amount"
    ),
    colClasses = c(
      "character",
      "character",
      "Date",
      "character",
      "integer",
      "numeric"
    )
  )
  
  purchases
}

loadRFM <- function(year) {
  RFM_file <- paste0(getwd(), "/data/RFM_", year , ".csv")
  
  customer_rfm <- read.csv(
    RFM_file,
    quote="\"",
    col.names = c(
      "contact_id",
      "days_since_last_purchase",
      "order_num",
      "total_sales",
      "product_num",
      "active_days"
    ),
    colClasses = c(
      "character",
      "integer",
      "integer",
      "numeric",
      "integer",
      "integer"
    )
  )
  
  customer_rfm
}

loadPreprocessedRFM <- function(year) {
  RFM_file <- paste0(getwd(), "/data/RFM_pp_", year , ".csv")
  
  col_names <- c(
    "contact_id",
    "days_since_last_purchase",
    "order_num",
    "total_sales",
    "product_num"
  )
  
  col_classes <- c(
    "character",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
  
  if(year == 2012) {
    col_names <- c(col_names, "nexty_total_sales")
    col_classes <- c(col_classes, "numeric")
  }
  
  customer_rfm <- read.csv(
    RFM_file,
    quote="\"",
    col.names = col_names,
    colClasses = col_classes
  )
  
  customer_rfm
}

loadResults <- function() {
  resultFiles <- list.files("data/results", full.names = TRUE)
  lapply(resultFiles, readRDS)
}

# Summarising
createPurchaseSummary <- function(purchases) {
  purchases %>% 
    group_by(year(purchase_date)) %>%
    summarise(rows = n(),
              orders = length(unique(order_id)),
              customers = length(unique(contact_id)),
              products = length(unique(product_id)),
              min_sales = min(sales_amount),
              mean_sales = mean(sales_amount),
              median_sales = median(sales_amount),
              max_sales = max(sales_amount),
              mean_quantity = mean(quantity),
              avg_prod_price = mean_sales / mean_quantity)
}

createModelSummary <- function(description,
                               train_RMSE,
                               test_RMSE, test_diff, test_diff_percent,
                               dec_test_RMSE = NULL, dec_test_diff = NULL, dec_test_diff_percent = NULL,
                               onetime_test_RMSE = NULL, onetime_test_diff = NULL, onetime_test_diff_percent = NULL) {
  list(desc = description,
       test_results = list(RMSE = test_RMSE,
                           diff = test_diff,
                           diff_perc = test_diff_percent),
       dec_test_results = list(RMSE = dec_test_RMSE,
                               diff = dec_test_diff, 
                               diff_perc = dec_test_diff_percent),
       onetime_test_results = list(RMSE = onetime_test_RMSE,
                                   diff = onetime_test_diff,
                                   diff_percent = onetime_test_diff_percent)
       )
}

printModelResults <- function(model_results) {
  is.NullObj <- function(x) is.null(x) | all(sapply(x, is.null))
  
  rmNulls <- function(x) {
    x <- Filter(Negate(is.NullObj), x)
    lapply(x, function(x) if (is.list(x)) rmNulls(x) else x)
  }
  
  prettyResult <- function(model_result) {
    str_results <- paste0("\n-------------------------------------------------------------------------------------------------------------",
           "\nDescription: ", model_result$desc,
           "\nTest RMSE, difference, difference(%):                    ", round(model_result$test_results$RMSE, 3), 
           ", ", round(model_result$test_results$diff),
           ", ", round(model_result$test_results$diff_perc, 3), "%")
    if(!is.null(model_result$dec_test_results$RMSE)) {
      str_results <- paste0(str_results,
                            "\nDecember customers test RMSE, difference, difference(%): ", round(model_result$dec_test_results$RMSE, 3), 
                            ", ", round(model_result$dec_test_results$diff),
                            ", ", round(model_result$dec_test_results$diff_perc, 3), "%",
                            "\nOnetime customers test RMSE, difference, difference(%):  ", round(model_result$onetime_test_results$RMSE, 3), 
                            ", ", round(model_result$onetime_test_results$diff),
                            ", ", round(model_result$onetime_test_results$diff_perc, 3), "%")
    }
    str_results
  }
  
  clean_model_results <- rmNulls(model_results)
  
  lapply(lapply(clean_model_results, prettyResult), cat)[[1]]
  
}

