library(dplyr)
library(ggplot2)
library(gridExtra)
require(vcd)

# Aggregation utils
calculateRFMs <- function(purchases, rfms = "rfmbt") {
  rfm_vector <- unlist(strsplit(rfms, ""))
  
  customer_rfm <- purchases %>%
    dplyr::select(contact_id) %>%
    unique
  
  for(i in 1:length(rfm_vector)) {
    cat( "Calculating: ", rfm_vector[i], " ... ")
    switch(rfm_vector[i],
           r = {
             customer_rfm <- inner_join(customer_rfm, calculateDaysSinceLastPurchase(purchases), by = "contact_id")
           },
           f = {
             customer_rfm <- inner_join(customer_rfm, calculateOrderNum(purchases), by = "contact_id")
           },
           m = {
             customer_rfm <- inner_join(customer_rfm, calculateTotalSales(purchases), by = "contact_id")
           },
           b = {
             customer_rfm <- inner_join(customer_rfm, calculateProductNum(purchases), by = "contact_id")
           },
           t = {
             customer_rfm <- inner_join(customer_rfm, calculateActiveDays(purchases), by = "contact_id")
           })
  }
  
  customer_rfm
}

calculateActiveDays <- function(purchases) {
  customer_rfm <- purchases %>%
    dplyr::select(contact_id, purchase_date) %>%
    group_by(contact_id) %>%
    summarise(
      active_days = as.numeric(max(purchase_date) - min(purchase_date))
    ) %>%
    ungroup()
  
  customer_rfm
}

calculateDaysSinceLastPurchase <- function(purchases) {
  last_purchase <-
    max(purchases$purchase_date)
  
  customer_rfm <- purchases %>%
    dplyr::select(contact_id, purchase_date) %>%
    group_by(contact_id) %>%
    summarise(
      days_since_last_purchase = as.numeric(last_purchase - max(purchase_date))
    ) %>%
    ungroup()
  
  customer_rfm
}

calculateProductNum <- function(purchases) {
  customer_rfm <- purchases %>%
    dplyr::select(contact_id, product_id) %>%
    group_by(contact_id) %>%
    summarise(
      product_num = length(unique(product_id))
    ) %>%
    ungroup()
  
  customer_rfm
}

calculateOrderNum <- function(purchases) {
  customer_rfm <- purchases %>%
    dplyr::select(contact_id, order_id) %>%
    group_by(contact_id) %>%
    summarise(
      order_num = length(unique(order_id))
    ) %>%
    ungroup()
  
  customer_rfm
}

calculateTotalSales <- function(purchases) {
  customer_rfm <- purchases %>%
    dplyr::select(contact_id, sales_amount) %>%
    group_by(contact_id) %>%
    summarise(
      total_sales = sum(sales_amount)
    ) %>%
    ungroup()
  
  customer_rfm
}


# Plotting utils
createRfmPlot <- function(data, field, bin_num, xlab, title) {
  ggplot(data, aes_string(x = field)) +
    geom_histogram(bins = bin_num, fill = "steelblue") + 
    labs(x = xlab, title = title)
}

plotRfms <- function(data, bin_nums, rfms = "rfmbt", title = "") {
  rfm_vector <- unlist(strsplit(rfms, ""))
  n <- length(rfm_vector)
  
  plots <- vector('list', n)
  
  for(i in 1:n) {
    switch(rfm_vector[i],
           r = {
             recency_plot <- createRfmPlot(data, "days_since_last_purchase", bin_nums[i], "recency", "Recency in days")
             plots[[i]] <- recency_plot
           },
           f = {
             frequency_plot <- createRfmPlot(data, "order_num", bin_nums[i], "frequency", "Purchase frequency")
             plots[[i]] <- frequency_plot
           },
           m = {
             monetary_plot <- createRfmPlot(data, "total_sales", bin_nums[i], "monetary", "Total sales amount")
             plots[[i]] <- monetary_plot
           },
           b = {
             breadth_plot <- createRfmPlot(data, "product_num", bin_nums[i], "breadth", "Number of unique products bought")
             plots[[i]] <- breadth_plot
           },
           t = {
             tenure_plot <- createRfmPlot(data, "active_days", bin_nums[i], "tenure", "Active days (from first to last purchase)")
             plots[[i]] <- tenure_plot
           },
           n = {
             nexty_monetary_plot <- createRfmPlot(data, "nexty_total_sales", bin_nums[i], "monetary", "Total sales amount of next year")
             plots[[i]] <- nexty_monetary_plot
           })
  }
  
  marrangeGrob(plots, nrow = ceiling(sqrt(n)), ncol = round(sqrt(n)), top = title)
}


createMosaicPlot <- function(f, data_table) {
  mosaic(
    f,
    data = data_table,
    shade = TRUE,
    labeling_args = list(
      rot_labels = c(left = 45, top = 45),
      just_labels = c(left = "left",
                      top = "center"),
      offset_labels = c(left = 0.6, top = 0.5)
    ),
    spacing = spacing_dimequal(unit(c(0.5, 0.8), "lines")),
    keep_aspect_ratio = FALSE
  )
}