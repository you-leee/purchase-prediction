library(ggplot2)
library(gridExtra)
library(lubridate)

source("R/utils.R")

### Load data
purchases <- loadPurchases()


# Data Exploration
## What kind of information holds the training set? For what does each column stend for? Are there any false observations, anomalies? 

# From the names of the columns and the first few rows, we get, that the training set consist of historical purchases of different product from different customers:
#     the id of the order (order_id),
#     the id of the customer (contact_id),
#     the id of the product (product_id),
#     date of purchase (purchase_date),
#     quantity of product in order (quantity)
#     the amount paid (sales_amount).
summary(purchases)
head(purchases, 20)

# Observing the summary, we can see, that the purchases roughly occur in a consistent basis and usually, the customers purchase 1 piece at a time from a product.
# Comment: since the sales_amount represents the amount paid within an order for a product(independently from the quantity),
#   it would be more reasonable to look at the whole amount paid within an order
# Comment: from the summary, we see, that the minimum of the quantity and the sales_amount is negative. We won't examing this case or other possibilities of false observations
#   (for example: duplicates, 0 quantity or amount), since it has been already clarified.

# Order frequency
purchases %>%
  dplyr::select(order_id, purchase_date) %>%
  group_by(purchase_date) %>%
  summarise(N = length(unique(order_id))) %>%
  ggplot() +
  geom_line(aes(purchase_date, N), stat = "identity")

# Quantities of products within an order (with at least 2 hits)
purchases %>%
  dplyr::select(quantity) %>%
  group_by(quantity) %>%
  summarise(N = n()) %>%
  filter(N > 1) %>%
  ggplot() +
  geom_bar(aes(quantity, N), stat = "identity")

# Sales amount of orders categorized
purchases %>%
  dplyr::select(order_id, sales_amount) %>%
  group_by(order_id) %>%
  mutate(
    order_amount = sum(sales_amount),
    order_amount_cat = order_amount %>%
      cut(
        c(0, 50, 100, 200, 500, 1000),
        include.lowest = T
      )
  ) %>%
  group_by(order_amount_cat) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_bar(aes(order_amount_cat,  N), stat = "identity")

# (Weighted) Differences in sales amount of products/piece 
purchases %>%
  dplyr::select(product_id, quantity, sales_amount) %>%
  filter(quantity > 0 & sales_amount > 0) %>%
  mutate(product_price = sales_amount / quantity) %>%
  group_by(product_id) %>%
  summarise(
    quantity = sum(quantity),
    avg_price = mean(product_price),
    diff_price = (max(product_price) - min(product_price)) / avg_price
  ) %>%
  ggplot(aes(quantity, diff_price)) +
  geom_jitter() +
  geom_smooth()



### Overview of the frequency and amount of purchases

# Since we have seen, that there is a trend in the frequency of purchases, it is likely, that there will be also a trend in the amount 
sales_raw <- purchases %>%
  dplyr::select(order_id, purchase_date, sales_amount) %>%
  group_by(order_id, purchase_date) %>%
  summarise(sales_amount = sum(sales_amount)) %>%
  ungroup()

# Amount of purchases /year day, /weekday, /month, /year
sales <- sales_raw %>%
  dplyr::select(sales_amount, purchase_date) %>%
  group_by(purchase_date) %>%
  summarise(total_sales = sum(sales_amount)) %>%
  ungroup() %>%
  mutate(
    month = months(purchase_date),
    month = factor(
      month,
      levels = c(
        "janu?r",
        "febru?r",
        "m?rcius",
        "?prilis",
        "m?jus",
        "j?nius",
        "j?lius",
        "augusztus",
        "szeptember",
        "okt?ber",
        "november",
        "december"
      )
    ),
    weekday = weekdays(purchase_date),
    weekday = factor(
      weekday,
      levels = c(
        "h?tf?",
        "kedd",
        "szerda",
        "cs?t?rt?k",
        "p?ntek",
        "szombat",
        "vas?rnap"
      )
    ),
    week = isoweek(purchase_date),
    year = factor(year(purchase_date)),
    quarter = quarter(purchase_date),
    day_num = yday(purchase_date)
  )

# Daily amount of purchases compared by year
# We can see an overall decrease in 2013, with having the same trend within a year.
ggplot(data = sales, aes(
  x = day_num,
  y = total_sales,
  group = year,
  color = year
)) +
  geom_line() +
  geom_jitter() +
  geom_smooth(n = 10, span = 0.5)


# Sales amount per days of week, weeks of year and months of year
# Days of week: We can see, that customers spend more in the middle of the work week and spend the less on Saturdays
# Weeks of year/months of year/quarters of year: customers spend the most before christmas(week 47-50 / end of november, beginning of december)
# Overall: The patterns are the same in both years
sales_weekdays <- sales %>%
  group_by(year, weekday) %>%
  summarise(avg_sales = weighted.mean(total_sales)) %>%
  rename(grouping = weekday)

sales_weeks <- sales %>%
  group_by(year, week) %>%
  summarise(avg_sales = weighted.mean(total_sales)) %>%
  rename(grouping = week)

sales_months <- sales %>%
  group_by(year, month) %>%
  summarise(avg_sales = weighted.mean(total_sales)) %>%
  rename(grouping = month)

sales_quarters <- sales %>%
  group_by(year, quarter) %>%
  summarise(avg_sales = weighted.mean(total_sales)) %>%
  rename(grouping = quarter)

sales_weekdays_plot <-
  ggplot(sales_weekdays, aes(x = grouping, y = avg_sales)) +
  geom_bar(stat = "identity", width = 0.5) + xlab("")  + facet_grid(. ~ year)
sales_weeks_plot <- sales_weekdays_plot %+% sales_weeks +
  scale_x_continuous(breaks = seq(min(sales_weeks$grouping), max(sales_weeks$grouping), by = 4))
sales_months_plot <- sales_weekdays_plot %+% sales_months +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
sales_quarters_plot <- sales_weekdays_plot  %+% sales_quarters 

grid.arrange(sales_weekdays_plot,
             sales_weeks_plot,
             sales_months_plot,
             sales_quarters_plot,
             ncol = 1)



### Just to see, let's forecast 2014..
# Comment: problem with simple forecasting is, that it didn't consideres user groups and we only have 2 years of data, 
# so we can't test it on a yearly basis
sales_ts <- ts(sales_raw$sales_amount, frequency = 365)
fit_stl <- stl(sales_ts, s.window = 365)
plot(fit_stl)

f_tbats = forecast(fit_stl, h = 365)
plot(f_tbats)


### Summarising the differences between 2012 and 2013
# Surprisingly, the sales_amount is usually higher in 2013, but there is a significant decrease in customers 
#   and (so) in orders and product purchases
purchases %>%
  createPurchaseSummary


