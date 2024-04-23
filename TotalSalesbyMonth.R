library(lubridate)
library(dplyr)
library(tidyverse)

#The following code takes in Sales data 
#and summarizes total sales by month

data <- read.csv(file = "train.csv")
sales_data <- data
sales_data$Order.Date <- dmy(sales_data$Order.Date)
sales_data$Order.Date.Month <- month(sales_data$Order.Date)

sales_by_day <- sales_data %>% group_by(Order.Date) %>% summarise(TotalSales = sum(Sales))

sales_by_month_all <-
  sales_data %>%
  mutate(YearMonth = floor_date(Order.Date, "month"))

sales_by_month <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = sum(Sales))

plot(sales_by_month$YearMonth, sales_by_month$TotalSales, type = "l", ylab = "Total Sales", xlab = "Month")

sales_dat <- sales_by_month$TotalSales
write.table(sales_dat,
    file = "sales.dat",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)



##############################################################
## Staples, Inc. Revenue 2010 - 2017
## source: https://www.macrotrends.net/stocks/delisted/SPLS/Staples,%20Inc./revenue

staples_annual = data.frame("Year" = c(2009, str_c(201, c(0:7)) ),
                            "Annual_Revenue" = c(23084, 24275, 24135, 24665,
                                                 24381, 23114, 19684, 18764, 18247))
  # revenue in terms of Millions of USD

staples_quarterly = data.frame("Year" = c(str_c("2009", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2010", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2011", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2012", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2013", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2014", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2015", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2016", c("-01-31", "-04-30", "-07-31", "-10-31")),
                                          str_c("2017", c("-01-31", "-04-30", "-07-31")) ),
                               "Quarterly_Revenue" = c(6174, 5818, 5534, 6518, 6406, 6058, 5534,
                                                       6538, 6006, 6173, 5820, 6481, 6191, 6025, 
                                                       5434, 6353, 6568, 5815, 5315, 6112, 5873,
                                                       5654, 5220, 5962, 2848, 5262, 4937, 5593,
                                                       2972, 4363, 4032, 5355, 4497, 4149, 3905))
  # revenue in terms of Millions of USD






