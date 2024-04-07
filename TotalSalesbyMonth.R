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