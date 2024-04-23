library(lubridate)
library(dplyr)
library(tidyverse)

#The following code takes in Sales data 
#and summarizes total sales by month

data <- read.csv(file = "train.csv")
sales_data <- data
sales_data$Order.Date <- dmy(sales_data$Order.Date)
sales_data$Order.Date.Month <- month(sales_data$Order.Date)



##############################################################
## Sales by Day
sales_by_day <- sales_data %>% group_by(Order.Date) %>% summarise(TotalSales = sum(Sales))



##############################################################
## Sales by Month
sales_by_month_all <-
  sales_data %>%
  mutate(YearMonth = floor_date(Order.Date, "month"))

sales_by_month <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = sum(Sales))



##############################################################
## Quarterly Sales to Match Staples

# from https://github.com/tidyverse/lubridate/issues/239
# by jonboiser
floor_date_new <- function(x, unit = c("second","minute","hour","day", "week", "month", "year", "quarter")) {
  unit <- match.arg(unit)
  
  new <- switch(unit,
                second = update(x, seconds = floor(second(x))),
                minute = update(x, seconds = 0),
                hour =   update(x, minutes = 0, seconds = 0),
                day =    update(x, hours = 0, minutes = 0, seconds = 0),
                week =   update(x, wdays = 1, hours = 0, minutes = 0, seconds = 0),
                month =  update(x, mdays = 1, hours = 0, minutes = 0, seconds = 0),
                year =   update(x, ydays = 1, hours = 0, minutes = 0, seconds = 0),
                # modified mdays = 1 to = 31 so that the dates are parsed like the
                # Staples data below
                quarter = update(x, months = ceiling(month(x)/3) * 3 - 2, mdays = 31)
  )
  new
}


sales_quarterly_all <-
  sales_data %>%
  mutate(Quarterly = floor_date_new(Order.Date, "quarter"))

sales_quarterly <- sales_quarterly_all %>% group_by(Quarterly) %>% summarise(TotalSales = sum(Sales))



##############################################################
## Plot #1
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

staples_quarterly$Year = as.Date(staples_quarterly$Year)

staples_quarterly$AR_scaled = staples_quarterly$Quarterly_Revenue*50


##############################################################
## Plot #2
plot(sales_quarterly$Quarterly, sales_quarterly$TotalSales, type = "l", 
     ylab = "Total Sales", xlab = "Month", main = "Quarterly Sales", col="red")

lines(staples_quarterly$Year, staples_quarterly$AR_scaled, col="blue")

legend("bottomright", c("Store Sales (USD)","Staples (20K USD)"), pch = c(20, 20), col = c("red", "blue"))


