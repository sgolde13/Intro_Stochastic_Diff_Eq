##############################################################
##############################################################
##############################################################
## Sales Data Exploratory Data Analysis and *.dat Preparation
##
## Outline:
##    1. Importation, Functions, and Packages
##    2. Date Parsing
##    3. Staples, Inc. Revenue 2010 - 2017
##    4. ODP Revenue 2010 - 2023
##    5. Plots
##    6. Tables
##
##


##############################################################
##############################################################
## 1. Importation, Functions, and Packages
library(lubridate)
library(dplyr)
library(tidyverse)


data <- read.csv(file = "train.csv")
sales_data <- data

# change the order date to Day-Month-Year or Month format
sales_data$Order.Date <- dmy(sales_data$Order.Date)
sales_data$Order.Date.Month <- month(sales_data$Order.Date)


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



##############################################################
##############################################################
## 2. Date Parsing
## The following code takes in Sales data 
## and summarizes total sales by month

##############################################################
## Total Sales by Day
sales_by_day <- sales_data %>% group_by(Order.Date) %>% summarise(TotalSales = sum(Sales))


##############################################################
## Total Sales by Month
sales_by_month_all <-
  sales_data %>%
  mutate(YearMonth = floor_date(Order.Date, "month"))

sales_by_month <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = sum(Sales))


##############################################################
## Total Annual Sales
sales_by_year_all <-
  sales_data %>%
  mutate(YearMonth = floor_date(Order.Date, "year"))

sales_by_year_all %>% group_by(YearMonth) %>% summarise(TotalSales = sum(Sales))


##############################################################
## Quarterly Sales to Match Staples
sales_quarterly_all <-
  sales_data %>%
  mutate(Quarterly = floor_date_new(Order.Date, "quarter"))

sales_quarterly <- sales_quarterly_all %>% group_by(Quarterly) %>% summarise(TotalSales = sum(Sales))



##############################################################
##############################################################
## 3. Staples, Inc. Revenue 2010 - 2017
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
##############################################################
## 4. ODP Revenue 2010 - 2023
## source: https://www.macrotrends.net/stocks/charts/ODP/odp/revenue

odp_annual = data.frame("Year" = c(2009, str_c(201, c(0:9)), 2020, 2021, 2022, 2023),
                        "Annual_Revenue" = c(12144, 11633, 11489, 10696, 11242,
                                             12710, 11727, 11021, 10240, 11015,
                                             9667, 8872, 8465, 8491, 7831))
# revenue in terms of Millions of USD

odp_quarterly = data.frame("Year" = c(str_c("2009", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2010", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2011", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2012", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2013", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2014", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2015", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2016", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2017", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2018", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2019", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2020", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2021", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2022", c("-03-31", "-06-30", "-09-30", "-12-31")),
                                      str_c("2023", c("-03-31", "-06-30", "-09-30", "-12-31")) ),
                           "Quarterly_Revenue" = c(3225, 2824, 3029, 3066, 3072, 2699, 2900,
                                                   2962, 2973, 2710, 2837, 2969, 2873, 2507,
                                                   2693, 2623, 2718, 2419, 2619, 3486, 4354,
                                                   3841, 4069,  446, 3877, 3440, 3046, 1364,
                                                   2876, 2583, 2836, 2726, 2676, 2363, 2620,
                                                   2581, 2830, 2628, 2887, 2670, 2769, 2588,
                                                   2782, 1528, 2725, 2158, 2347, 1642, 2174,
                                                   2070, 2179, 2042, 2178, 2034, 2172, 2107,
                                                   2108, 1908, 2009, 1806))
# revenue in terms of Millions of USD

odp_quarterly$Year = as.Date(odp_quarterly$Year)

odp_quarterly$AR_scaled = odp_quarterly$Quarterly_Revenue*50




##############################################################
##############################################################
## 5. Plots

##############################################################
## Sales by Month vs. Total Sales
plot(sales_by_month$YearMonth, sales_by_month$TotalSales, type = "l", ylab = "Total Sales", xlab = "Month")

sales_dat <- sales_by_month$TotalSales
write.table(sales_dat,
            file = "sales.dat",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)


##############################################################
## Quarterly vs. Total Sales + Staples
plot(sales_quarterly$Quarterly, sales_quarterly$TotalSales, type = "l", 
     ylab = "Total Sales", xlab = "Month", main = "Quarterly Sales", col="red")

lines(staples_quarterly$Year, staples_quarterly$AR_scaled, col="blue")

legend("bottomright", c("Store Sales (USD)","Staples (20K USD)"), pch = c(20, 20), col = c("red", "blue"))



##############################################################
##############################################################
## 6. Tables
round((table(data$Sub.Category, data$Category) / 9800)*100, 1)

round((table(data$Sub.Category) / 9800)*100, 1) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

round((table(data$Category, data$Segment) / 9800)*100, 1)







