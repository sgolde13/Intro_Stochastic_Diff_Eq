##############################################################
##############################################################
##############################################################
## Sales Data Exploratory Data Analysis and *.dat Preparation
##
## Outline:
##    1. Importation, Functions, and Packages
##    2. Date Parsing
##    3. Staples, Inc. Revenue 2010 - 2017
##    4. Staples, Inc. Revenue 2010 - 2017
##    5. Data Preparation
##    6. DHR on Full Data Set
##
##
##


##############################################################
##############################################################
## 1. Importation, Functions, and Packages
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(ggfortify)


#Load in Data
data <- read.csv(file = "train.csv")
sales_data <- data

#convert order date to date object
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
## Total Sales by Week
sales_data$Week <- week(sales_data$Order.Date)
sales_data$Year <- year(sales_data$Order.Date)
sales_by_week <- sales_data %>% group_by(Year, Week) %>% summarise(TotalSales = sum(Sales))


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
## 5. Data Preparation

# Data Clean Up
outlier <- which(sales_data$Sales == max(sales_data$Sales))
sales_data <- sales_data[-outlier,] #remove outlier
#sales_data <- sales_data[sales_data$Category != "Technology",] #remove technology 

# Number of Sales by Month - potential to look at this
sales_by_month_count <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = n())

# Create a time series object of sales by month
sales_full <- ts(sales_by_month$TotalSales, frequency = 12, start = c(2015,1)) #all data
sales_train <- ts(sales_by_month$TotalSales[1:42], frequency = 12,start = c(2015,1)) #subset for forecasting
sales_train2 <- ts(sales_by_month$TotalSales[7:48], frequency = 12,start = c(2015,7)) #subset for forecasting


##############################################################
## Plots

# Using the forecast package
autoplot(stl(sales_full, s.window = 'periodic'), ts.colour = 'black', xlab = "Year")


# Using ggplot2
library(gridExtra)
full_stl <- stl(sales_full, s.window = 'periodic')[[1]] %>% as.data.frame()
full_stl$data <- sales_by_month$TotalSales

# x-axis labels and label spacing
dates = format(sales_by_month$YearMonth, format="%b %y") %>% as.character()
  # https://www.statology.org/r-date-format/

spacing = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 36, 40, 44, 48)

# subplots
data_plot = ggplot(data = full_stl) + theme_minimal() +
  geom_line(aes(x = 1:nrow(full_stl), y = data )) +
  labs(title = "Data", x ="", y = "") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())


seasonal_plot = ggplot(data = full_stl) + theme_minimal() +
  geom_line(aes(x = 1:nrow(full_stl), y = seasonal )) +
  labs(title = "Seasonal", x ="", y = "") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
  
  
remainder_plot = ggplot(data = full_stl) + theme_minimal() +
  geom_line(aes(x = 1:nrow(full_stl), y = remainder )) +
  labs(title = "Remainder", x ="", y = "") + 
  theme(axis.text.x = element_text(angle = 30)) +
  scale_x_continuous(breaks = spacing, labels = dates[spacing])


grid.arrange(data_plot, seasonal_plot, remainder_plot, nrow = 3)
# https://stackoverflow.com/questions/65985705/defining-grid-arrange-so-the-third-plot-is-in-the-middle


##############################################################
##############################################################
## 6. DHR on Full Data Set

#Set up harmonic regressors of order k
#Test different values of k, chose k where model has lowest AICc
for (i in 1:6){
  harmonics <- fourier(sales_full, K = i)
  # Fit a dynamic regression model to fit. Set xreg equal to harmonics 
  # and seasonal to FALSE because seasonality is handled by the regressors.
  fit <- auto.arima(sales_full, xreg = harmonics, seasonal = FALSE)
  print(fit$aicc)
}
# define harmonics after determining K 
harmonics <- fourier(sales_full, K = 6)

# Fit a dynamic regression model to fit. Set xreg equal to harmonics and 
# seasonal to FALSE because seasonality is handled by the regressors.
fit <- auto.arima(sales_full, xreg = harmonics, seasonal = FALSE)
fit <- tslm(sales_full ~ fourier(sales_full, K = c(6, 6)))

##############################################################
## Fitted vs Actual
ggplot() + geom_line(data = fortify(fit$fitted), mapping = aes(x=Index,y=Data, color = "Model Fit")) + 
  geom_line(data = fortify(sales_full), mapping = aes(x = Index, y=Data, color = "Actual Data"))+
  theme(legend.title = element_blank()) + ylab("Total Sales") + xlab("Time") + ggtitle("DHR Model Fit")

#Residuals Plots
autoplot(fit$residuals)
ggtsdiag(auto.arima(sales_full))

# # Forecasts next year
newharmonics <- fourier(sales_full, K = 6, h = 12)
fc <- forecast(fit, xreg = newharmonics)
# 
# # Plot forecasts fc
autoplot(fc, main = "Forecasting with Sales Data",ts.linetype = 'dashed') +
  geom_line(data = fortify(sales_full),mapping = aes(x = Index, y = Data, color = "Actual Data"),color = "black",linetype = "dashed") + 
  geom_line(data = fortify(fc$fitted),mapping = aes(x = Index, y = Data, color = "Fitted Data"),color = "blue",linetype = "dashed")


#DHR ON TRAINING DATASET and FORECASTING---------------------------
#Set up harmonic regressors of order k
#Test different values of k, chose k where model has lowest AICc
for (i in 1:6){
  harmonics <- fourier(sales_train, K = i)
  # # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
  fit <- auto.arima(sales_train, xreg = harmonics, seasonal = FALSE)
  print(fit$aicc)
}
#define harmonics after determining K 
harmonics <- fourier(sales_train, K = 6)

#Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit <- auto.arima(sales_train, xreg = harmonics, seasonal = FALSE)

#Fitted vs Actual
autoplot(fit)
 
# Forecasts next 6 months
newharmonics <- fourier(sales_train, K = 6, h = 6)
fc <- forecast(fit, xreg = newharmonics)
 
# Plot forecasts fc
autoplot(fc)

#Forecasting Plot
autoplot(fc, main = "Forecasting with Sales Data") +
  geom_line(data = fortify(sales_full),mapping = aes(x = Index, y = Data, color = "Actual Data"),color = "black",linetype = "dashed")+
  ylab("Total Sales") + xlab("Time") +  scale_x_continuous(name = "Time",labels = c(2015,2016,2017, 2018,2019))

plot(fc, main = "Forecasting with Sales Data", xlab = "Time", ylab = "Total Sales")
lines(sales_full, lty = 2)

# ggplot() + 
#   geom_line(fortify(fc$mean), mapping = aes(x = Index, y = Data)) + 
#   geom_line(data = fortify(sales_full),mapping = aes(x = Index, y = Data, color = "Actual Data"),color = "black",linetype = "dashed") 
# + ylab("Total Sales") + xlab("Time") 
# 
# autoplot(fc, main = "Forecasting with Sales Data",ts.linetype = 'dashed') +
#   geom_line(data = fortify(sales_full),mapping = aes(x = Index, y = Data, color = "Actual Data"),color = "black",linetype = "dashed") + 
#   geom_line(data = fortify(fc$fitted),mapping = aes(x = Index, y = Data, color = "Fitted Data"),color = "blue",linetype = "dashed")

#Backcasting with Sales Data --------------------------------

#define harmonics after determining K 
harmonics <- fourier(sales_train2, K = 6)

#Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit <- auto.arima(sales_train2, xreg = harmonics, seasonal = FALSE)

#Fitted vs Actual
autoplot(fit)

# backcast prev 6 months
f = frequency(sales_train2)

revx <- ts(rev(sales_train2), frequency=f)
fc <- forecast(auto.arima(revx), h = 6)
plot(fc)

# Reverse time again
h = 6
fc$mean <- ts(rev(fc$mean),end=tsp(sales_train2)[1] - 1/f, frequency=f)
fc$upper <- fc$upper[h:1,]
fc$lower <- fc$lower[h:1,]
fc$x <- sales_train2
# Plot result
plot(fc, xlim=c(tsp(sales_train2)[1]-h/f, tsp(sales_train2)[2]), main = "Backcasting with Sales Data", xlab = "Time", ylab = "Total Sales", ylim = c(0,120000))
lines(sales_full, lty = 2)

#AR Spectrum Plot
spec.ar(sales_full, main = "Sales Data \n AR (14) Spectrum" ) #Order chosen by AIC

