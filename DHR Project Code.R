library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)

#Load in Data
data <- read.csv(file = "train.csv")
sales_data <- data
#convert order date to date object
sales_data$Order.Date <- dmy(sales_data$Order.Date)
sales_data$Order.Date.Month <- month(sales_data$Order.Date)

#Total Sales by Day
sales_by_day <- sales_data %>% group_by(Order.Date) %>% summarise(TotalSales = sum(Sales))

#Total Sales by Month
sales_by_month_all <-
  sales_data %>%
  mutate(YearMonth = floor_date(Order.Date, "month"))
sales_by_month <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = sum(Sales))

#Total Sales by Week
sales_data$Week <- week(sales_data$Order.Date)
sales_data$Year <- year(sales_data$Order.Date)
sales_by_week <- sales_data %>% group_by(Year, Week) %>% summarise(TotalSales = sum(Sales))

#Data Clean Up
outlier <- which(sales_data$Sales == max(sales_data$Sales))
sales_data <- sales_data[-outlier,] #remove outlier
sales_data <- sales_data[sales_data$Category != "Technology",] #remove technology 

#Number of Sales by Month - potential to look at this
sales_by_month_count <- sales_by_month_all %>% group_by(YearMonth) %>% summarise(TotalSales = n())

#Create a time series object of sales by month
sales_full <- ts(sales_by_month$TotalSales, frequency = 12) #all data
sales_train <- ts(sales_by_month$TotalSales[1:42], frequency = 12) #subset for forecasting


#DHR ON FULL DATASET-----------------------------------------
#Set up harmonic regressors of order k
#Test different values of k, chose k where model has lowest AICc
for (i in 1:6){
  harmonics <- fourier(sales_full, K = i)
  # # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
  fit <- auto.arima(sales_full, xreg = harmonics, seasonal = FALSE)
  print(fit$aicc)
}
#define harmonics after determining K 
harmonics <- fourier(sales_full, K = 6)

#Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit <- auto.arima(sales_full, xreg = harmonics, seasonal = FALSE)

#Fitted vs Actual
autoplot(fit)
# 
# # Forecasts next 6 months
newharmonics <- fourier(sales, K = 6, h = 6)
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

autoplot(fc, main = "Forecasting with Sales Data",ts.linetype = 'dashed') +
  geom_line(data = fortify(sales_full),mapping = aes(x = Index, y = Data, color = "Actual Data"),color = "black",linetype = "dashed") + 
  geom_line(data = fortify(fc$fitted),mapping = aes(x = Index, y = Data, color = "Fitted Data"),color = "blue",linetype = "dashed")
