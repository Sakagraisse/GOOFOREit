#clean space
rm(list = ls())
# ##import data in excell format from the first table only
if(!require(readxl)) install.packages("readxl")
if(!require(reshape2)) install.packages("reshape2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(urca)) install.packages("urca")
if(!require(ecm)) install.packages("ecm")
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(lubridate)) install.packages("lubridate")
if(!require(zoo)) install.packages("zoo")
if(!require(tempdisagg)) install.packages("tempdisagg")
if(!require(openxlsx)) install.packages("openxlsx")
library(readxl)
library(reshape2)
library(ecm)
library(urca)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(tseries)
library(lubridate)
library(tempdisagg)
library(openxlsx)

## import the data ##

######
# 1 Crude oil price
######
#import MCOILWTICO.csv
ECM_Data <- read.csv("MCOILWTICO.csv", header = TRUE, sep = ",")
#convert oil_price$Date to R format from YYYY.MM.DD to monthly format
ECM_Data$Date <- as.Date(ECM_Data$DATE, format = "%Y-%m-%d")
#remove DATE and order columns with date first
ECM_Data <- ECM_Data[,c(3,2)]
#import usdchf.csv AFTER line 16
exchange_rate <- read.csv("EXSZUS.csv", header = TRUE, sep = ",")
exchange_rate$USD_to_CHF <- 1 / exchange_rate$EXSZUS
exchange_rate$Date_c <- as.Date(exchange_rate$DATE, format = "%Y-%m-%d")
#import USD_to_CHF.csv and Date_c to ECM data by subseting the dates by the earliest and latest date of the ECM_data
exchange_rate <-  subset(exchange_rate, Date_c >= "1986-01-01")
exchange_rate <-  subset(exchange_rate, Date_c <= "2023-09-01")
#merge oil_price and exchange_rate
ECM_Data <- merge(ECM_Data, exchange_rate, by.x = "Date", by.y = "Date_c", all.x = TRUE)
#if ECM_Data$Date == ECM_Data$DATE
ECM_Data$Date == ECM_Data$DATE
#remove exchange_rate
rm(exchange_rate)
#remove DATE and EXSZUS from ECM_Data
ECM_Data <- ECM_Data[,c(1,2,5)]
# convert oil_price$MCOILWTICO to swiss francs
ECM_Data$OIL_CHF <- ECM_Data$MCOILWTICO * ECM_Data$USD_to_CHF
# convert OIL_CHF in proportion of 2010-12-01 prices
ECM_Data$B20 <- (ECM_Data$OIL_CHF / ECM_Data$OIL_CHF[which(ECM_Data$Date == "2020-12-01")] ) * 100

######
# 2 Petroleum Products
######
load("CPIs.RData")

#remove CPIs dates below the first date of ECM_Data
CPIs <- subset(CPIs, CPIs$Year >= "1986-01-01")
# Bring Petroleum in ecm data
ECM_Data <- merge(ECM_Data, CPIs, by.x = "Date", by.y = "Year", all.x = TRUE)

## plot B20 and oil
plot(ECM_Data$B20, type = "l", col = "red")
lines(ECM_Data$Petroleum.products, type = "l", col = "blue")

######
# 4 perfom checks before ECM
######
#check for stationarity of the data
adf.test(ECM_Data$B20)
#non stationnarity satisfied

adf.test(ECM_Data$Petroleum.products)
#non stationnarity satisfied

#check for cointegration between the two time series using urca package
test <- ca.jo(ECM_Data[,c(5,6)], type = "trace", ecdet = "const", K = 2, spec = "transitory")
#display the results
summary(test)


######
# 5 estimate the ecm
######

lm1 <- lm(ECM_Data$Petroleum.products~ECM_Data$B20) #Create the linear regression
summary(lm1)
checkresiduals(lm1)

#create a lag ofe one for OIL and B10
ECM_Data$B20_lag1 <- lag(ECM_Data$B20,1)
ECM_Data$Petroleum.products_lag1 <- lag(ECM_Data$Petroleum.products,1)
#create a delta of OIL and B10
ECM_Data$B20_delta <- (ECM_Data$B20 - ECM_Data$B20_lag1)
ECM_Data$Petroleum.products_delta <- (ECM_Data$Petroleum.products - ECM_Data$Petroleum.products_lag1)
#create long term correction
ECM_Data$long_term_correction <- ECM_Data$Petroleum.products_lag1 - lm1$coefficients[1] - lm1$coefficients[2] * ECM_Data$B20_lag1

lm2 <- lm(ECM_Data$Petroleum.products~ECM_Data$B20_delta + ECM_Data$long_term_correction ) #Create the linear regression
plot(lm2$fitted.values)
checkresiduals(lm2)

Box.test(lm2$residuals, lag = 1, type = "Ljung-Box")
plot(lm2$residuals)

#marche pas mais pas grave pour l'instant

######
# 5 Predictions
######

#create function to create data_forcast from different lenghts of row ECM_Data
create_data_forecast <- function(data_to_use, end_row, steps_ahead) {
  data_forecast <- data_to_use[1:end_row, c(1,5:11)]
  ll <- length(data_forecast$B10)
  new_rows <- data.frame(matrix(NA, nrow = 36, ncol = ncol(data_forecast)))
  colnames(new_rows) <- colnames(data_forecast)
  data_forecast <- rbind(data_forecast, new_rows)
  data_forecast$B20[ll+1:steps_ahead] <- rep(tail(ECM_Data$B20, 1), steps_ahead)
  data_forecast$B20_lag1[ll+1:steps_ahead] <- rep(tail(ECM_Data$B20, 1), steps_ahead)
  data_forecast$B20_delta[ll+1:steps_ahead] <- rep(0, steps_ahead)
  #data_forecast$B10_lag1[ll+1:12] <- tail(ECM_Data$B10, 12)
  #continue the date column
  data_forecast$Date[ll+1:steps_ahead] <- seq(as.Date("2023-10-01"), by = "1 months", length.out = steps_ahead)
  return(data_forecast)
}


  # Function to forecast future values
forecast_ECM <- function(data_to_use,starting_row, steps_ahead) {
  temp <- data_to_use
  # Initialize the forecast dataframe with the last row of ECM_Data
  l_base <- starting_row
  # Iterate for the number of steps you want to forecast
  for(i in 2:steps_ahead-1) {
    # Create oil  lag 1
    temp$Petroleum.products_lag1[l_base + i] <- temp$Petroleun.products[l_base + i - 1]
    # Calculate long term correction
    temp$long_term_correction[l_base + i] <- temp$Petroleum.products_lag1[l_base + i] - lm1$coefficients[1] - lm1$coefficients[2] * temp$B20_lag1[l_base + i]

    # Calculate OIL delta
    temp$Petroleum.products_delta[l_base + i] <- lm2$coefficients[1] + lm2$coefficients[3] * temp$long_term_correction[l_base + i]

    # Update the value of oil
    temp$Petroleum.products[l_base + i] <- temp$Petroleum.products_lag1[l_base + i] + temp$Petroleum.products_delta[l_base + i]


  }
  return(temp)
}

test <- create_data_forecast(ECM_Data, nrow(ECM_Data), 36)
# Example usage: Forecasting 5 steps ahead
yay <- forecast_ECM(test,nrow(ECM_Data),36)

test1 <- create_data_forecast(ECM_Data,400, 36)
# Example usage: Forecasting 5 steps ahead
yay1 <- forecast_ECM(test1,nrow(test1)-36,36)

test2 <- create_data_forecast(ECM_Data,375, 36)
# Example usage: Forecasting 5 steps ahead
yay2 <- forecast_ECM(test1,nrow(test2)-36,36)

test3 <- create_data_forecast(ECM_Data,350, 36)
# Example usage: Forecasting 5 steps ahead
yay3 <- forecast_ECM(test1,nrow(test3)-36,36)

test4 <- create_data_forecast(ECM_Data,300, 36)
# Example usage: Forecasting 5 steps ahead
yay4 <- forecast_ECM(test1,nrow(test4)-36,36)
#plot oil delta from row 400
plot(yay$Petroleum.products_delta, type = "l", col = "red")

lines(yay1$Petroleum.products_delta, type = "l", col = "green")
lines(yay2$Petroleum.products_delta, type = "l", col = "yellow")
lines(yay3$Petroleum.products_delta, type = "l", col = "black")
lines(yay4$Petroleum.products_delta, type = "l", col = "purple")
lines(ECM_Data$Petroleum.products_delta, type = "l", col = "blue")
