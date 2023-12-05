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


#load data : CPIs.RData
#load data : CPIs.RData
load("CPIs.RData")
#CPIs <- CPIs_trunk
#rm(CPIs_trunk)
CPIs$Inf_OIL <- log(CPIs$`Petroleum.products`/lag(CPIs$`Petroleum.products`,1))
CPIs$Inf_Rent <- log(CPIs$`Housing.rental.1`/lag(CPIs$`Housing.rental.1`,3))
CPIs$Inf_Total <- log(CPIs$Total/lag(CPIs$Total,1))
CPIs$Inflation.withoutRI_log <- (CPIs$Inf_Total - 0.02879*CPIs$Inf_OIL - 0.18625*CPIs$Inf_Rent)/0.78496

#plot without rent index and withoutRI
plot(CPIs$Year, CPIs$`Inflation.withoutRI_log`, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
plot(CPIs$Year, CPIs$Inf_Total, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")

#auto arima fit CPIs$Inflation.withoutRI
fit <- auto.arima(CPIs$Inflation.withoutRI_log, seasonal = FALSE, approximation = FALSE, trace=TRUE)

#reduce data to remove NAs introduced by the lag
CPIs <- CPIs[13:nrow(CPIs),]

#manually check the best model by finding p,d,q in an ARIMA(p,d,q) model
#check for d with dickey fuller test
adf.test(CPIs$Inflation.withoutRI_log, alternative = "stationary", k = trunc((length(CPIs$Inflation.withoutRI_log)-1)^(1/3)))
## Not Stationnary
# check with KPSS
kpss.test(CPIs$Inflation.withoutRI_log, null = "Trend", lshort = TRUE)
## is stationnary

#check for p using PACF
pacf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 3
#check for q using ACF
acf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 2

#fit the correpsonding ARIMA(2,0,0) model
fit2 <- arima(CPIs$Inflation.withoutRI_log, order = c(3,0,0), method = "ML")
#check the residuals
checkresiduals(fit2)

#forecast the inflation rate for the next 36 months
forecast <- forecast(fit, h = 36)
plot(forecast)


######
# In sample tests
######

#calculate the in sample forecast
in_sample_forecast <- fitted(fit2)
#calculate the in sample residuals
in_sample_residuals <- residuals(fit2)
#calculate the in sample RMSE
in_sample_RMSE <- sqrt(mean(in_sample_residuals^2))
#calculate the in sample MAPE
in_sample_MAPE <- mean(abs(in_sample_residuals/in_sample_forecast))
#calculate the in sample MAE
in_sample_MAE <- mean(abs(in_sample_residuals))

# Ljung Box-Q Test
Ljung <- Box.test(in_sample_residuals, lag = 20, type = "Ljung-Box")
# White Test
White <- Box.test(in_sample_residuals^2, lag = 20, type = "Ljung-Box")
#Jarque Bera Test
JB <- jarque.bera.test(in_sample_residuals)

######
# Out of sample tests
######

Inflation.withoutRI_log <- ts(CPIs$Inflation.withoutRI_log,start = c(1984,1), frequency = 12)
out_of_sample <- data.frame(matrix(ncol = 1, nrow = 36))
mean_of_fit <- data.frame(matrix(ncol = 1, nrow = 36))
end <- nrow(CPIs)
end <- end - 36
plot(Inflation.withoutRI_log)

#iterate from line 36 to the en of CPIs
for (i in 37:end){
    temporary <- Inflation.withoutRI_log[1:i-1]
    temporary <- ts(temporary, start = c(1984,1), frequency = 12)
    end_year <- end(temporary)[1]
    end_month <- end(temporary)[2]
    #fit arima model on the first i-1 observations
    fit <- auto.arima(temporary, seasonal = FALSE, approximation = FALSE, trace=TRUE)

    #forecast the i-th observation
    fore <- forecast(fit, h = 36)
    fore <- fore$mean
    print <- ts(fore, start = c(end_year, end_month + 1 ), frequency = 12)
    lines(print, col="red")
    fore <- as.numeric(fore)
    #calculate the out of sample residuals squared
    out_of_sample_residuals <- (Inflation.withoutRI_log[i:i+36] - as.data.frame(fore))^2
    #out_of_sample_residuals <- as.numeric(out_of_sample_residuals)
    #store mean
    meat_of_fit <- data.frame(mean_of_fit, fore)
    #calculate the out of sample forecast
    out_of_sample <- data.frame(out_of_sample, out_of_sample_residuals)

}


#drop column one od out_of_sample
Squared <- out_of_sample[,-1]
# do the mean of each row of  out_of_sample

Squared <- rowMeans(Squared)
plot(Squared)
