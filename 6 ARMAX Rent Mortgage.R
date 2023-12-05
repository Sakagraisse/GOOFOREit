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
load("CPIs_trunk.RData")
CPIs <- CPIs_trunk
rm(CPIs_trunk)
#

# #calculate the Ren and Mortgage.rate over the year inflation rate
CPIs$Rent <- log(CPIs$Rent/lag(CPIs$Rent,12))
CPIs$Mortgage <- log(CPIs$Mortgage/lag(CPIs$Mortgage,12))
#plot the inflation rate
plot(CPIs$Year, CPIs$Rent, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of rent")
plot(CPIs$Year, CPIs$Mortgage, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate of mortgage")

#auto arimaX fit CPIs$Rent using CPIs$Mortgage as exogenous variable
fit <- auto.arima(CPIs$Rent, xreg = CPIs$Mortgage, seasonal = FALSE, approximation = FALSE, trace=TRUE)

# #reduce data to remove NAs introduced by the lag
CPIs <- CPIs[13:nrow(CPIs),]

#manually check the best model by finding p,d,q in an ARIMA(p,d,q) model
#check for d with dickey fuller test
adf.test(CPIs$Rent, alternative = "stationary", k = trunc((length(CPIs$Rent)-1)^(1/3)))
## NOT stationnary
