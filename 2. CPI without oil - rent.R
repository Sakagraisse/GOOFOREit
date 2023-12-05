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

# add a new column to the data
CPIs$Index.withoutRI <- CPIs$`Index.without.petroleum.products` - 0.19325*CPIs$`Rent`

#plot without rent index and withoutRI
plot(CPIs$Year, CPIs$`Index.without.housing.rental`, type = "l", col = "red", xlab = "Year", ylab = "Index", main = "CPIs without rent and without petroleum products")
plot(CPIs$Year, CPIs$Index.withoutRI, col = "blue")

#calculate the CPI$Index.withoutRI over the year inflation rate
CPIs$Inflation.withoutRI_log <- log(CPIs$Index.withoutRI/lag(CPIs$Index.withoutRI,12))
#plot the inflation rate
plot(CPIs$Year, CPIs$Inflation.withoutRI_log, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate", main = "Inflation rate without rent and without petroleum products")

#auto arima fit CPIs$Inflation.withoutRI
fit <- auto.arima(CPIs$Inflation.withoutRI_log, seasonal = FALSE, approximation = FALSE, trace=TRUE)

#reduce data to remove NAs introduced by the lag
CPIs <- CPIs[13:nrow(CPIs),]


# Calcul de la transformation de Fourier rapide (FFT)
fft_result <- fft(CPIs$Inflation.withoutRI_log)

# Calcul des fréquences
frequencies <- seq(from = 0, to = length(CPIs$Inflation.withoutRI_log)/2, length.out = length(CPIs$Inflation.withoutRI_log)/2 + 1)

# Calcul de l'amplitude
amplitudes <- Mod(fft_result)[1:length(frequencies)]

# Tracé du spectre de fréquence
plot(frequencies[1:30], amplitudes[1:30], type = 'l', xlab = 'Fréquence', ylab = 'Amplitude')

#manually check the best model by finding p,d,q in an ARIMA(p,d,q) model
#check for d with dickey fuller test
adf.test(CPIs$Inflation.withoutRI_log, alternative = "stationary", k = trunc((length(CPIs$Inflation.withoutRI_log)-1)^(1/3)))
## NOT stationnary




# check with KPSS
kpss.test(CPIs$Inflation.withoutRI_log, null = "Trend", lshort = TRUE)
## is stationnary

#check for p using PACF
pacf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 1
#check for q using ACF
acf(CPIs$Inflation.withoutRI_log, lag.max = 20, plot = TRUE)
## around 2

#fit the correpsonding ARIMA(1,0,2) model
fit2 <- arima(CPIs$Inflation.withoutRI_log, order = c(1,0,2), method = "ML")
#check the residuals
checkresiduals(fit2)