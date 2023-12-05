## Read Data ---------------------------------------------------------------

#clean space
rm(list = ls())
# ##import data in excell format from the first table only
#install.packages("readxl")
#install.packages("reshape2")
#install.packages("zoo")
#install.packages('dplyr')
#install.packages("forecast")
#install.packages("tseries")
#install.packages((lubridate))

library(readxl)
library(zoo)
library(dplyr)
library(forecast)
library(tseries)
library(lubridate)

rm(list = ls())
data <- read_excel("CPI_2020.xlsx", sheet = 1, col_names = TRUE, skip = 1)
#check which type is data
class(data)

#Remove the first 5 columns
data <- data[,12:504]

#Remove the columns 2
data <- data[,-2]
#Remove columns 3
data <- data[,-3]
#Remove first line
data <- data[-1,]
# Make the first line the name of the columns
colnames(data) <- data[1,]
#check type of data
class(data)
#exctracte column 2 and name it weight
weight <- data[,1:2]
#remove the first line
weight <- weight[-c(1:3),]
#remove first line and secdond column from data
data <- data[,-2]
#change the first index of the table to "Year"
data[1,1] <- "Year"
#check the type of data$30317
class(data$`30317`)
#Reshape the data so the years which is the first line are in one column
data <- t(data)
#make the first line the name of the columns
colnames(data) <- data[1,]
#remove the first line
data <- data[-1,]
#make colums vectors again and make it numbers not characters
data <- as.data.frame(data)
#format each column of data to be numeric
data <- data.frame(lapply(data, as.numeric))
#format the first column of date to display a date from the excel way of counting using openxlsx package
data$Year <- as.Date(as.numeric(data$Year), origin = "1899-12-30")


#####Rent data quarterly-----
#Mortgage rate quarterly

#import excel file with mortgage rate
data2 <- read_excel("mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE)

#rename first column "mortgage rate"
colnames(data2)[1] <- "mortgage.rate"
#rename second column "date"
colnames(data2)[2] <- "date"

#order date
data2 <- data2[order(data2$date),]

#create dates
data2$dates <- seq(as.Date("2008-09-01"), by = "3 months", length.out = nrow(data2))
#delete date column
data2 <- data2[,-2]
#order date
data2 <- data2[order(data2$dates),]
#keep only Rent and Year variables from data3
data3 <- data[,c(1,156)]
#Convert monthly Rent data3 to quarterly
data3 <- data3 %>%
  mutate(date = as.Date(Year),  # Ensure the 'Year' column is in Date format
         year = year(date),
         quarter = quarter(date)) %>%
  group_by(year, quarter) %>%
  # Remove rows with NA in 'Rent' before summarizing
  filter(!is.na(Rent)) %>%
  summarize(last_quarter_rent = last(Rent))  # Get the last Rent value of each quarter

data3 <- data3 %>%
  mutate(Year = paste(year,
                      case_when(
                        quarter == 1 ~ "03.01",
                        quarter == 2 ~ "06.01",
                        quarter == 3 ~ "09.01",
                        quarter == 4 ~ "12.01"
                      ),
                      sep="."
  )
  )
#Keep only the variables Year and quarterly_rent
data3 <- data3[,c(3,4)]
#drop the first 102
data3 <- data3[-c(1:102),]
#rename quareterly_rent to Rent
colnames(data3)[1] <- "Rent"

#Merge data3 and data 2
data3$mortgage.rate <- data2$mortgage.rate

#create the "good monthly" times series using show_lin

install.packages("tempdisagg")
library(tempdisagg)
packageVersion("tempdisagg")
install.packages("tempdisagg")
# Exemple de séries trimestrielles
rent_quarterly <- ts(data3$Rent, start=c(2008,3), frequency=4)
mortgage_rate_quarterly <- ts(data3$mortgage.rate, start=c(2008,3), frequency=4)

#create a dataset with housing.and.energy from data but only the last 309 lines

data4 <- data$Housing.and.energy
#only keep the last 309 lines of data4
data4 <- data4[-c(1:309)]

# Exemple de série indicatrice mensuelle
indicator_monthly <- ts(data4, start=c(2008,6), frequency=12)

rent_monthly <- td(rent_quarterly ~ 1, to = "monthly", method = "denton-cholette")
mortgage_rate_monthly <- td(mortgage_rate_quarterly ~ 1, to = "monthly", method = "denton-cholette")

plot(rent_monthly)
plot(mortgage_rate_monthly)


rent_monthly_numeric <- as.numeric(rent_monthly[[1]])
mortgage_rate_monthly_numeric <- as.numeric(mortgage_rate_monthly[[1]])
rent_monthly_inflation <- log(rent_monthly_numeric/lag(rent_monthly_numeric, 12))


###### Rent forecast -----
#ARIMA model for rent  with mortgage rate as exogenous variable
# Dclate Rent as a quaretly time series
Rent <- ts(rent_monthly_inflation,start=c(2008,6),frequency=12)
mortgage.rate <- ts(mortgage_rate_monthly_numeric,start=c(2008,6),frequency=12)



#adf.test(Rent)
#rent.diff <- diff(Rent)
#adf.test(rent.diff)

#adf.test(mortgage.rate)
#mortgage.rate.diff <- diff(mortgage.rate)
#adf.test(mortgage.rate.diff)

fit <- auto.arima(Rent, xreg = mortgage.rate, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit)
checkresiduals(fit)

## Forecast of the mortgage rate
# Fit an ARMA model to the historical mortgage rate data
#first difference for mortgage rate

fit2 <- auto.arima(mortgage.rate, seasonal=FALSE, stepwise=TRUE, approximation=FALSE)
checkresiduals (fit2)
# Use the fitted ARMA model to forecast future mortgage rates
future_mortgage_rate_forecast <- forecast(fit2, h=12)
# The forecast object contains point forecasts, lower and upper confidence intervals
print(future_mortgage_rate_forecast)
# Plot the forecast to visualize
plot(future_mortgage_rate_forecast)
# Extract the point forecasts of the future mortgage rates
future_mortgage_rate_values <- future_mortgage_rate_forecast$mean
# Forecasted mortgage rate values as the exogenous variable in the rent forecast
rent_forecast <- forecast(fit, xreg=future_mortgage_rate_values, h=12)
# Plot the forecast of rent
plot(rent_forecast)
# Print the forecasted rent values
print(rent_forecast)
#transform point forecast values to inflation rate


#Forecast of Total----------
#build a new column = data$Total - data$rent - oil
data5 <- data[,c(1,2,156,188)]
#keep after line 306
data5 <- data5[-c(1:305),]

Total <- ts(data5$Total,start=c(2008,6),frequency=12)
Heating.oil <- ts(data5$Heating.oil,start=c(2008,6),frequency=12)

#create time series equatl to total - rent - oil
Total_wor <- Total - 0.19325*Rent - 0.00603*Heating.oil
#drop NA
Total_wor <- na.omit(Total_wor)
Total_wor_numeric <- as.numeric(Total_wor)
Total_wor = log(Total_wor_numeric/lag(Total_wor_numeric,12))
Total_wor <- na.omit(Total_wor)


fit3 <- auto.arima(Total_wor, seasonal=FALSE, approximation=FALSE, trace=TRUE)
summary(fit3)
checkresiduals(fit3)
total_forecast_monthly <- forecast(fit3,  h=36)
# Plot the forecast of rent
plot(total_forecast_monthly$residuals)
# Print the forecasted rent values
print(total_forecast_monthly)
 plot(total_forecast_monthly$residuals)