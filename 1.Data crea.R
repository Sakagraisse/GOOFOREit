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


CPIs<- data[c("Year", "Total", "Housing.rental.1","Index.without.housing.rental","Petroleum.products","Index.without.petroleum.products")]
save(CPIs, file = "CPIs.RData")

save(weight, file = "weight.RData")


### mortgage

#import excel file with mortgage rate
Mortgage <- read_excel("mortgage_rate_c.xlsx", sheet = 1, col_names = TRUE)
#rename first column "mortgage rate"
colnames(Mortgage)[1] <- "mortgage_rate"
#rename second column "date"
colnames(Mortgage)[2] <- "date"
#create a dataframe with only dates generated monthly from 2008-03-01 to 2023-09-01
dates <- seq(as.Date("2008-07-01"), as.Date("2023-09-01"), by = "month")
indicator_monthly <- ts(dates, start=c(2008,7), frequency=12)
Mortgage <- ts(Mortgage$mortgage_rate, frequency = 4, start = c(2008,3))
Mortgage_q <- Mortgage
monthly_series <- td(Mortgage ~ 1, to = "monthly", method = "denton-cholette")
mort_monthly_numeric <- as.numeric(monthly_series[[1]])
#add the dates to the dataframe
Mortgage <- data.frame(dates, mort_monthly_numeric)
# remove uneeded asset in memory
rm(dates, indicator_monthly, monthly_series, mort_monthly_numeric)

### Rent
rental <- CPIs
#remove data before 2008-07-01
CPIs_trunk <- rental[rental$Year >= as.Date("2008-07-01"),]
#keep only rent
rental <- CPIs_trunk$Housing.rental.1
#convert to ts
rental <- ts(rental, frequency = 12, start = c(2008,3))
#convert to quarterly data
quarterly_series <- aggregate(rental, nfrequency = 4, FUN = mean)
rent_q <- quarterly_series
# convert to monthly data
monthly_series <- td(quarterly_series ~ 1, to = "monthly", method = "additive")
rent_monthly_numeric <- as.numeric(monthly_series[[1]])
dates <- seq(as.Date("2008-07-01"), as.Date("2023-09-01"), by = "month")
indicator_monthly <- ts(dates, start=c(2008,7), frequency=12)
#add the dates to the dataframe
Rent <- data.frame(dates, rent_monthly_numeric)

# remove uneeded asset in memory
rm(dates, indicator_monthly, monthly_series, rent_monthly_numeric, rental, quarterly_series)

# add mortgage and rent to CPIs_trunk
CPIs_trunk$Rent <- Rent$rent_monthly_numeric
CPIs_trunk$Mortgage <- Mortgage$mort_monthly_numeric
#save CPIs_trunk
save(CPIs_trunk, file = "CPIs_trunk.RData")

Date_q <- seq(as.Date("2008-07-01"), as.Date("2023-09-01"), by = "quarter")
Mortgage_q <- as.numeric(Mortgage_q)
rent_q <- as.numeric(rent_q)
Rent_fore_q <- data.frame(Date_q,Mortgage_q,rent_q)

save(Rent_fore_q, file = "Rent_fore_q.RData")