rm(list = ls())
library(readxl)
library(dplyr)
library(lubridate)
library(AER)

#import data
monthly_fuel <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Monthly", col_types = c("date", "numeric", "numeric"))
daily_fuel <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date", "numeric", "numeric"))
colnames(daily_fuel) <- c("date", "HFO380", "brent")

VLCC_ME_USG_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/VLCC_ME_USG_monthly.xlsx", col_types = c("date", "numeric"), skip = 5)
Daily_all_6 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/Daily_all_6.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 5)

#
# Fix data
#



#Bulk rates
#
panamax_daily <- data.frame(Daily_all_6$Date, Daily_all_6$`Baltic Exchange Panamax Index`)
colnames(panamax_daily) <- c("date", "rate")

#aggregate to weekly 
week <- unique(floor_date(panamax_daily$date, "week", week_start = 1)) #find mondays
panamax_weekly <- panamax_daily[panamax_daily$date %in% week,]
sum(is.na(panamax_weekly))


#aggregate to monthly 
month <- unique(floor_date(panamax_daily$date, "month")) #find mondays
panamax_month <- panamax_daily[panamax_daily$date %in% month,]
sum(is.na(panamax_weekly))


#Fuel
week <- unique(floor_date(daily_fuel$date, "week", week_start = 1)) #find mondays
fuel_weekly <- as.data.frame(daily_fuel[daily_fuel$date %in% week,])

#Hacky løysing 
missing_index = which(is.na(fuel_weekly$HFO380))

week <- unique(floor_date(daily_fuel$date, "week", week_start = 2)) #find tuesday
fuel_weekly <- as.data.frame(daily_fuel[daily_fuel$date %in% week,])
missing_index = which(is.na(fuel_weekly$HFO380))

week <- unique(floor_date(daily_fuel$date, "week", week_start = 3)) #find tuesday
fuel_weekly <- as.data.frame(daily_fuel[daily_fuel$date %in% week,])
missing_index = which(is.na(fuel_weekly$HFO380))

week <- unique(floor_date(daily_fuel$date, "week", week_start = 4)) #find tuesday
fuel_weekly <- as.data.frame(daily_fuel[daily_fuel$date %in% week,])
missing_index = which(is.na(fuel_weekly$HFO380))




length(missing_index)
#fuel_weekly[missing_index] <- as.data.frame(daily_fuel[daily_fuel$date %in% week,])


#impute missing data
#HFO380
#missing_index = which(is.na(fuel_weekly$HFO380))
#fuel_weekly$HFO380[missing_index] <- mean(c(fuel_weekly$HFO380[missing_index+1], fuel_weekly$HFO380[missing_index-1]), na.rm = T)
#Brent
#missing_index = which(is.na(fuel_weekly$brent))
#fuel_weekly$brent[missing_index] <- mean(c(fuel_weekly$brent[missing_index+1], fuel_weekly$brent[missing_index-1]), na.rm = T)


sum(is.na(fuel_weekly$brent))


#
# IV
#
#Bad market
bad_market <- 
