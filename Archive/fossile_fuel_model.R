<<<<<<< HEAD
rm(list = ls())
library(readxl)

#Daily 01-1998 to 04-2013
#Bunker1 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunker.xlsx", col_types = c("date", "numeric", "numeric"))

#Monthly 01-1990 to 09-2024
#Bunkers2 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunkers.xlsx", col_types = c("date", "text"), skip = 5)

#Import daily data
bunker_daily <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date","numeric", "numeric"))
colnames(bunker_daily) <- c("Date", "HFO380", "Brent")

#Import monthly data
bunker_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx",sheet = "Monthly", col_types = c("date","numeric", "numeric"))
colnames(bunker_monthly) <- c("Date", "HFO380", "Brent")

#plot fuel
plot(bunker_daily$Date, bunker_daily$HFO380, type = "l")


##
##linear model
##

#Full sample
#fit <- lm(bunker_monthly$HFO380~bunker_monthly$Brent)
#summary(fit)
#fit1 <- lm(bunker_daily$HFO380~bunker_daily$Brent)
#summary(fit1)


#post 2007
fit <- lm(HFO380~Brent, data = bunker_monthly[bunker_monthly$Date>as.POSIXct("2007-01-01"),])
fit_summary <- summary(fit)
fit1 <- lm(HFO380~Brent, data = bunker_daily[bunker_daily$Date>as.POSIXct("2007-01-01"),])
summary(fit1)



#Simulate
scenarios <- c(47.04, 95, 154.92)
B <- 5000

path_brent <- data.frame(brent = sample(scenarios, B, T, c(0.25, 0.5, 0.25)))
#colnames(path_brent) = "Brent"

#HFO380 <- predict(fit, newdata = path_brent)
HFO380 <- rnorm(B, fit_summary$coefficients[1,1],fit_summary$coefficients[1,2]) +rnorm(B, fit_summary$coefficients[2,1],fit_summary$coefficients[2,2])*path_brent+rnorm(B,0, sd(fit$residuals))


hist(HFO380$brent)
mean(HFO380$brent)
sd(HFO380$brent)



#
# With price of carbon
#

#Emissions in tCo2e/Toe
emissions_hfo <- 3.219
emissions_nh3 <- 0.2219

#IMO NZF
tier1_price <- 100
tier2_price <- 380


#tier 1 scenario













=======
<<<<<<< HEAD
rm(list = ls())
library(readxl)

#Daily 01-1998 to 04-2013
#Bunker1 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunker.xlsx", col_types = c("date", "numeric", "numeric"))

#Monthly 01-1990 to 09-2024
#Bunkers2 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunkers.xlsx", col_types = c("date", "text"), skip = 5)

#Import daily data
bunker_daily <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date","numeric", "numeric"))
colnames(bunker_daily) <- c("Date", "HFO380", "Brent")

#Import monthly data
bunker_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx",sheet = "Monthly", col_types = c("date","numeric", "numeric"))
colnames(bunker_monthly) <- c("Date", "HFO380", "Brent")

#plot fuel
plot(bunker_daily$Date, bunker_daily$HFO380, type = "l")


##
##linear model
##

#Full sample
#fit <- lm(bunker_monthly$HFO380~bunker_monthly$Brent)
#summary(fit)
#fit1 <- lm(bunker_daily$HFO380~bunker_daily$Brent)
#summary(fit1)


#post 2007
fit <- lm(HFO380~Brent, data = bunker_monthly[bunker_monthly$Date>as.POSIXct("2007-01-01"),])
fit_summary <- summary(fit)
fit1 <- lm(HFO380~Brent, data = bunker_daily[bunker_daily$Date>as.POSIXct("2007-01-01"),])
summary(fit1)



#Simulate
scenarios <- c(47.04, 95, 154.92)
B <- 5000

path_brent <- data.frame(brent = sample(scenarios, B, T, c(0.25, 0.5, 0.25)))
#colnames(path_brent) = "Brent"

#HFO380 <- predict(fit, newdata = path_brent)
HFO380 <- rnorm(B, fit_summary$coefficients[1,1],fit_summary$coefficients[1,2]) +rnorm(B, fit_summary$coefficients[2,1],fit_summary$coefficients[2,2])*path_brent+rnorm(B,0, sd(fit$residuals))


hist(HFO380$brent)
mean(HFO380$brent)
sd(HFO380$brent)



#
# With price of carbon
#

#Emissions in tCo2e/Toe
emissions_hfo <- 3.219
emissions_nh3 <- 0.2219

#IMO NZF
tier1_price <- 100
tier2_price <- 380


#tier 1 scenario













=======
rm(list = ls())
library(readxl)

#Daily 01-1998 to 04-2013
#Bunker1 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunker.xlsx", col_types = c("date", "numeric", "numeric"))

#Monthly 01-1990 to 09-2024
#Bunkers2 <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Bunkers.xlsx", col_types = c("date", "text"), skip = 5)

#Import daily data
bunker_daily <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date","numeric", "numeric"))
colnames(bunker_daily) <- c("Date", "HFO380", "Brent")

#Import monthly data
bunker_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx",sheet = "Monthly", col_types = c("date","numeric", "numeric"))
colnames(bunker_monthly) <- c("Date", "HFO380", "Brent")

#plot fuel
plot(bunker_daily$Date, bunker_daily$HFO380, type = "l")


##
##linear model
##

#Full sample
#fit <- lm(bunker_monthly$HFO380~bunker_monthly$Brent)
#summary(fit)
#fit1 <- lm(bunker_daily$HFO380~bunker_daily$Brent)
#summary(fit1)


#post 2007
fit <- lm(HFO380~Brent, data = bunker_monthly[bunker_monthly$Date>as.POSIXct("2007-01-01"),])
fit_summary <- summary(fit)
fit1 <- lm(HFO380~Brent, data = bunker_daily[bunker_daily$Date>as.POSIXct("2007-01-01"),])
summary(fit1)



#Simulate
scenarios <- c(47.04, 95, 154.92)
B <- 5000

path_brent <- data.frame(brent = sample(scenarios, B, T, c(0.25, 0.5, 0.25)))
#colnames(path_brent) = "Brent"

#HFO380 <- predict(fit, newdata = path_brent)
HFO380 <- rnorm(B, fit_summary$coefficients[1,1],fit_summary$coefficients[1,2]) +rnorm(B, fit_summary$coefficients[2,1],fit_summary$coefficients[2,2])*path_brent+rnorm(B,0, sd(fit$residuals))


hist(HFO380$brent)
mean(HFO380$brent)
sd(HFO380$brent)



#
# With price of carbon
#

#Emissions in tCo2e/Toe
emissions_hfo <- 3.219
emissions_nh3 <- 0.2219

#IMO NZF
tier1_price <- 100
tier2_price <- 380


#tier 1 scenario













>>>>>>> 7adb83d50cbaa460565a66ecc5570c65560fd671
>>>>>>> 8269d2a5ef16ea27896714e05893d0fa3128d894
