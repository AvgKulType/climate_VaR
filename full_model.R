<<<<<<< HEAD
rm(list = ls())
library(readxl)
library(stargazer)


####################
##                ##
## Import data    ##
##                ##
####################

#Import daily bunker data
bunker_daily <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date","numeric", "numeric"))
colnames(bunker_daily) <- c("Date", "HFO380", "Brent")

#Import monthly bunker data
bunker_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx",sheet = "Monthly", col_types = c("date","numeric", "numeric"))
colnames(bunker_monthly) <- c("Date", "HFO380", "Brent")

#plot fuel
#plot(bunker_daily$Date, bunker_daily$HFO380, type = "l")

#import EU ETS data
icap.graph.carbon.price.data.2008.08.01.2025.08.09 <- read.csv("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/icap-graph-carbon-price-data-2008-08-01-2025-08-09.csv", header=FALSE)
#prerp ETS data
ETS_EUR <- c(icap.graph.carbon.price.data.2008.08.01.2025.08.09$V5[1000:2638], icap.graph.carbon.price.data.2008.08.01.2025.08.09$V10[2639:4081])
date <- icap.graph.carbon.price.data.2008.08.01.2025.08.09$V1[1000:4081]
EUR_USD <- c( icap.graph.carbon.price.data.2008.08.01.2025.08.09$V3[1000:2638],icap.graph.carbon.price.data.2008.08.01.2025.08.09$V8[2639:4081])

ETS_EUR <- as.numeric(ETS_EUR)[1400:3082] #Remove the part of the data with the most null values
EUR_USD <- as.numeric(EUR_USD)[1400:3082]
ETS <- ETS_EUR*EUR_USD
date <- date[1400:3082]



##################
##              ##
##  Parameters  ##
##              ##
##################
B <- 10000
pass_through <- 0.4     #Rule of thumb for dry bulk in the literature 


######################
##                  ##
##  Fossile fuel    ##
##                  ##
######################

#Fits a linear model on the price of bunker based on price of oil
fit <- lm(HFO380~Brent, data = bunker_monthly[bunker_monthly$Date>as.POSIXct("2007-01-01"),])
fit_summary <- summary(fit)
#fit1 <- lm(HFO380~Brent, data = bunker_daily[bunker_daily$Date>as.POSIXct("2007-01-01"),])
#summary(fit1)



#Simulate
#scenarios <- c(47.04, 95, 154.92)

path_brent <- runif(B, min = 25, 150)

#path_brent <- data.frame(brent = sample(scenarios, B, T, c(0.25, 0.5, 0.25)))
#colnames(path_brent) = "Brent"

#HFO380 <- predict(fit, newdata = path_brent)
HFO380 <- rnorm(B, fit_summary$coefficients[1,1],fit_summary$coefficients[1,2]) +rnorm(B, fit_summary$coefficients[2,1],fit_summary$coefficients[2,2])*path_brent+rnorm(B,0, sd(fit$residuals))


hist(HFO380)
mean(HFO380$brent)
sd(HFO380$brent)



##########################
##                      ##
##  Zero-emission fuel  ##
##                      ##
##########################

#
#Simulating ammonia production
# https://iea.blob.core.windows.net/assets/2ceb17b8-474f-4154-aab5-4d898f735c17/IEAGHRassumptions_final.pdf
#

#Model 1: with capex and opex with 20 year lifespan
CAPEX <- runif(B, 1000,1100)
OPEX <- runif(B, 0.015,0.03)*CAPEX #Annual
Electricity_price <- runif(B, 10,50) #in Mwh
Price_ammonia <- 2.17*((CAPEX+OPEX)/20+9.167*Electricity_price)

mean(Price_ammonia)
hist(Price_ammonia)



########################
##                    ##
##  Price of carbon   ##
##                    ##
########################

#Emissions in tCo2e/Toe
emissions_hfo <- 3.219
emissions_nh3 <- 0.2219

#IMO NZF
tier1_price <- 100
tier2_price <- 380


#tier 1 scenario
ammonia_w_poc <- Price_ammonia+tier1_price*emissions_nh3
hfo_w_poc <- HFO380+tier1_price*emissions_hfo

#tier 2 scenario
ammonia_w_poc <- Price_ammonia+tier2_price*emissions_nh3
hfo_w_poc <- HFO380+tier2_price*emissions_hfo


#EU ETS
#Data from 2022-
ETS_2022 <- ETS[902:1683]
hist(ETS_2022)

#Find model parameters

#2022- model
ETS_2022_log <- log(ETS_2022[2:782]/ETS_2022[1:781])
ETS_2022_log[is.na(ETS_2022_log)] <-0

mu_2022_day <- mean(ETS_2022_log)
sd_2022_day <- sd(ETS_2022_log)

mu_2022 <- mu_2022_day*250
sd_2022 <- sd_2022_day*sqrt(250)


#Full period ( The period before 2022 is not representative)
ETS_log <- log(ETS[2:1683]/ETS_2022[1:1682])
ETS_log[is.na(ETS_log)] <-0

mu_day <- mean(ETS_log)
sd_day <- sd(ETS_log)

mu_full <- mu_day*250
sd_full <- sd_day*sqrt(250)


#Simulate EU ETS in 2050
#using only the 2022- model
ETS_sim <- c()
P_0 <- ETS[1683]
Tmax <- 25

for(i in 1:B){
    eps <- rnorm(Tmax)
    P_t_i <- c(P_0)
    for(j in 1:Tmax){
      P_t_i <- P_t_i*exp((mu_2022-0.5*sd_2022^2)+sd_2022*eps[j])
    }
    ETS_sim[i] <- P_t_i
  
    
}
hist(ETS_sim)
mean(ETS_sim)
sd(ETS_sim)

################################################
##                                            ##
##  Simulate fuel choice and calculate cost   ## 
##                                            ##
################################################

#EU ETS-scenario
ETS_NH3 <- Price_ammonia+ETS_sim*emissions_nh3
ETS_HFO <- HFO380 + ETS_sim*emissions_hfo

ETS_best_fuel <- ETS_NH3<ETS_HFO #True is HFO, False is NH3
ETS_fuel_cost <- as.numeric(unlist(ETS_best_fuel*ETS_HFO+(1-ETS_best_fuel)*ETS_NH3))
hist(sort(ETS_fuel_cost)[1:9800])

#IMO NZF
NZF_NH3 <- Price_ammonia+tier2_price*emissions_nh3
NZF_HFO <- HFO380 + tier2_price*emissions_hfo

NZF_best_fuel <- NZF_NH3<NZF_HFO
NZF_fuel_cost <- as.numeric(unlist(NZF_best_fuel*NZF_HFO+(1-NZF_best_fuel)*NZF_NH3))
hist(NZF_fuel_cost)


##########################################
##                                      ##
##  Calculate cost after pass through   ##
##                                      ##
##########################################

#Baseline freight rate
baseline_freight_rate <- 18155    #Panamax Transatlantic RV as of 14.12.2025 (https://fearnpulse.com/)
baseline_bunker_cost <- 388  #IFO380 as of 14.12.2025 (https://shipandbunker.com/prices/emea/nwe/nl-rtm-rotterdam#IFO380)

#EU ETS
pct_change_cost_ets <- (ETS_fuel_cost-baseline_bunker_cost)/baseline_bunker_cost
pct_change_rate_ETS <- pass_through*pct_change_cost_ets
future_freight_rate_ETS <- baseline_freight_rate*(1+pct_change_rate_ETS)

mean(future_freight_rate_ETS)

#NZF 
pct_change_cost_NZF <- (NZF_fuel_cost-baseline_bunker_cost)/baseline_bunker_cost
pct_change_rate_NZF <- pass_through*pct_change_cost_NZF
future_freight_rate_NZF <- baseline_freight_rate*(1+pct_change_rate_NZF)

mean(future_freight_rate_NZF)



############################
##                        ##  
##  Create output tables   ##
##                        ##
############################
#Best choice for each scenario

mean(ETS_best_fuel)
mean(NZF_best_fuel)

#
#best fuel choice, percentage fossile 
#
stargazer(ETS_best_fuel, summary = T, type = "text")

#
#Cost of best fuel
#
#ETS
mean(ETS_fuel_cost)
sd(ETS_fuel_cost)
sort(ETS_fuel_cost)[B*0.95]
sort(ETS_fuel_cost)[B*0.99]
mean(sort(ETS_fuel_cost)[9500:10000])

#NZF
mean(NZF_fuel_cost)
sd(NZF_fuel_cost)
sort(NZF_fuel_cost)[B*0.95]
sort(NZF_fuel_cost)[B*0.99]
mean(sort(NZF_fuel_cost)[9500:10000])


#
#Cost of HFO fuel
#
#ETS 
mean(ETS_HFO$brent)
sd(ETS_HFO$brent)
sort(ETS_HFO$brent)[B*0.95]
sort(ETS_HFO$brent)[B*0.99]
mean(sort(ETS_HFO$brent)[9500:10000])

#NZF
mean(NZF_HFO$brent)
sd(NZF_HFO$brent)
sort(NZF_HFO$brent)[B*0.95]
sort(NZF_HFO$brent)[B*0.99]
mean(sort(NZF_HFO$brent)[9500:10000])


#
#Cost of NH3 fuel
#
#ETS 
mean(ETS_NH3)
sd(ETS_NH3)
sort(ETS_NH3)[B*0.95]
sort(ETS_NH3)[B*0.99]
mean(sort(ETS_NH3)[9500:10000])

#NZF
mean(ETS_NH3)
sd(ETS_NH3)
sort(ETS_NH3)[B*0.95]
sort(ETS_NH3)[B*0.99]
mean(sort(ETS_NH3)[9500:10000])



#
#Freight rate(USD TCE)
#
#NZF 
mean(future_freight_rate_NZF)
sd(future_freight_rate_NZF)
sorted_NZF <- sort(future_freight_rate_NZF)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])

#EU ETS 
mean(future_freight_rate_ETS)
sd(future_freight_rate_ETS)
sorted_ETS <- sort(future_freight_rate_ETS)
sorted_ETS[B*0.95]
sorted_ETS[B*0.99]
mean(sorted_ETS[9500:10000])

#
#Freight rate (%)
#
#NZF
mean(pct_change_cost_NZF)
sd(pct_change_cost_NZF)
sorted_NZF <- sort(pct_change_cost_ets)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])

#EU ETS 
mean(pct_change_cost_ets)
sd(pct_change_cost_ets)
sorted_NZF <- sort(pct_change_cost_ets)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])




############################
##                        ##
##  Sensitivity analysis  ##
##                        ##
############################

#Lifetime
Price_ammonia_lifetime <- 2.17*(mean(CAPEX+OPEX)/seq(15,30)+9.167*mean(Electricity_price))

#Electricity price
Price_ammonia_electricity <- 2.17*(mean(CAPEX+OPEX)/20+9.167*seq(15,45))

#Capex
Price_ammonia_capex <- 2.17*((seq(500,1500)+mean(OPEX))/20+9.167*mean(Electricity_price))


#Opex
Price_ammonia_opex <- 2.17*((mean(CAPEX)*(1+seq(1,4,by=0.01)))/20+9.167*mean(Electricity_price))

#plot
par(mfrow = c(1, 1)) 
plot(Price_ammonia_lifetime, type = "l", main = "Senstivity - Lifetime", ylab = "USD/tNH3", xlab = "Years", xtick = seq(15,30))
plot(Price_ammonia_electricity, type = "l",ylab = "USD/TNH3", main = "Senstivity - Electricityprice", xlab = "USD/MWh")
plot(Price_ammonia_capex, type = "l",ylab = "USD/TNH3", main = "Senstivity - CAPEX", xlab = "USD/tNH3")
plot(Price_ammonia_opex, type = "l",ylab = "USD/TNH3", main = "Senstivity - OPEX", xlab = "% of CAPEX")

=======
rm(list = ls())
library(readxl)
library(stargazer)


####################
##                ##
## Import data    ##
##                ##
####################

#Import daily bunker data
bunker_daily <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx", sheet = "Daily", col_types = c("date","numeric", "numeric"))
colnames(bunker_daily) <- c("Date", "HFO380", "Brent")

#Import monthly bunker data
bunker_monthly <- read_excel("C:/Users/vegar/Desktop/Pricing climate risk in shipping/fossile_fuel_data.xlsx",sheet = "Monthly", col_types = c("date","numeric", "numeric"))
colnames(bunker_monthly) <- c("Date", "HFO380", "Brent")

#plot fuel
#plot(bunker_daily$Date, bunker_daily$HFO380, type = "l")

#import EU ETS data
icap.graph.carbon.price.data.2008.08.01.2025.08.09 <- read.csv("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/icap-graph-carbon-price-data-2008-08-01-2025-08-09.csv", header=FALSE)
#prerp ETS data
ETS_EUR <- c(icap.graph.carbon.price.data.2008.08.01.2025.08.09$V5[1000:2638], icap.graph.carbon.price.data.2008.08.01.2025.08.09$V10[2639:4081])
date <- icap.graph.carbon.price.data.2008.08.01.2025.08.09$V1[1000:4081]
EUR_USD <- c( icap.graph.carbon.price.data.2008.08.01.2025.08.09$V3[1000:2638],icap.graph.carbon.price.data.2008.08.01.2025.08.09$V8[2639:4081])

ETS_EUR <- as.numeric(ETS_EUR)[1400:3082] #Remove the part of the data with the most null values
EUR_USD <- as.numeric(EUR_USD)[1400:3082]
ETS <- ETS_EUR*EUR_USD
date <- date[1400:3082]



##################
##              ##
##  Parameters  ##
##              ##
##################
B <- 10000
pass_through <- 0.4     #Rule of thumb for dry bulk in the literature 


######################
##                  ##
##  Fossile fuel    ##
##                  ##
######################

#Fits a linear model on the price of bunker based on price of oil
fit <- lm(HFO380~Brent, data = bunker_monthly[bunker_monthly$Date>as.POSIXct("2007-01-01"),])
fit_summary <- summary(fit)
#fit1 <- lm(HFO380~Brent, data = bunker_daily[bunker_daily$Date>as.POSIXct("2007-01-01"),])
#summary(fit1)



#Simulate
scenarios <- c(47.04, 95, 154.92)


path_brent <- data.frame(brent = sample(scenarios, B, T, c(0.25, 0.5, 0.25)))
#colnames(path_brent) = "Brent"

#HFO380 <- predict(fit, newdata = path_brent)
HFO380 <- rnorm(B, fit_summary$coefficients[1,1],fit_summary$coefficients[1,2]) +rnorm(B, fit_summary$coefficients[2,1],fit_summary$coefficients[2,2])*path_brent+rnorm(B,0, sd(fit$residuals))


hist(HFO380$brent)
mean(HFO380$brent)
sd(HFO380$brent)



##########################
##                      ##
##  Zero-emission fuel  ##
##                      ##
##########################

#
#Simulating ammonia production
# https://iea.blob.core.windows.net/assets/2ceb17b8-474f-4154-aab5-4d898f735c17/IEAGHRassumptions_final.pdf
#

#Model 1: with capex and opex with 20 year lifespan
CAPEX <- runif(B, 1000,1100)
OPEX <- runif(B, 0.015,0.03)*CAPEX #Annual
Electricity_price <- runif(B, 20,40) #in Mwh
Price_ammonia <- 2.17*((CAPEX+OPEX)/20+9.167*Electricity_price)

mean(Price_ammonia)
hist(Price_ammonia)



########################
##                    ##
##  Price of carbon   ##
##                    ##
########################

#Emissions in tCo2e/Toe
emissions_hfo <- 3.219
emissions_nh3 <- 0.2219

#IMO NZF
tier1_price <- 100
tier2_price <- 380


#tier 1 scenario
ammonia_w_poc <- Price_ammonia+tier1_price*emissions_nh3
hfo_w_poc <- HFO380+tier1_price*emissions_hfo

#tier 2 scenario
ammonia_w_poc <- Price_ammonia+tier2_price*emissions_nh3
hfo_w_poc <- HFO380+tier2_price*emissions_hfo


#EU ETS
#Data from 2022-
ETS_2022 <- ETS[902:1683]
hist(ETS_2022)

#Find model parameters

#2022- model
ETS_2022_log <- log(ETS_2022[2:782]/ETS_2022[1:781])
ETS_2022_log[is.na(ETS_2022_log)] <-0

mu_2022_day <- mean(ETS_2022_log)
sd_2022_day <- sd(ETS_2022_log)

mu_2022 <- mu_2022_day*250
sd_2022 <- sd_2022_day*sqrt(250)


#Full period ( The period before 2022 is not representative)
ETS_log <- log(ETS[2:1683]/ETS_2022[1:1682])
ETS_log[is.na(ETS_log)] <-0

mu_day <- mean(ETS_log)
sd_day <- sd(ETS_log)

mu_full <- mu_day*250
sd_full <- sd_day*sqrt(250)


#Simulate EU ETS in 2050
#using only the 2022- model
ETS_sim <- c()
P_0 <- ETS[1683]
Tmax <- 25

for(i in 1:B){
    eps <- rnorm(Tmax)
    P_t_i <- c(P_0)
    for(j in 1:Tmax){
      P_t_i <- P_t_i*exp((mu_2022-0.5*sd_2022^2)+sd_2022*eps[j])
    }
    ETS_sim[i] <- P_t_i
  
    
}
hist(ETS_sim)
mean(ETS_sim)
sd(ETS_sim)

################################################
##                                            ##
##  Simulate fuel choice and calculate cost   ## 
##                                            ##
################################################

#EU ETS-scenario
ETS_NH3 <- Price_ammonia+ETS_sim*emissions_nh3
ETS_HFO <- HFO380 + ETS_sim*emissions_hfo

ETS_best_fuel <- ETS_NH3<ETS_HFO #True is HFO, False is NH3
ETS_fuel_cost <- as.numeric(unlist(ETS_best_fuel*ETS_HFO+(1-ETS_best_fuel)*ETS_NH3))
hist(sort(ETS_fuel_cost)[1:9800])

#IMO NZF
NZF_NH3 <- Price_ammonia+tier2_price*emissions_nh3
NZF_HFO <- HFO380 + tier2_price*emissions_hfo

NZF_best_fuel <- NZF_NH3<NZF_HFO
NZF_fuel_cost <- as.numeric(unlist(NZF_best_fuel*NZF_HFO+(1-NZF_best_fuel)*NZF_NH3))
hist(NZF_fuel_cost)


##########################################
##                                      ##
##  Calculate cost after pass through   ##
##                                      ##
##########################################

#Baseline freight rate
baseline_freight_rate <- 18155    #Panamax Transatlantic RV as of 14.12.2025 (https://fearnpulse.com/)
baseline_bunker_cost <- 388  #IFO380 as of 14.12.2025 (https://shipandbunker.com/prices/emea/nwe/nl-rtm-rotterdam#IFO380)

#EU ETS
pct_change_cost_ets <- (ETS_fuel_cost-baseline_bunker_cost)/baseline_bunker_cost
pct_change_rate_ETS <- pass_through*pct_change_cost_ets
future_freight_rate_ETS <- baseline_freight_rate*(1+pct_change_rate_ETS)

mean(future_freight_rate_ETS)

#NZF 
pct_change_cost_NZF <- (NZF_fuel_cost-baseline_bunker_cost)/baseline_bunker_cost
pct_change_rate_NZF <- pass_through*pct_change_cost_NZF
future_freight_rate_NZF <- baseline_freight_rate*(1+pct_change_rate_NZF)

mean(future_freight_rate_NZF)



############################
##                        ##  
##  Create output tables   ##
##                        ##
############################
#Best choice for each scenario

mean(ETS_best_fuel)
mean(NZF_best_fuel)

#
#best fuel choice, percentage fossile 
#
stargazer(ETS_best_fuel, summary = T, type = "text")

#
#Cost of best fuel
#
#ETS
mean(ETS_fuel_cost)
sd(ETS_fuel_cost)
sort(ETS_fuel_cost)[B*0.95]
sort(ETS_fuel_cost)[B*0.99]
mean(sort(ETS_fuel_cost)[9500:10000])

#NZF
mean(NZF_fuel_cost)
sd(NZF_fuel_cost)
sort(NZF_fuel_cost)[B*0.95]
sort(NZF_fuel_cost)[B*0.99]
mean(sort(NZF_fuel_cost)[9500:10000])


#
#Cost of HFO fuel
#
#ETS 
mean(ETS_HFO$brent)
sd(ETS_HFO$brent)
sort(ETS_HFO$brent)[B*0.95]
sort(ETS_HFO$brent)[B*0.99]
mean(sort(ETS_HFO$brent)[9500:10000])

#NZF
mean(NZF_HFO$brent)
sd(NZF_HFO$brent)
sort(NZF_HFO$brent)[B*0.95]
sort(NZF_HFO$brent)[B*0.99]
mean(sort(NZF_HFO$brent)[9500:10000])


#
#Cost of NH3 fuel
#
#ETS 
mean(ETS_NH3)
sd(ETS_NH3)
sort(ETS_NH3)[B*0.95]
sort(ETS_NH3)[B*0.99]
mean(sort(ETS_NH3)[9500:10000])

#NZF
mean(ETS_NH3)
sd(ETS_NH3)
sort(ETS_NH3)[B*0.95]
sort(ETS_NH3)[B*0.99]
mean(sort(ETS_NH3)[9500:10000])



#
#Freight rate(USD TCE)
#
#NZF 
mean(future_freight_rate_NZF)
sd(future_freight_rate_NZF)
sorted_NZF <- sort(future_freight_rate_NZF)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])

#EU ETS 
mean(future_freight_rate_ETS)
sd(future_freight_rate_ETS)
sorted_ETS <- sort(future_freight_rate_ETS)
sorted_ETS[B*0.95]
sorted_ETS[B*0.99]
mean(sorted_ETS[9500:10000])

#
#Freight rate (%)
#
#NZF
mean(pct_change_cost_NZF)
sd(pct_change_cost_NZF)
sorted_NZF <- sort(pct_change_cost_ets)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])

#EU ETS 
mean(pct_change_cost_ets)
sd(pct_change_cost_ets)
sorted_NZF <- sort(pct_change_cost_ets)
sorted_NZF[B*0.95]
sorted_NZF[B*0.99]
mean(sorted_NZF[9500:10000])




############################
##                        ##
##  Sensitivity analysis  ##
##                        ##
############################

#Lifetime
Price_ammonia_lifetime <- 2.17*(mean(CAPEX+OPEX)/seq(15,30)+9.167*mean(Electricity_price))

#Electricity price
Price_ammonia_electricity <- 2.17*(mean(CAPEX+OPEX)/20+9.167*seq(15,45))

#Capex
Price_ammonia_capex <- 2.17*((seq(500,1500)+mean(OPEX))/20+9.167*mean(Electricity_price))


#Opex
Price_ammonia_opex <- 2.17*((mean(CAPEX)*(1+seq(1,4,by=0.01)))/20+9.167*mean(Electricity_price))

#plot
par(mfrow = c(1, 1)) 
plot(Price_ammonia_lifetime, type = "l", main = "Senstivity - Lifetime", ylab = "USD/tNH3", xlab = "Years", xtick = seq(15,30))
plot(Price_ammonia_electricity, type = "l",ylab = "USD/TNH3", main = "Senstivity - Electricityprice", xlab = "USD/MWh")
plot(Price_ammonia_capex, type = "l",ylab = "USD/TNH3", main = "Senstivity - CAPEX", xlab = "USD/tNH3")
plot(Price_ammonia_opex, type = "l",ylab = "USD/TNH3", main = "Senstivity - OPEX", xlab = "% of CAPEX")

>>>>>>> 8269d2a5ef16ea27896714e05893d0fa3128d894
