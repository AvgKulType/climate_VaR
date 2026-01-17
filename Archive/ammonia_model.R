<<<<<<< HEAD
rm(list = ls())

#
#Simulating ammonia production
# https://iea.blob.core.windows.net/assets/2ceb17b8-474f-4154-aab5-4d898f735c17/IEAGHRassumptions_final.pdf
#

#Parameters
B <- 10000



#Model 1: with capex and opex with 20 year lifespann
CAPEX <- runif(B, 1000,1100)
OPEX <- runif(B, 0.015,0.03)*CAPEX #Annual
Electricity_price <- runif(B, 20,40) #in Mwh
Price_ammonia <- 2.17*((CAPEX+OPEX)/20+9.167*Electricity_price)

mean(Price_ammonia)
hist(Price_ammonia)


#
#sensitivity analysis
#
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

#Model 2: A direct fuel cost from the table on electricity generation
#Price_ammonia <- 2.17*runif(B, 300,500)


#model 3: Price the electricity
#Price_ammonia <- 2.17*9.167*Electricity_price
#mean(Price_ammonia)
=======
rm(list = ls())

#
#Simulating ammonia production
# https://iea.blob.core.windows.net/assets/2ceb17b8-474f-4154-aab5-4d898f735c17/IEAGHRassumptions_final.pdf
#

#Parameters
B <- 10000



#Model 1: with capex and opex with 20 year lifespann
CAPEX <- runif(B, 1000,1100)
OPEX <- runif(B, 0.015,0.03)*CAPEX #Annual
Electricity_price <- runif(B, 20,40) #in Mwh
Price_ammonia <- 2.17*((CAPEX+OPEX)/20+9.167*Electricity_price)

mean(Price_ammonia)
hist(Price_ammonia)


#
#sensitivity analysis
#
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

#Model 2: A direct fuel cost from the table on electricity generation
#Price_ammonia <- 2.17*runif(B, 300,500)


#model 3: Price the electricity
#Price_ammonia <- 2.17*9.167*Electricity_price
#mean(Price_ammonia)
>>>>>>> 8269d2a5ef16ea27896714e05893d0fa3128d894
