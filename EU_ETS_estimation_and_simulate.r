<<<<<<< HEAD
library(moments)
set.seed(69420)

#Import data
icap.graph.carbon.price.data.2008.08.01.2025.08.09 <- read.csv("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/icap-graph-carbon-price-data-2008-08-01-2025-08-09.csv", header=FALSE)

#prepp data
carbon_prices_eur <- c(icap.graph.carbon.price.data.2008.08.01.2025.08.09$V5[1000:2638], icap.graph.carbon.price.data.2008.08.01.2025.08.09$V10[2639:4081])
date <- icap.graph.carbon.price.data.2008.08.01.2025.08.09$V1[1000:4081]
EUR_USD <- c( icap.graph.carbon.price.data.2008.08.01.2025.08.09$V3[1000:2638],icap.graph.carbon.price.data.2008.08.01.2025.08.09$V8[2639:4081])

carbon_prices_eur <- as.numeric(carbon_prices_eur)[1400:3082] #Remove the part of the data with the most null values
EUR_USD <- as.numeric(EUR_USD)[1400:3082]
carbon_prices <- carbon_prices_eur#*EUR_USD

#plot
plot(EUR_USD, type = "l")
plot(as.Date(date[1400:3082]),
     carbon_prices,
     type = "l",
     main = "Price of Carbon under the EU ETS",
     xlab = "time",
     ylab = "Price (USD/tonne Co2)")
#Find the paramters of the model

carbon_price_log <- log(carbon_prices[2:1683]/carbon_prices[1:1682])
carbon_price_log[is.na(carbon_price_log)] <-0

mu_day <- mean(carbon_price_log)
sd_day <- sd(carbon_price_log)

mu <- mu_day*250
sd <- sd_day*sqrt(250)


#Simulating
B <- 5000
P_0 <- 68.95 # the price on 30.06.2025
dt <- 1
#P_T <- P_0*rlnorm(B, mu*25, sd*sqrt(25))
Tmax <- 25 
P_t <- matrix(nrow = B, ncol = Tmax+1)

for(i in 1:B){
  eps <- rnorm(Tmax)
  P_t_i <- c(P_0)
  for(j in 1:Tmax){
    P_t_i[j+1] <- P_t_i[j]*exp((mu-0.5*sd^2)+sd*eps[j])
  }
  P_t[i,] <- P_t_i
  
  
}

#


#plot a couple of sample paths
plot(P_t[1,], type = "l")

#Discriptive statistics for P_T 
P_T <- P_t[,(Tmax+1)]
mean(P_T)
sd(P_T)
boxplot(P_T)

hist(P_T,
     100,
     freq = F,
     main = "Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


hist(sort(P_T)[1:4000],
     100,
     freq = F,
     main = "Truncated Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


sort(P_T)[B*0.9]
sort(P_T)[B*0.95]
sort(P_T)[B*0.99]



##
##  Bootstrap emprical simulation
##
##
# B <- 5000
# P_0 <- 68.95 # the price on 30.06.2025
# Tmax <- 250*25
# P_t <- matrix(nrow = B, ncol = Tmax+1)
# 
# P_t[,1]  <- P_0
# for(i in 1:B){
#   dP <- sample(carbon_price_log, Tmax, replace=T)
#   P_t[i,2:(Tmax+1)] <- P_0*exp(cumsum(dP))
#   
# }
# P_T <- P_t[,(Tmax+1)]
# mean(P_T)
# median(P_T)
# sd(P_T)
# kurtosis(P_T)
# skewness(P_T)
# 
# boxplot(P_T)
# 
# hist(P_T,100, main = "Histogram of Simulated Price of Carbon in 2050",xlab = "")
# 
# sort(P_T)[B*0.9]
# sort(P_T)[B*0.95]
# sort(P_T)[B*0.99]
=======
<<<<<<< HEAD
library(moments)
set.seed(69420)

#Import data
icap.graph.carbon.price.data.2008.08.01.2025.08.09 <- read.csv("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/icap-graph-carbon-price-data-2008-08-01-2025-08-09.csv", header=FALSE)

#prepp data
carbon_prices_eur <- c(icap.graph.carbon.price.data.2008.08.01.2025.08.09$V5[1000:2638], icap.graph.carbon.price.data.2008.08.01.2025.08.09$V10[2639:4081])
date <- icap.graph.carbon.price.data.2008.08.01.2025.08.09$V1[1000:4081]
EUR_USD <- c( icap.graph.carbon.price.data.2008.08.01.2025.08.09$V3[1000:2638],icap.graph.carbon.price.data.2008.08.01.2025.08.09$V8[2639:4081])

carbon_prices_eur <- as.numeric(carbon_prices_eur)[1400:3082] #Remove the part of the data with the most null values
EUR_USD <- as.numeric(EUR_USD)[1400:3082]
carbon_prices <- carbon_prices_eur#*EUR_USD

#plot
plot(EUR_USD, type = "l")
plot(as.Date(date[1400:3082]),
     carbon_prices,
     type = "l",
     main = "Price of Carbon under the EU ETS",
     xlab = "time",
     ylab = "Price (USD/tonne Co2)")
#Find the paramters of the model

carbon_price_log <- log(carbon_prices[2:1683]/carbon_prices[1:1682])
carbon_price_log[is.na(carbon_price_log)] <-0

mu_day <- mean(carbon_price_log)
sd_day <- sd(carbon_price_log)

mu <- mu_day*250
sd <- sd_day*sqrt(250)


#Simulating
B <- 5000
P_0 <- 68.95 # the price on 30.06.2025
dt <- 1
#P_T <- P_0*rlnorm(B, mu*25, sd*sqrt(25))
Tmax <- 25 
P_t <- matrix(nrow = B, ncol = Tmax+1)

for(i in 1:B){
  eps <- rnorm(Tmax)
  P_t_i <- c(P_0)
  for(j in 1:Tmax){
    P_t_i[j+1] <- P_t_i[j]*exp((mu-0.5*sd^2)+sd*eps[j])
  }
  P_t[i,] <- P_t_i
  
  
}

#


#plot a couple of sample paths
plot(P_t[1,], type = "l")

#Discriptive statistics for P_T 
P_T <- P_t[,(Tmax+1)]
mean(P_T)
sd(P_T)
boxplot(P_T)

hist(P_T,
     100,
     freq = F,
     main = "Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


hist(sort(P_T)[1:4000],
     100,
     freq = F,
     main = "Truncated Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


sort(P_T)[B*0.9]
sort(P_T)[B*0.95]
sort(P_T)[B*0.99]



##
##  Bootstrap emprical simulation
##
##
# B <- 5000
# P_0 <- 68.95 # the price on 30.06.2025
# Tmax <- 250*25
# P_t <- matrix(nrow = B, ncol = Tmax+1)
# 
# P_t[,1]  <- P_0
# for(i in 1:B){
#   dP <- sample(carbon_price_log, Tmax, replace=T)
#   P_t[i,2:(Tmax+1)] <- P_0*exp(cumsum(dP))
#   
# }
# P_T <- P_t[,(Tmax+1)]
# mean(P_T)
# median(P_T)
# sd(P_T)
# kurtosis(P_T)
# skewness(P_T)
# 
# boxplot(P_T)
# 
# hist(P_T,100, main = "Histogram of Simulated Price of Carbon in 2050",xlab = "")
# 
# sort(P_T)[B*0.9]
# sort(P_T)[B*0.95]
# sort(P_T)[B*0.99]
=======
library(moments)
set.seed(69420)

#Import data
icap.graph.carbon.price.data.2008.08.01.2025.08.09 <- read.csv("C:/Users/vegar/Desktop/Pricing climate risk in shipping/Data/icap-graph-carbon-price-data-2008-08-01-2025-08-09.csv", header=FALSE)

#prepp data
carbon_prices_eur <- c(icap.graph.carbon.price.data.2008.08.01.2025.08.09$V5[1000:2638], icap.graph.carbon.price.data.2008.08.01.2025.08.09$V10[2639:4081])
date <- icap.graph.carbon.price.data.2008.08.01.2025.08.09$V1[1000:4081]
EUR_USD <- c( icap.graph.carbon.price.data.2008.08.01.2025.08.09$V3[1000:2638],icap.graph.carbon.price.data.2008.08.01.2025.08.09$V8[2639:4081])

carbon_prices_eur <- as.numeric(carbon_prices_eur)[1400:3082] #Remove the part of the data with the most null values
EUR_USD <- as.numeric(EUR_USD)[1400:3082]
carbon_prices <- carbon_prices_eur#*EUR_USD

#plot
plot(EUR_USD, type = "l")
plot(as.Date(date[1400:3082]),
     carbon_prices,
     type = "l",
     main = "Price of Carbon under the EU ETS",
     xlab = "time",
     ylab = "Price (USD/tonne Co2)")
#Find the paramters of the model

carbon_price_log <- log(carbon_prices[2:1683]/carbon_prices[1:1682])
carbon_price_log[is.na(carbon_price_log)] <-0

mu_day <- mean(carbon_price_log)
sd_day <- sd(carbon_price_log)

mu <- mu_day*250
sd <- sd_day*sqrt(250)


#Simulating
B <- 5000
P_0 <- 68.95 # the price on 30.06.2025
dt <- 1
#P_T <- P_0*rlnorm(B, mu*25, sd*sqrt(25))
Tmax <- 25 
P_t <- matrix(nrow = B, ncol = Tmax+1)

for(i in 1:B){
  eps <- rnorm(Tmax)
  P_t_i <- c(P_0)
  for(j in 1:Tmax){
    P_t_i[j+1] <- P_t_i[j]*exp((mu-0.5*sd^2)+sd*eps[j])
  }
  P_t[i,] <- P_t_i
  
  
}

#


#plot a couple of sample paths
plot(P_t[1,], type = "l")

#Discriptive statistics for P_T 
P_T <- P_t[,(Tmax+1)]
mean(P_T)
sd(P_T)
boxplot(P_T)

hist(P_T,
     100,
     freq = F,
     main = "Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


hist(sort(P_T)[1:4000],
     100,
     freq = F,
     main = "Truncated Histogram of Simulated Price of Carbon in 2050",
     xlab = "",
     ylab = "Price (USD/tonne Co2)")


sort(P_T)[B*0.9]
sort(P_T)[B*0.95]
sort(P_T)[B*0.99]



##
##  Bootstrap emprical simulation
##
##
# B <- 5000
# P_0 <- 68.95 # the price on 30.06.2025
# Tmax <- 250*25
# P_t <- matrix(nrow = B, ncol = Tmax+1)
# 
# P_t[,1]  <- P_0
# for(i in 1:B){
#   dP <- sample(carbon_price_log, Tmax, replace=T)
#   P_t[i,2:(Tmax+1)] <- P_0*exp(cumsum(dP))
#   
# }
# P_T <- P_t[,(Tmax+1)]
# mean(P_T)
# median(P_T)
# sd(P_T)
# kurtosis(P_T)
# skewness(P_T)
# 
# boxplot(P_T)
# 
# hist(P_T,100, main = "Histogram of Simulated Price of Carbon in 2050",xlab = "")
# 
# sort(P_T)[B*0.9]
# sort(P_T)[B*0.95]
# sort(P_T)[B*0.99]
>>>>>>> 7adb83d50cbaa460565a66ecc5570c65560fd671
>>>>>>> 8269d2a5ef16ea27896714e05893d0fa3128d894
