<<<<<<< HEAD
#OLD

#Script for å modellere drivstoffovergangen
#rm(list = ls())


#make sure to run the simulate EU ETS script first
P_T

#Parameters
B <- 5000
fuel_price_median <- c(373, 523, 217, 383, 128, 756, 756)
fuel_price_q1 <- c(231, 303, 215, 183, 724, 675, 675)
fuel_price_q3 <- c(686, 948, 220, 582,  1839, 1070, 1070)

co2 <- c(3.18, 3.31, 2.34, 2.88, 3.19,0,0) #IFO,MGO, LNG, Methanol, Biodiesel, LH2 og NH3
fuel_name <- c("IFO", "MGO", "LNG", "Methanol", "Biodiesel", "LH2", "NH3" )

#using method C3 described in Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range
fuel_price_avg <- (fuel_price_q1 + fuel_price_median + fuel_price_q3)/3
fuel_price_se <- (fuel_price_q3-fuel_price_q1)/1.35 #When n goes to infinity the denominator converges to 1.35


#log normal?
fuel_price_avg_log <- log(fuel_price_avg/sqrt(1+fuel_price_se^2/fuel_price_avg^2))
fuel_price_se_log <- sqrt(log(1+fuel_price_se^2/fuel_price_avg^2))


#Drawing random prices
fuel_price_path <- matrix(rep(0,B*7), nrow = 7)
for(i in 1:7){
  temp <- rep(0,B)
  #temp <- rnorm(B, fuel_price_avg[i], fuel_price_se)
  temp <- rlnorm(B, fuel_price_avg_log[i], fuel_price_se_log)
  fuel_price_path[i,] <- temp
  }
dim(fuel_price_path)


hist(fuel_price_path[1,])
hist(fuel_price_path[2,])
hist(fuel_price_path[3,])
hist(fuel_price_path[4,])
hist(fuel_price_path[5,])
hist(fuel_price_path[6,])
hist(fuel_price_path[7,])



##Finding the optimal fuel
choice_of_fuel <- c()
fuel_cost_with_ETS <- c()
for(j in 1:B){
  #calculate cost
  C <- fuel_price_path[,j]+co2*P_T[j]
  best_fuel <- which.min(C)
  
  min_cos <- C[best_fuel]
  
  choice_of_fuel[j] <- best_fuel
  fuel_cost_with_ETS[j] <- min_cos
}

hist(choice_of_fuel, xticks = fuel_name)


#Results
table(choice_of_fuel)/B
hist(fuel_cost_with_ETS)
sort(fuel_cost_with_ETS)[B*0.9]
sort(fuel_cost_with_ETS)[B*0.95]
sort(fuel_cost_with_ETS)[B*0.99]

=======
#OLD

#Script for å modellere drivstoffovergangen
#rm(list = ls())


#make sure to run the simulate EU ETS script first
P_T

#Parameters
B <- 5000
fuel_price_median <- c(373, 523, 217, 383, 128, 756, 756)
fuel_price_q1 <- c(231, 303, 215, 183, 724, 675, 675)
fuel_price_q3 <- c(686, 948, 220, 582,  1839, 1070, 1070)

co2 <- c(3.18, 3.31, 2.34, 2.88, 3.19,0,0) #IFO,MGO, LNG, Methanol, Biodiesel, LH2 og NH3
fuel_name <- c("IFO", "MGO", "LNG", "Methanol", "Biodiesel", "LH2", "NH3" )

#using method C3 described in Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range
fuel_price_avg <- (fuel_price_q1 + fuel_price_median + fuel_price_q3)/3
fuel_price_se <- (fuel_price_q3-fuel_price_q1)/1.35 #When n goes to infinity the denominator converges to 1.35


#log normal?
fuel_price_avg_log <- log(fuel_price_avg/sqrt(1+fuel_price_se^2/fuel_price_avg^2))
fuel_price_se_log <- sqrt(log(1+fuel_price_se^2/fuel_price_avg^2))


#Drawing random prices
fuel_price_path <- matrix(rep(0,B*7), nrow = 7)
for(i in 1:7){
  temp <- rep(0,B)
  #temp <- rnorm(B, fuel_price_avg[i], fuel_price_se)
  temp <- rlnorm(B, fuel_price_avg_log[i], fuel_price_se_log)
  fuel_price_path[i,] <- temp
  }
dim(fuel_price_path)


hist(fuel_price_path[1,])
hist(fuel_price_path[2,])
hist(fuel_price_path[3,])
hist(fuel_price_path[4,])
hist(fuel_price_path[5,])
hist(fuel_price_path[6,])
hist(fuel_price_path[7,])



##Finding the optimal fuel
choice_of_fuel <- c()
fuel_cost_with_ETS <- c()
for(j in 1:B){
  #calculate cost
  C <- fuel_price_path[,j]+co2*P_T[j]
  best_fuel <- which.min(C)
  
  min_cos <- C[best_fuel]
  
  choice_of_fuel[j] <- best_fuel
  fuel_cost_with_ETS[j] <- min_cos
}

hist(choice_of_fuel, xticks = fuel_name)


#Results
table(choice_of_fuel)/B
hist(fuel_cost_with_ETS)
sort(fuel_cost_with_ETS)[B*0.9]
sort(fuel_cost_with_ETS)[B*0.95]
sort(fuel_cost_with_ETS)[B*0.99]

>>>>>>> 8269d2a5ef16ea27896714e05893d0fa3128d894
