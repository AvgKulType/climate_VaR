library(queueing)
rm(list = ls())
# #Play with the western bulk data
# TOP_6_ports_by_frequency_added_actual_daily_hire <- read_excel("PhD/TOP 6 ports by frequency - added actual daily hire.xlsx")
# 
# 
# 
# #remove non-weather related delays
# X <- TOP_6_ports_by_frequency_added_actual_daily_hire[TOP_6_ports_by_frequency_added_actual_daily_hire$exclusion_category=="WEATHER/SEA STATE RELATED EXCLUSION",]
# 
# #group delays that happens at the same point in time
# 
# 
# 
# delays <- c()
# for(i in 1:length(unique(X$voyage_code))){
#   pc <-X$number_of_days_adj[X$voyage_code ==unique(X$voyage_code)[i]]
#   delays[i] <- sum(pc)
# }
# #empirical delays
# mean(delays)
# sd(delays)
# 

#
#Log normal parameters
#


#Camus parameters: x_bar=198.9, s_har = 2.625 and x_bar = 332.13, s_hat =38.61
#Area 1
#x_bar_pres =198.9
#s_hat_pres = 2.625 


#x_bar_cc =332.13
#s_hat_cc = 38.61

#Area 2
x_bar_pres =313.62
s_hat_pres = 4.57^2

x_bar_cc = 475.14
s_hat_cc = 49.604



mu_pres = log(x_bar_pres/sqrt(1+s_hat_pres^2 /x_bar_pres^2))
s_pres = sqrt(log(1+s_hat_pres^2/x_bar_pres^2))

mu_cc = log(x_bar_cc/sqrt(1+s_hat_cc^2 /x_bar_cc^2))
s_cc = sqrt(log(1+s_hat_cc^2/x_bar_cc^2))

##
##
## Simulate expected time at port 
##
##
set.seed(69420)
# 
# Erlang <- function(k, lambda, mu){
#   rho <- lambda/(k*mu)
#   a <- 1-rho
#   b <- factorial(k)/(k*rho)^k
#   
#   js <- seq(0,k-1)
#   c <- sum((k*rho)^js /factorial(js))
#   return(1/(1+a*b*c))
# }
# 


#paramters
B <- 1000  #Number of simulations

#"Smaller than Rotterdam" senario
k <- 180 #inital_berths
lambda <- 2.28 #Average number of arriving vessels
mu <- 0.014 #Service rate (72 hour average service time)

#Loosly based on Valencia
#lambda <- 20.5
#mu <- 1/3
#k <- 40
#k <- 110


#Alexandria parameters (with climate change mean being 1.67 times present mean and sd 14 times greater)
#lambda <- 5.68
#mu <- 0.18
#k <- 35
lambda/(mu*k)

time_at_port_present <-c()
time_at_port_climate_change <-c()
port_capacity_present <- c()
port_capacity_cc<- c()


for(i in 1:B){
  print(B-i)
  port_capacity_present[i] <- round(k*(8760-rlnorm(1, mu_pres, s_pres))/8760)  #Is there another function we can use??
  port_capacity_cc[i] <- round(k*(8760-rlnorm(1, mu_cc, s_cc))/8760)

  
  present <- NewInput.MMCK(lambda, mu, port_capacity_present[i],k=500)
  Q_present <- QueueingModel(present)
  time_at_port_present[i] <- Q_present$W
  
  future <- NewInput.MMCK(lambda, mu, port_capacity_cc[i],k=500)
  Q_future <- QueueingModel(future)
  time_at_port_climate_change[i]<- Q_future$W
}

a <- mean(time_at_port_present)
b <- mean(time_at_port_climate_change)
c <- sd(time_at_port_present)
d <- sd(time_at_port_climate_change)
e <- sort(time_at_port_present)[B*0.95]
f <- sort(time_at_port_climate_change)[B*0.95]

df <- data.frame(matrix(c(a,b,c,d,e,f),nrow = 2))
df

plot(sort(time_at_port_present))
plot(sort(time_at_port_climate_change))

#hist(time_at_port_climate_change)
#hist(time_at_port_present)
