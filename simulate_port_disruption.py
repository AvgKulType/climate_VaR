"""
This script simulates M/M/k/infty queues with non-premptive priority with 2 priority classes

"""


import numpy as np
import matplotlib.pyplot as plt 
import pandas as pd


#Simulation parameters
mu = np.array([0.14,0.14])   #vessel service times (vessels per hour)
l = np.array([2.28, 0.5]) #Vessel arrival rate (vessels per hour)

k = 180 
p = 2

assert len(mu) == len(l) and len(l)==p, "mu and l must have a length equal to p"


##
##
## Creating the vessel database
##
##
rng = np.random.default_rng()
number_of_vessels = rng.poisson(l*24*365, size=(p))  # Draw arrivals for each vessel type over k time steps

#Creating vessel type
vessel_type = np.array((), dtype=int)  # Create an array for vessel type, 0 is normal, 1 is priority
for i in range(p):
    vessel_type = np.concatenate((vessel_type, np.ones(number_of_vessels[i]) * i))  # Create an array for vessel type



#creating vessel ID
vessel_ID = np.zeros(len(vessel_type), dtype=int)  # Create an array for vessel ID, initialized to zeros
for i in range(len(vessel_type)):
    vessel_ID[i] = i  # Create an array for vessel ID, initialized to zeros


#Creating the vessel database
vessels = pd.DataFrame({
    'vessel_ID': vessel_ID,
    'vessel_type': vessel_type})

#print(number_of_vessels)
#print(sum(number_of_vessels))

#Drawing interarrival times and service times
arrival_times_tmp = []  # Draw interarrival times for each vessel type
service_times_tmp = []  # Draw service times for each vessel type
for i in range(p):
    arrival_times_p = rng.exponential(1/l[i], size=number_of_vessels[i])
    #print(arrival_times_p)
    service_times_p = rng.exponential(mu[i], size=number_of_vessels[i])
#    arrival_timestamps_p = np.cumsum(arrival_times_p)  # Cumulative sum of interarrival times
    
    arrival_times_tmp.append(arrival_times_p)  # Draw interarrival times for each vessel type
    service_times_tmp.append(service_times_p)  # Draw service times for each vessel type
#    arrival_timestamps_tmp.append(arrival_timestamps_p)  # Cumulative sum of interarrival times for each vessel type

arrival_times = np.array(arrival_times_tmp, dtype = np.ndarray)
service_times = np.array(service_times_tmp, dtype = np.ndarray)

#print(np.mean(arrival_times[0]), np.mean(arrival_times[1]))

#Finding the arrival timestamps
arrival_timestamps = np.array(())  # Initialize an array for arrival timestamps
for i in range(p):
    arrival_timestamps = np.concatenate((arrival_timestamps, np.cumsum(arrival_times[i])))  # Cumulative sum of interarrival times for each vessel type
    #arrival_timestamps[i, number_of_vessels[i]:] = np.nan  # Fill the rest with NaN to keep the shape consistent



vessels["arrival_timestamps"] = arrival_timestamps
vessels.to_csv('data.txt', sep='-', index=False)


print(vessels["vessel_ID"])
plt.plot(vessels["arrival_timestamps"])
plt.show()


##
##
##  setting up the simulator
##
##


vessels_in_queue = []  # List to keep track of vessels in the queue
vessels_at_berth = []  # List to keep track of vessels at berth
vessel_exit_times = np.zeros()  # List to keep track of vessel exit times
# for i in range(len(timesteps_merged)):
#     #Determine the current time stamp
#     current_time = timesteps_merged[i]
#     current_vessel_type = 0 if current_time in arrival_timestamps[0] else 1  # Determine the vessel type based on the timestamp. 0 is normal, 1 is priority

#     #Check if there is available berths, and add the vessel to berth if it is available
#     if len(vessels_at_berth) >=k:


#     #If there are no available berths, add the vessel to the queue

#     #Check if any vessels will eave during the next period
    


