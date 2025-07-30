"""
This script simulates M/M/k/infty queues with non-premptive priority with 2 priority classes

"""


import numpy as np
import matplotlib.pyplot as plt 
import pandas as pd


#Simulation parameters
mu = np.array([0.14,0.14])   #vessel service times (vessels per hour)
l = np.array([1, 2.28]) #Vessel arrival rate (vessels per hour)

k = 18 #180
p = 2

assert len(mu) == len(l) and len(l)==p, "mu and l must have a length equal to p"


##
##
## Creating the vessel database
##
##
rng = np.random.default_rng()
#number_of_vessels = rng.poisson(l*24*365, size=(p))  # Draw arrivals for each vessel type over k time steps
number_of_vessels = rng.poisson(l*100, size=(p))  # Draw arrivals for each vessel type over k time steps

#Creating vessel type
vessel_type = np.array((), dtype=int)  # Create an array for vessel type, 0 is normal, 1 is priority
for i in range(p):
    vessel_type = np.concatenate((vessel_type, np.ones(number_of_vessels[i]) * i))  # Create an array for vessel type



#creating vessel ID
vessel_ID = np.zeros(len(vessel_type), dtype=int)  # Create an array for vessel ID, initialized to zeros
for i in range(len(vessel_type)):
    vessel_ID[i] = int(i)  # Create an array for vessel ID, initialized to zeros


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
    service_times_p = rng.exponential(1/mu[i], size=number_of_vessels[i])
#    arrival_timestamps_p = np.cumsum(arrival_times_p)  # Cumulative sum of interarrival times
    
    arrival_times_tmp.append(arrival_times_p)  # Draw interarrival times for each vessel type
    service_times_tmp.append(service_times_p)  # Draw service times for each vessel type
#    arrival_timestamps_tmp.append(arrival_timestamps_p)  # Cumulative sum of interarrival times for each vessel type

arrival_times = np.array(arrival_times_tmp, dtype = np.ndarray)
service_times = np.array(service_times_tmp, dtype = np.ndarray)

#print(np.mean(arrival_times[0]), np.mean(arrival_times[1]))

#Finding the arrival timestamps
arrival_timestamps = np.array(())  # Initialize an array for arrival timestamps
service_time = np.array(())  # Initialize an array for service timestamps
for i in range(p):
    arrival_timestamps = np.concatenate((arrival_timestamps, np.cumsum(arrival_times[i])))  # Cumulative sum of interarrival times for each vessel type
    service_time = np.concatenate((service_time, service_times[i]))  # Cumulative sum of service times for each vessel type
    #arrival_timestamps[i, number_of_vessels[i]:] = np.nan  # Fill the rest with NaN to keep the shape consistent



#Setting up the vessels dataframe
vessels["arrival_timestamps"] = arrival_timestamps
vessels["time in service"] = service_time
vessels["Enter queue timestamp"] = np.zeros(vessels.shape[0])  # Initialize the Enter queue timestamp column
vessels["Enter berth timestamp"] = np.zeros(vessels.shape[0])  # Initialize the Enter berth timestamp column
vessels["Exit berth timestamp"] = np.zeros(vessels.shape[0])  # Initialize the Exit berth timestamp column
vessels["In queue"] = np.zeros(vessels.shape[0])  # Initialize the Exit berth timestamp column
#vessels.to_csv('data.txt', sep='-', index=False)


#print(vessels["vessel_ID"])
#plt.plot(vessels["arrival_timestamps"])
#plt.hist(vessels["service_times"], bins=50, alpha=0.5, label='Service Times')
#plt.show()


##
##
##  setting up the simulator
##
##

#Columns to add to the vessels dataframe
#Enter queue timestamp
#Enter berth timestamp 
#Exit berth timestamp

#Then the following columns can be calculated:
#Time in queue
#Time in system 

vessels_in_queue = [[] for i in range(p)]  # List to keep track of vessels in the queue
vessels_at_berth = []  # List to keep track of vessels at berth (BY ID)
leaving_vessels = []  # List to keep track of vessels that are leaving the berth (BY ID)
num_vessels_at_berth = 0  # Counter for the number of vessels at berth

for i in range(vessels.shape[0]):
    #print(vessels.shape[0]-i)
    #Determine the current time stamp
    current_vessel = vessels.iloc[i]  # Get the current vessel
    current_time = vessels.loc[i,"arrival_timestamps"]  # Get the current time stamp


    #Check if there is available berths, and add the vessel to berth if it is available
    if len(vessels_at_berth) < k:
        vessels.loc[i,"Enter queue timestamp"] = current_time  # Record the Enter queue timestamp
        vessels.loc[i,"Enter berth timestamp"] = current_time  # Record the Enter berth timestamp
        vessels.loc[i,"Exit berth timestamp"] = current_time + vessels.loc[i,"time in service"] # Record the Exit timestamp  
        vessels_at_berth.append(vessels.loc[i,"vessel_ID"])   #Add vessel ID to the berth list  
        #num_vessels_at_berth += 1
        # print(current_time)
        # print(vessels["time in service"].iloc[i])
        # print(vessels_at_berth)
    
    #If there are no available berths, add the vessel to the the appropriate queue
    elif len(vessels_at_berth) >= k:
        print("No available berth, adding vessel to queue")
        current_vessel_priority = vessels.loc[i,"vessel_type"]  # Get the priority of the vessel
        current_vessel_ID = vessels.loc[i,"vessel_ID"] # Get the ID of the vessel
        #print(current_vessel_ID)
        vessels_in_queue[int(current_vessel_priority)].append(current_vessel_ID)    #Add vessel to the queue
        vessels.loc[i,"Enter queue timestamp"] = current_time  # Record the Enter queue timestamp       
        vessels.loc[i, "In queue"] = 1
    


    #Check if any vessels will leave during the next period
    leaving_vessels = []
    print(len(vessels_at_berth))
    print(len(vessels_in_queue[0]))
    print(len(vessels_in_queue[1]))
    print(" ")
    for j in range(len(vessels_at_berth)):
        #print("lmaos")
        vessel_at_berth_tmp = vessels.iloc[vessels_at_berth[j]] # The vessel at berth that is to be checked
        #print(vessel_at_berth_tmp["Exit berth timestamp"].values[0])
        if vessel_at_berth_tmp["Exit berth timestamp"]<= current_time:
            #remove the vessel from the berth (by adding the index to the leaving_vessels list) 
            leaving_vessels.append(vessels_at_berth[j])
            #num_vessels_at_berth -= 1
            #taking the highest priority vessel from the queue
            for q in range(p):
                #check if there are any vessels in the queue of priority k
                if len(vessels_in_queue[q]) > 0 and len(vessels_at_berth) < k:
                    next_vessel_in_ID = vessels_in_queue[q][0] #get vessel ID of the next vessel in the queue
                    service_time = vessels.loc[next_vessel_in_ID,"time in service"]
                    vessels_at_berth.append(next_vessel_in_ID)

                    #calculating the exit time for the vessel entering the berth
                    vessels.loc[next_vessel_in_ID,"Enter berth timestamp"] = current_time   # Record the timestamp for the vessel entering the bert
                    vessels.loc[next_vessel_in_ID,"Exit berth timestamp"] = current_time + service_time  # Record the timestamp for when the vessel exits the berth
                    vessels_in_queue[q].pop(0)  # Remove the vessel from the queue
                    #num_vessels_at_berth += 1
                    #break #stop looking for a vessel in the queue, since we have found one
    #remove the vessel from the berth list
    for a in range(len(leaving_vessels)):
        vessels_at_berth.remove(leaving_vessels[a])


#Resolve the queue when all the vessels have arrived 



#output the databaser to test
vessels.to_csv('data.csv', sep = ";",index=False)
