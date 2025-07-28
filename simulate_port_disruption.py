"""
This script simulates M/M/c/K queues with non-premptive priority with p priority classes 
"""


import numpy as np



#parameters
mu = np.array([0.14,0.14])   #vessel service times
l = np.array([2.28, 1]) #Vessel arrival rate

k = 180 


#Drawing number of arriving vessels of each type

print("Hello world")