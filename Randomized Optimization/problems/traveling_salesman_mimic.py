# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Traveling Salesman Problem - MIMIC
from mimicry import Mimic
import math

iterations = 15

cities = ["Gotham","Metropolis","Smallville","Hill Valley",
          "Mayberry","South Park","Los Santos","Vice City",
          "Racoon City","Charming","Granville","Coast City"]
xcoord = [2,17,7,3,8,10,6,8,3,5,1,8]
ycoord = [7,2,5,13,1,9,2,8,4,11,19,3]

def city_dist(start,end):
    global xcoord
    global ycoord
    return math.sqrt((xcoord[start] - xcoord[end])**2 + (ycoord[start] - ycoord[end])**2)

def travel_dist(route):
    for i in range(0,len(cities)):
        if sum(route[i::len(cities)]) != 1:
            return 0
        if sum(route[i:i+len(cities):]) != 1:
            return 0
    distance = 0
    for i in range(0,len(route)):
        if (route[i] == 0):
            next
        start = int(math.ceil((i+.1)/12)-1)
        end = int(i-(start)*12)
        #if (start == end):
        #    return 0
        distance += city_dist(start,end)
    return distance

limits = [(0,1)]*(len(cities)**2)

optimizer = Mimic(limits,travel_dist)

for i in range(1,iterations):
    optimal_paths = optimizer.fit()

optimal_dist = 0
optimal_path = 'None'

for path in optimal_paths:
    dist = travel_dist(path)
    if dist > optimal_dist:
        optimal_dist = dist
        optimal_path = path

print("Optimal Path: %s \n" % optimal_path)
print("    Distance: %s \n" % optimal_dist)
