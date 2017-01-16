# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Knapsack Problem - MIMIC
from mimicry import Mimic

iterations = 5

item_weights = [93,76,87,95,55,19,18,91,66,26,
                91,72,18,50, 3,59,16,23,42,92,
                57,65,16,46,21,54,67,89,88,76,
                38, 2,99,41, 9,43,91,47,17,74,
                56,53,79,94,77,42,63, 9,13,50,
                29,80,91,33, 7, 9, 2,37,10, 1,
                39, 3,25,34,59,85,11,47, 9,24,
                21,18,19,47,91,18,60,80,38,90,
                 1,98, 4,41,44, 3,49,68,47,62,
                12,74,22,33, 8,23,95,39, 6,39]

item_values = [35,23,13,29,34,30, 2,31, 6,10,
               22,17,14,24,12, 3,21,11, 8,31,
               10,15,20,28,23, 7,12,15,32,14,
                5,10,12,13, 4,21,17,24,25,21,
               33,28,16,14,27, 7,13,13,23,17,
               33, 5,21,27, 8,21,30, 4,25,34,
               22,21,11,23, 2, 3,25, 4,21,16,
               22,10,12, 7, 1,34,17,20, 6,17,
               31,20,19,34, 1,15,34,31,14, 5,
               34, 6,20, 9,27,21,35, 7, 9, 2]

weight_limit = 1500

def sack_value(contents):
    global item_weights
    global item_values
    global weight_limit
    if len(contents) != 100:
        return 0
    total_weight = 0
    for a,b in zip(contents,item_weights):
        total_weight += a*b
    if total_weight > weight_limit:
        return 0
    total_value = 0
    for a,b in zip(contents,item_values):
        total_value += a*b
    return total_value

limits = [(0,1)]*100

optimizer = Mimic(limits,sack_value)

for i in range(1,iterations):
    optimal_packings = optimizer.fit()
    #print("Found Optimal Value: %s" % optima)

optimal_packing_value = 0

for packing in optimal_packings:
    value = sack_value(packing)
    if value > optimal_packing_value:
        optimal_packing_value = value
        optimal_packing = packing

print("Optimal Packing: %s \n" % optimal_packing)
print("          Value: %s \n" % optimal_packing_value)
