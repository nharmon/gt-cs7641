# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Four Peaks Problem - MIMIC
from mimicry import Mimic
import math

iterations = 3
limits = [(0,1)]*20
t = 2

def heads(val,string):
    returnVal = 0
    for i in range(0,len(string)):
        if string[i] != val:
            break
        returnVal += 1
    return returnVal

def tails(val,string):
    returnVal = 0
    for i in range(0,len(string)):
        if string[len(string)-1-i] != val:
            break
        returnVal += 1
    return returnVal
    

def four_peaks(string):
    global t
    num_heads = heads(1,string)
    num_tails = tails(0,string)    
    returnVal = max(num_heads,num_tails)
    if (num_heads > t and num_tails > t):
        returnVal = returnVal + len(string)
    return returnVal
    
optimizer = Mimic(limits,four_peaks)

optimal_string_value = 0

for i in range(1,iterations+1):
    optimal_strings = optimizer.fit()
    print("  Iteration: %s" % i)
    print("Best String: %s" % optimal_strings[0])
    print("      Value: %s\n" % four_peaks(optimal_strings[0]))
    for string in optimal_strings:
        value = four_peaks(string)
        if value > optimal_string_value:
            optimal_string_value = value
            optimal_string = string

print(" Optimal String: %s" % optimal_string)
print("          Value: %s\n" % optimal_string_value)
