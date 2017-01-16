# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Knapsack Problem - RHC, SA, GA

#install.packages("GA")
require("GA")

set.seed = 1234

sack_weight_limit <- 1500

items <- data.frame(item_weight <- c(93,76,87,95,55,19,18,91,66,26,
                                     91,72,18,50, 3,59,16,23,42,92,
                                     57,65,16,46,21,54,67,89,88,76,
                                     38, 2,99,41, 9,43,91,47,17,74,
                                     56,53,79,94,77,42,63, 9,13,50,
                                     29,80,91,33, 7, 9, 2,37,10, 1,
                                     39, 3,25,34,59,85,11,47, 9,24,
                                     21,18,19,47,91,18,60,80,38,90,
                                      1,98, 4,41,44, 3,49,68,47,62,
                                     12,74,22,33, 8,23,95,39, 6,39),
                    item_value <-  c(35,23,13,29,34,30, 2,31, 6,10,
                                     22,17,14,24,12, 3,21,11, 8,31,
                                     10,15,20,28,23, 7,12,15,32,14,
                                      5,10,12,13, 4,21,17,24,25,21,
                                     33,28,16,14,27, 7,13,13,23,17,
                                     33, 5,21,27, 8,21,30, 4,25,34,
                                     22,21,11,23, 2, 3,25, 4,21,16,
                                     22,10,12, 7, 1,34,17,20, 6,17,
                                     31,20,19,34, 1,15,34,31,14, 5,
                                     34, 6,20, 9,27,21,35, 7, 9, 2))

sack_weight <- function(string=c()) {
  return(sum(string * item_weight))
}

sack_value <- function(string=c()) {
  if (length(string) != 100) {
    cat("Expect length 100\n")
    return(0)
  }
  if (sack_weight(string) > sack_weight_limit) {
    return(0)
  }
  return(sum(string * item_value))
}

cost_func <- function(string=c()) {
  return(80310 - sack_value(string))
}

grad_func <- function(string=c()) {
  rand_position <- sample(1:length(string),1)
  string[rand_position] <- xor(string[rand_position],1)
  return(string)
}

get_pop <- function() {
  return(c(1,rep(0,99)))
}

min = rep(0,100)
max = rep(1,100)

iterations <- 100

###
### Random Hill Climbing ###
###

rhc_analysis <- function() {
  cat("Random Hill Climbing\n")

  ptm <- proc.time()

  rhc_optimal_packing <- rep(0,100)
  rhc_optimal_value   <- 0

  for (i in 1:iterations) {
    repeat {
      packing <- sample(0:1, size=100, replace=T)
      #packing <- c(sample(0:1, size=10, replace=T),rep(0,90))
      value   <- sack_value(packing)
      if (value > 0) {
        break
      }
    }
    sequence <- sample(1:100,100,replace=F)
    for (j in 1:100) {
      test_packing <- packing
      test_packing[sequence[j]] <- xor(test_packing[sequence[j]],1)
      test_value <- sack_value(test_packing)
      if (test_value > value) {
        packing <- test_packing
        value <- test_value
        sequence <- sample(1:100,100,replace=F)
        j <- 1
      }
    }
    if (value > rhc_optimal_value) {
      rhc_optimal_packing <- packing
      rhc_optimal_value <- value
    }
    cat("Time:",(proc.time() - ptm)," Value:",value," Optimal:",rhc_optimal_value,"\n")
  }

  cat("            Time:",(proc.time() - ptm),"\n")
  cat(" Optimal Packing:",rhc_optimal_packing,"\n")
  cat("           Value:",rhc_optimal_value,"\n")
  
  cat("\n----------------------------------------------------\n")

}


###
### Simulated Annealing
###

sa_function <- function (temperature) {
  sa_result <- optim(min,
                     sack_value,
                     gr = grad_func,
                     method = "SANN",
                     control = list(fnscale = -1,
                                    maxit = iterations,
                                    temp = temperature))
  return(sa_result)
}

sa_analysis <- function () {
  cat("Simulated Annealing\n")
  max_sa_value <- 0
  ptm <- proc.time()
  
  for (temp in 1:250) {
    sa_result <- sa_function(temp)
    sa_value = sack_value(sa_result$par)
    if (sa_value > max_sa_value) {
      max_sa_value <- sa_value
      max_sa_packing <- sa_result$par
      max_sa_temp <- temp
    }
    cat("Temp: ",temp," Value:",sa_value," Max: ",max_sa_value,"\n")
  }

  cat("            Time:",(proc.time() - ptm),"\n")
  cat("    Optimal Temp:",max_sa_temp,"\n")
  cat(" Optimal Packing:",max_sa_packing,"\n")
  cat("   Optimal Value:",max_sa_value,"\n")

  cat("\n----------------------------------------------------\n")
}
  
###
### Genetic Algorithm
###

ga_analysis <- function() {
  cat("Genetic Algorithm\n")
  
  ptm <- proc.time()
  
  ga_result <- ga(type="binary",
                  sack_value,
                  nBits = 100,
                  popSize = 1000,
                  #monitor=NULL,
                  maxiter = iterations)
  
  cat("            Time:",(proc.time() - ptm),"\n")
  cat(" Optimal Packing:",ga_result@solution[1,],"\n")
  cat("           Value:",sack_value(ga_result@solution[1,]),"\n")
  png("knapsack_ga.png",width=5,height=3.25,units="in",res=1200)
  plot(ga_result,main="Figure 3.2-2")

  cat("\n----------------------------------------------------\n")
}

### Run ###
rhc_analysis()
sa_analysis()
ga_analysis()