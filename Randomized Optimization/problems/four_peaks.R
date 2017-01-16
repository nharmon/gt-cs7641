# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Four Peaks Problem - RHC, SA, GA

#install.packages("GA")
require("GA")

set.seed = 1234
string_length = 20
t <- 2
iterations = 3

min = rep(0,string_length)
max = rep(1,string_length)

heads <- function(val=int,string=c()) {
  returnVal <- 0
  for (i in 1:length(string)) {
    if (string[i] != val) {
      break
    }
    returnVal = returnVal + 1
  }
  return(returnVal)
}

tails <- function(val=int,string=c()) {
  returnVal <- 0
  for (i in 1:length(string)) {
    if (string[length(string)+1-i] != val) {
      break
    }
    returnVal = returnVal + 1
  }
  return(returnVal)
}

four_peaks <- function(string=c()) {
  num_heads <- heads(1,string)
  num_tails <- tails(0,string)
  
  returnVal = max(num_heads,num_tails)
  
  if (num_heads > t && num_tails > t) {
    returnVal = returnVal + length(string)
  }
  
  return(returnVal)
}

grad_func <- function(string=c()) {
  rand_position <- sample(1:length(string),1)
  string[rand_position] <- xor(string[rand_position],1)
  return(string)
}

cost_func <- function(string=c()) {
  return(500 - four_peaks(string))
}

###
### Random Hill Climbing ###
###

rhc_analysis <- function() {
  cat("Random Hill Climbing\n")
  
  ptm <- proc.time()
  
  rhc_optimal_value   <- 0
  
  for (i in 1:iterations) {
    string <- sample(0:1, size=string_length, replace=T)
    value <- four_peaks(string)
    sequence <- sample(1:string_length,string_length,replace=F)
    for (j in 1:string_length) {
      test_string <- string
      test_string[sequence[j]] <- xor(test_string[sequence[j]],1)
      test_value <- four_peaks(test_string)
      if (test_value > value) {
        string <- test_string
        value <- test_value
        j <- 1
      }
    }
    if (value > rhc_optimal_value) {
      rhc_optimal_string <- string
      rhc_optimal_value <- value
    }
    cat("Time:",(proc.time() - ptm)," Value:",value," Optimal:",rhc_optimal_value,"\n")
  }
  
  cat("            Time:",(proc.time() - ptm),"\n")
  cat("  Optimal String:",rhc_optimal_string,"\n")
  cat("           Value:",rhc_optimal_value,"\n")
  
  cat("\n----------------------------------------------------\n")
  
}

###
### Simulated Annealing
###

sa_function <- function (temperature) {
  sa_result <- optim(sample(0:1,string_length,replace=T),
                     four_peaks,
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
    sa_value = four_peaks(sa_result$par)
    if (sa_value > max_sa_value) {
      max_sa_value <- sa_value
      max_sa_string <- sa_result$par
      max_sa_temp <- temp
    }
    cat("Temp: ",temp," Value:",sa_value," Max: ",max_sa_value,"\n")
  }
  
  cat("            Time:",(proc.time() - ptm),"\n")
  cat("    Optimal Temp:",max_sa_temp,"\n")
  cat("  Optimal String:",max_sa_string,"\n")
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
                  four_peaks,
                  nBits = string_length,
                  popSize = 1000,
                  #monitor=NULL,
                  maxiter = iterations)
  
  cat("            Time:",(proc.time() - ptm),"\n")
  cat("  Optimal String:",ga_result@solution[1,],"\n")
  cat("           Value:",four_peaks(ga_result@solution[1,]),"\n")
  png("four_peaks_ga.png",width=5,height=3.25,units="in",res=1200)
  plot(ga_result,main="Figure 3.3-2")
  
  cat("\n----------------------------------------------------\n")
}

### Run ###
rhc_analysis()
sa_analysis()
ga_analysis()