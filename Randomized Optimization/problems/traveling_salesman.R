# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Traveling Salesman Problem

#install.packages("GA")
require("GA")

iterations <- 15

cities <- data.frame(name  <- c("Gotham","Metropolis","Smallville","Hill Valley",
                                "Mayberry","South Park","Los Santos","Vice City",
                                "Racoon City","Charming","Granville","Coast City"),
                     xcoord <- c(2,17,7,3,8,10,6,8,3,5,1,8),
                     ycoord <- c(7,2,5,13,1,9,2,8,4,11,19,3) )

city_dist <- function(start,end) {
  sqrt(abs(cities$xcoord[start] - cities$xcoord[end])^2 +
       abs(cities$ycoord[start] - cities$ycoord[end])^2  )  
}

travel_dist <- function(string=c()) {
  returnVal = 0;
  if (length(string) > 1) {
    for (i in 2:length(string)) {
      returnVal = returnVal + city_dist(string[i],string[i-1])
    }
    returnVal = returnVal + city_dist(string[1],string[length(string)])
  } else {
    stop("Need at least two cities")
  }
  returnVal
}

grad_func <- function(string=c()) {
  swap_points <- sample(string, size=2, replace=FALSE)
  tmp <- string[swap_points[1]]
  string[swap_points[1]] <- string[swap_points[2]]
  string[swap_points[2]] <- tmp
  return(string)
}

initial_sequence <- c(1:12)

###
### Random Hill Climbing ###
###

cat("Random Hill Climbing\n")

ptm <- proc.time()

rhc_optimal_path <- initial_sequence
rhc_optimal_dist <- travel_dist(initial_sequence)

for (i in 1:iterations) {
  path <- sample(initial_sequence, size=12, replace=FALSE)
  dist <- travel_dist(path)
  #cat("Path: ",path,"(",dist,")\n")
  for (j in 1:66) {
    test_path <- grad_func(path)
    test_dist <- travel_dist(test_path)
    if (test_dist > dist) {
      path <- test_path
      dist <- test_dist
      j <- 1
      #cat("      ",path,"(",dist,")\n")
    }
  }
  if (dist > rhc_optimal_dist) {
    rhc_optimal_path <- path
    rhc_optimal_dist <- dist
  }
  cat("Time:",(proc.time() - ptm)," Value:",dist," Optimal:",rhc_optimal_dist,"\n")
}

cat("         Time:",(proc.time() - ptm),"\n")
cat(" Optimal Path:",rhc_optimal_path,"\n")
cat("     Distance:",rhc_optimal_dist,"\n")

cat("\n----------------------------------------------------\n")

###
### Simulated Annealing
###

sa_function <- function (temperature) {
  sa_result <- optim(initial_sequence,
                     travel_dist,
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
    sa_value = travel_dist(sa_result$par)
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

sa_analysis()

###
### Genetic Algorithm
###

cat("Genetic Algorithm\n")

ptm <- proc.time()

min <- c(1,1,1,1,1,1,1,1,1,1,1,1)
max <- min*12

ga_result <- ga(type="permutation",
                travel_dist,
                min=min,
                max=max,
                #monitor=NULL,
                popSize=100,
                maxiter = iterations)

cat("         Time:",(proc.time() - ptm),"\n")
cat(" Optimal Path:",ga_result@solution[1,],"\n")
cat("     Distance:",travel_dist(ga_result@solution[1,]),"\n")
png("traveling_salesman_ga.png",width=5,height=3.25,units="in",res=1200)
plot(ga_result,main="Figure 3.1-2")

cat("\n----------------------------------------------------\n")
