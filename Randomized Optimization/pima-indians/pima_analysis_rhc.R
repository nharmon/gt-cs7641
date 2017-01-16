# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Simulated Annealing Weighting of Pima Indians Neural Network

# Uncomment if you need to install
#install.packages("neuralnet")
#install.packages("gmodels")

require("neuralnet")
require("gmodels")

# Settings
iterations <- 100

# Functions
normalize <- function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

maxfactor <- function(x) {
  return(which(x == max(x)))
}

# Data is from:
# https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
pimadata <- read.csv("pima-indians-diabetes.data", 
                     header = FALSE)

# Column names are from:
# https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.names
colnames(pimadata) <- c("Pregnancies",
                        "GlucoseConcentration",
                        "DiastolicBP",
                        "TricepSkinFoldThickness",
                        "TwoHrSerumInsulin",
                        "BMI",
                        "DiabetesPedigreeFunction",
                        "Age",
                        "Diagnosis")

# Remove Observations with Missing Data
pimadata <- pimadata[pimadata[2] != 0 &
                       pimadata[3] != 0 &
                       pimadata[4] != 0 &
                       pimadata[5] != 0 &
                       pimadata[6] != 0 &
                       pimadata[7] != 0 &
                       pimadata[8] != 0, ]

# Normalize our data. Note this new set is not classified.
pimadata_use <- as.data.frame(lapply(pimadata[1:8], scale))

# Classify the data
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 1)
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 0)
names(pimadata_use)[9] <- "Diabetic"
names(pimadata_use)[10] <- "Normal"

# Create our model, training and testing sets
initial_model_set <- pimadata_use[1:(nrow(pimadata)-100), ]

training_set <- pimadata_use[1:(nrow(pimadata)-100),1:8]
training_set$Diagnosis <- as.factor(
  as.integer(xor(1,pimadata$Diagnosis[1:(nrow(pimadata)-100)])))
levels(training_set$Diagnosis) <- c("Diabetic", "Normal")

testing_set <- pimadata_use[(nrow(pimadata)-99):nrow(pimadata),1:8]
testing_set$Diagnosis <- as.factor(
  as.integer(xor(1,pimadata$Diagnosis[(nrow(pimadata)-99):nrow(pimadata)])))
levels(testing_set$Diagnosis) <- c("Diabetic", "Normal")

# Create our neural network model
model <- neuralnet(Diabetic+Normal ~
                     Pregnancies + 
                     GlucoseConcentration + 
                     DiastolicBP +
                     TricepSkinFoldThickness + 
                     TwoHrSerumInsulin + 
                     BMI +
                     DiabetesPedigreeFunction + 
                     Age,
                   data = initial_model_set,
                   hidden = 0)

# Neural Network as Fitness Function

fit_func <- function(string=c()) {
  model$weights[[1]][[1]][,1] <- string[1:9]
  model$weights[[1]][[1]][,2] <- string[10:18]
  prediction <- compute(model, training_set[1:8])$net.result
  classification <- apply(prediction, c(1), maxfactor)
  prediction <- c('Diabetic', 'Normal')[classification]
  result <- table(prediction,training_set$Diagnosis)
  return(sum(prediction == training_set$Diagnosis)/sum(result))
}

ideal_accuracy <- 0
ideal_weights <- list()

ptm <- proc.time()

for (i in 1:iterations) {
  weights <- sample(-1000:1000,18,replace=T)/1000
  accuracy <- fit_func(weights)
  #cat("---------------------------------------------------\n")
  #cat("New Random Start:",weights)
  #cat(" (",accuracy,")\n", sep="")
  weight_order <- sample(1:18,18,replace=F)
  for (j in 1:18) {
    for (k in 1:100) {
      new_weights <- weights
      new_weights[weight_order[j]] <- weights[weight_order[j]] + .00235*k
      new_accuracy <- fit_func(new_weights)
      
      if ( new_accuracy > accuracy) {
        accuracy <- new_accuracy
        weights <- new_weights
        #at("                :",weights)
        #cat(" (",accuracy,")\n", sep="")
        j <- 1
        next        
      }

      new_weights <- weights
      new_weights[weight_order[j]] <- weights[weight_order[j]] - .00235*k
      new_accuracy <- fit_func(new_weights)
                                                           
      if ( new_accuracy > accuracy) {
        accuracy <- new_accuracy
        weights <- new_weights
        #cat("                :",weights)
        #cat(" (",accuracy,")\n", sep="")
        j <- 1
        next        
      }
    }
  }
  if ( accuracy > ideal_accuracy) {
    ideal_accuracy <- accuracy
    ideal_weights <- weights
    #cat("      New Optima:",weights)
    #cat(" (",accuracy,")\n", sep="")    
  }
  cat("     Time:",(proc.time() - ptm))
  cat(" Accuracy:",accuracy)
  cat(" Ideal:",ideal_accuracy,"\n")
}

#cat("              Time:",(proc.time() - ptm),"\n")
cat("\n")
cat("   Optimal Weights:",ideal_weights[1:18],"\n")
cat(" Training Accuracy:",ideal_accuracy,"\n")

model$weights[[1]][[1]][,1] <- ideal_weights[1:9]
model$weights[[1]][[1]][,2] <- ideal_weights[10:18]

prediction <- compute(model, testing_set[1:8])$net.result
classification <- apply(prediction, c(1), maxfactor)
prediction <- c('Diabetic', 'Normal')[classification]
result <- table(prediction,testing_set$Diagnosis)

cat("  Testing Accuracy:",sum(prediction == testing_set$Diagnosis)/sum(result),"\n")

CrossTable(x = testing_set$Diagnosis, 
           y = prediction,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           dnn = c("Actual", "Prediction"))
