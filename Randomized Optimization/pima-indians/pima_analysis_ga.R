# Nathan Harmon
# CS7641, Spring 2015, Randomized Optimization Assignment
# Genetic Algorithm Weighting of Pima Indians Neural Network

# Uncomment if you need to install
#install.packages("GA")
#install.packages("neuralnet")
#install.packages("gmodels")
require("GA")
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

ptm <- proc.time()

ga_result <- ga(type="real-valued",
                fit_func,
                min=rep(-5,18),
                max=rep(5,18),
                popSize=1000,
                maxiter=iterations)

cat("            Time:",(proc.time() - ptm),"\n")
cat(" Optimal Weights:",ga_result@solution[1,],"\n")

model$weights[[1]][[1]][,1] <- as.vector(ga_result@solution[1,1:9])
model$weights[[1]][[1]][,2] <- as.vector(ga_result@solution[1,10:18])
prediction <- compute(model, testing_set[1:8])$net.result
classification <- apply(prediction, c(1), maxfactor)
prediction <- c('Diabetic', 'Normal')[classification]
result <- table(prediction,testing_set$Diagnosis)

cat("        Accuracy:",sum(prediction == testing_set$Diagnosis)/sum(result),"\n")

CrossTable(x = testing_set$Diagnosis, 
           y = prediction,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           dnn = c("Actual", "Prediction"))  
