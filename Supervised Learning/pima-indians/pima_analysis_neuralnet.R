# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# Neural Network Analysis of Pima Indians Dataset

#install.packages("neuralnet")
#install.packages("gmodels")
require("neuralnet")
require("gmodels")

# Function to normalize vectors
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

# Prepare our data (normalize or scale). Note this new set is not classified.
#pimadata_use <- as.data.frame(pimadata[1:8])
#pimadata_use <- as.data.frame(lapply(pimadata[1:8], normalize))
pimadata_use <- as.data.frame(lapply(pimadata[1:8], scale))

# Classify the data
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 1)
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 0)
names(pimadata_use)[9] <- "Diabetic"
names(pimadata_use)[10] <- "Normal"

# Create our training and testing sets
training_set <- pimadata_use[1:(nrow(pimadata)-100), ]

testing_set <- pimadata_use[(nrow(pimadata)-99):nrow(pimadata),1:8]
testing_set$Diagnosis <- as.factor(
  as.integer(xor(1,pimadata$Diagnosis[(nrow(pimadata)-99):nrow(pimadata)])))
levels(testing_set$Diagnosis) <- c("Diabetic", "Normal")

# Check distribution of testing set:
#table(testing_set$Diabetic)
#table(testing_set$Normal)

# Test Neural Networks with multiple hidden nodes
for (i in 0:10) {
  cat("-----------------------------------------------------------------\n")
  cat("With",i,"hidden nodes:\n")  
  
  ptm <- proc.time()
  model <- neuralnet(Diabetic+Normal ~
                       Pregnancies + 
                       GlucoseConcentration + 
                       DiastolicBP +
                       TricepSkinFoldThickness + 
                       TwoHrSerumInsulin + 
                       BMI +
                       DiabetesPedigreeFunction + 
                       Age,
                     data = training_set,
                     hidden = i)
  
  #plot(model)
  
  prediction <- compute(model, testing_set[1:8])$net.result
  cat("Time:",(proc.time() - ptm),"\n")

  classification <- apply(prediction, c(1), maxfactor)
  prediction <- c('Diabetic', 'Normal')[classification]
  table(prediction, testing_set$Diagnosis)
  
  CrossTable(x = testing_set$Diagnosis, 
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Prediction"))  
}
