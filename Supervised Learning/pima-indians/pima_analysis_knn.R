# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# k-Nearest Neighbors Analysis of Pima Indians Dataset

#install.packages("class")
#install.packages("gmodels")
require("class")
require("gmodels")

# Function to normalize vectors
normalize <- function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
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

# Diagnosis classes are from the same document as column names (see above)
pimadata$Diagnosis <- factor(pimadata$Diagnosis,
                             levels = c(1,0),
                             labels = c("Diabetic", "Normal"))

# Remove Observations with Missing Data
pimadata <- pimadata[pimadata[2] != 0 &
                     pimadata[3] != 0 &
                     pimadata[4] != 0 &
                     pimadata[5] != 0 &
                     pimadata[6] != 0 &
                     pimadata[7] != 0 &
                     pimadata[8] != 0, ]

# Prepare our data (normalize or scale). Note this new set is not classified.
#pimadata_use <- as.data.frame(lapply(pimadata[1:8], normalize))
pimadata_use <- as.data.frame(scale(pimadata[1:8]))

# Create our training and testing sets, and their labels
training_set <- pimadata_use[1:(nrow(pimadata)-100), ]
training_set_labels <- pimadata[1:(nrow(pimadata)-100),9]

testing_set <- pimadata_use[(nrow(pimadata)-99):nrow(pimadata), ]
testing_set_labels <- pimadata[(nrow(pimadata)-99):nrow(pimadata),9]

# Can check equitable distribution in training and test sets using:
table(testing_set_labels)
table(training_set_labels)

# Perform our k-Nearest Neighbor Analysis for multiple values of k
for (i in 1:10) {
  ptm <- proc.time()
  
  cat("-----------------------------------------------------------------\n")
  cat("For k =",i,":")
  
  prediction <- knn(train = training_set, 
                    test = testing_set,
                    cl = training_set_labels, 
                    k = i,
                    use.all = TRUE)

  CrossTable(x = testing_set_labels, 
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Prediction"))
  
  cat("Time:",(proc.time() - ptm),"\n")
}
