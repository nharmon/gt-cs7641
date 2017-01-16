# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# Decision Tree Analysis of Pima Indians Dataset

#install.packages("C50")
#install.packages("gmodels")
require("C50")
require("gmodels")

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

#pimadata_rand <- pimadata[order(runif(768)), ]

# Create our training and testing sets
training_set <- pimadata[1:(nrow(pimadata)-100), ]
testing_set <- pimadata[(nrow(pimadata)-99):nrow(pimadata), ]

# Can check equitable distribution in training and test sets using:
table(testing_set[9])
table(training_set[9])

# Perform Decision Tree Analysis
ptm <- proc.time()
model <- C5.0(training_set[-9], 
              training_set$Diagnosis,
              trials=20)
prediction <- predict(model, 
                      testing_set)
cat("Time:",(proc.time() - ptm),"\n")

summary(model)

CrossTable(x = testing_set$Diagnosis, 
           y = prediction,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           dnn = c("Actual", "Prediction"))
