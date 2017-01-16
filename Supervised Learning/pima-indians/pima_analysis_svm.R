# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# Support Vector Machine Analysis of Pima Indians Dataset

#install.packages("kernlab")
#install.packages("gmodels")
require("kernlab")
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

# Create our training and testing sets
training_set <- pimadata[1:(nrow(pimadata)-100), ]
testing_set <- pimadata[(nrow(pimadata)-99):nrow(pimadata), ]

# Perform SVM Analyses for different kernels
# Supported Kernels: rbfdot     - Radial Basis kernel "Gaussian"
#                    polydot    - Polynomial kernel
#                    tanhdot    - Hyperbolic tangent kernel
#                    laplacedot - Laplacian kernel
#                    besseldot  - Bessel kernel
#                    anovadot   - ANOVA RBF kernel
#                    splinedot  - Spline kernel

kernels = c("rbfdot","polydot","vanilladot","tanhdot","laplacedot",
            "besseldot","anovadot","splinedot")

for (i in 1:length(kernels)) {
  cat("-----------------------------------------------------------------\n")
  cat("Using Kernel:",kernels[i],"\n")

  ptm <- proc.time()
  diabetes_classifier <- ksvm(Diagnosis ~ ., data = training_set,
                              type = "C-svc", kernel = kernels[i])  
  prediction <- predict(diabetes_classifier, testing_set)
  cat("Time:",(proc.time() - ptm),"\n")
  
  CrossTable(x = testing_set$Diagnosis, 
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Prediction"))  
}
