# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# Support Vector Machine Analysis of Diabetes Dataset

#install.packages("kernlab")
#install.packages("dummies")
#install.packages("gmodels")
require("kernlab")
require("dummies")
require("gmodels")

# Function to normalize vectors
normalize <- function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

#Data From: https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip
dbdata <- read.csv("diabetic_data.csv")

#Prepare Data
dbdata <- dbdata[-12]  # Remove Medical Specialty Code (Non-deterministic)
dbdata <- dbdata[-11]  # Remove Payer Code (Non-deterministic)

dbdata <- dbdata[-6]   # Remove Weight (Not enough instances with known values)
dbdata <- dbdata[-2]   # Remove Patient Number (Non-deterministic)
dbdata <- dbdata[-1]   # Remove Encounter Number (Non-deterministic)

dbdata <- dbdata[dbdata$race != "?" &
                   dbdata$gender != "Unknown/Invalid" &
                   dbdata$diag_1 != "?" &
                   dbdata$diag_2 != "?" &
                   dbdata$diag_3 != "?" , ]

dbdata$diag_1 <- as.numeric(dbdata$diag_1)
dbdata$diag_2 <- as.numeric(dbdata$diag_2)
dbdata$diag_3 <- as.numeric(dbdata$diag_3)

dbdata <- dummy.data.frame(dbdata, 
                           c("race", "gender", "age", "weight",
                             "admission_type_id", "discharge_disposition_id", 
                             "admission_source_id", "max_glu_serum", "A1Cresult", 
                             "metformin", "repaglinide", "nateglinide", 
                             "chlorpropamide", "glimepiride", "acetohexamide", 
                             "glipizide", "glyburide", "tolbutamide", 
                             "pioglitazone",  "rosiglitazone","acarbose", 
                             "miglitol", "troglitazone", "tolazamide", 
                             "examide", "citoglipton", "insulin", 
                             "glyburide.metformin", "glipizide.metformin", 
                             "glimepiride.pioglitazone", "metformin.rosiglitazone", 
                             "metformin.pioglitazone", "change", "diabetesMed"),
                           omit.constants=TRUE)

colnames(dbdata) <- gsub("\\[",'',colnames(dbdata))
colnames(dbdata) <- gsub("\\)",'',colnames(dbdata))
colnames(dbdata) <- gsub(">",'gt',colnames(dbdata))
colnames(dbdata) <- gsub("/",'',colnames(dbdata))
colnames(dbdata) <- gsub("\\?",'Unknown',colnames(dbdata))
colnames(dbdata) <- gsub("\\-",'to',colnames(dbdata))

levels(dbdata$readmitted) <- c("YES", "YES", "NO")

# Set up training sizes and a common testing_set:
training_set_sizes <- c(5870,11740,17610,23480,29350,35220,41090,
                        46960,52830,58700,64570,70440,76310,82180)

testing_set <- dbdata[(nrow(dbdata)-9999):nrow(dbdata), ]

# Check distribution of testing_set
table(testing_set$readmitted)

### SVM Analysis

# Partial Training Sets
for (i in 1:length(training_set_sizes)) {
  training_set <- dbdata[1:training_set_sizes[i], ]
  cat("-----------------------------------------------------------------\n")
  cat("Using Kernel rbfdot. Size of training set: ",nrow(training_set),"\n")
  
  ptm <- proc.time()
  classifier <- ksvm(readmitted ~ ., 
                     data = training_set,
                     type = "C-svc", 
                     kernel = "rbfdot",
                     na.action = na.omit,
                     scaled = FALSE)
  cat("Training Time:",(proc.time() - ptm),"\n")
  
  ptm <- proc.time()
  prediction <- predict(classifier,
                        testing_set[1:(ncol(testing_set)-1)])
  cat("Testing Time:",(proc.time() - ptm),"\n")

  tab <- table(testing_set$readmitted,prediction)
  error <- (tab[2]+tab[3])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
}  

# Full Training Set
training_set <- dbdata[1:(nrow(dbdata)-10000), ]

#training_set <- dbdata[1:180, ]

# Perform SVM Analyses for different kernels
# Kernels: rbfdot     - Radial Basis kernel "Gaussian"
#          besseldot  - Bessel kernel

kernels = c("rbfdot","besseldot")

for (i in 1:length(kernels)) {
  ptm <- proc.time()
  cat("-----------------------------------------------------------------\n")
  cat("Using Kernel:",kernels[i]," Size of training set: ",nrow(training_set),"\n")
  
  classifier <- ksvm(readmitted ~ ., 
                     data = training_set,
                     type = "C-svc", 
                     kernel = kernels[i],
                     na.action = na.omit,
                     scaled = FALSE)
  
  prediction <- predict(classifier, testing_set)

  CrossTable(x = testing_set$readmitted,
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Predicted"))
  
  cat("Time:",(proc.time() - ptm),"\n")
}
