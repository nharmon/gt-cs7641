# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# Neural Network Analysis of Diabetes Dataset

#install.packages("neuralnet")
#install.packages("dummies")
#install.packages("gmodels")
require("neuralnet")
require("dummies")
require("gmodels")

# Function to normalize vectors
normalize <- function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

maxfactor <- function(x) {
  return(which(x == max(x)))
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

dbdata$time_in_hospital <- normalize(dbdata$time_in_hospital)
dbdata$num_lab_procedures <- normalize(dbdata$num_lab_procedures)
dbdata$num_procedures <- normalize(dbdata$num_procedures)
dbdata$num_medications <- normalize(dbdata$num_medications)
dbdata$number_outpatient <- normalize(dbdata$number_outpatient)
dbdata$number_emergency <- normalize(dbdata$number_emergency)
dbdata$number_inpatient <- normalize(dbdata$number_inpatient)
dbdata$diag_1 <- normalize(as.numeric(dbdata$diag_1))
dbdata$diag_2 <- normalize(as.numeric(dbdata$diag_2))
dbdata$diag_3 <- normalize(as.numeric(dbdata$diag_3))
dbdata$number_diagnoses <- normalize(dbdata$number_diagnoses)

dbdata <- dummy.data.frame(dbdata, c("race", "gender", "age", 
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
                                     "metformin.pioglitazone", "change", "diabetesMed"))

colnames(dbdata) <- gsub("\\[",'',colnames(dbdata))
colnames(dbdata) <- gsub("\\)",'',colnames(dbdata))
colnames(dbdata) <- gsub(">",'gt',colnames(dbdata))
colnames(dbdata) <- gsub("/",'',colnames(dbdata))
colnames(dbdata) <- gsub("\\?",'Unknown',colnames(dbdata))
colnames(dbdata) <- gsub("\\-",'to',colnames(dbdata))

levels(dbdata$readmitted) <- c("Yes", "Yes", "No")

# Set up training sizes and a common testing_set:
training_set_sizes <- c(5870,11740,17610,23480,29350,35220,41090,
                        46960,52830,58700,64570,70440,76310,82180)

testing_set <- dbdata[(nrow(dbdata)-9999):nrow(dbdata), ]

# Data Prep for training sets only
dbdata <- cbind(dbdata, dbdata$readmitted == "Yes")
dbdata <- cbind(dbdata, dbdata$readmitted == "No")
dbdata <- dbdata[-159]
names(dbdata)[159] <- "readmittedYes"
names(dbdata)[160] <- "readmittedNo"

# Check distribution of testing_set
table(testing_set$readmitted)

# Partial Training Sets
for (i in 1:length(training_set_sizes)) {
  training_set <- dbdata[1:training_set_sizes[i], ]
  
  # Formula for Neural Network
  n <- names(training_set[1:158])
  f <- as.formula(paste("readmittedYes+readmittedNo ~", 
                        paste(n[!n %in% "readmitted"], collapse = " + ")))
  
  cat("-----------------------------------------------------------------\n")
  cat("For 0 hidden nodes. Size of training set: ",nrow(training_set),"\n")
  
  ptm <- proc.time()
  model <- neuralnet(f, 
                     data = training_set, 
                     hidden = 0)
  cat("Training Time:",(proc.time() - ptm),"\n")
  
  ptm <- proc.time()
  prediction <- compute(model, testing_set[1:(ncol(testing_set)-1)])$net.result
  classification <- apply(prediction, c(1), maxfactor)
  prediction <- c('Yes', 'No')[classification]
  table(prediction, testing_set$readmitted)
  cat("Testing Time:",(proc.time() - ptm),"\n")
  
  tab <- table(testing_set$readmitted,prediction)
  error <- (tab[1]+tab[4])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
}

# Create our full training set and its labels
training_set <- dbdata[1:(nrow(dbdata)-10000), ]

# Formula for Neural Network
n <- names(training_set[1:158])
f <- as.formula(paste("readmittedYes+readmittedNo ~", 
                      paste(n[!n %in% "readmitted"], collapse = " + ")))

# Test Neural Networks with multiple hidden nodes
for (i in 0:10) {
  cat("-----------------------------------------------------------------\n")
  cat("With",i,"hidden nodes. Size of training set: ",nrow(training_set),"\n")
  
  ptm <- proc.time()
  model <- neuralnet(f, 
                     data = training_set, 
                     hidden = i)
  cat("Training Time:",(proc.time() - ptm),"\n")
  
  ptm <- proc.time()
  prediction <- compute(model, testing_set[1:(ncol(testing_set)-1)])$net.result
  classification <- apply(prediction, c(1), maxfactor)
  prediction <- c('Yes', 'No')[classification]
  table(prediction, testing_set$readmitted)
  cat("Testing Time:",(proc.time() - ptm),"\n")
  
  tab <- table(testing_set$readmitted,prediction)
  error <- (tab[1]+tab[4])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
  
  CrossTable(x = testing_set$readmitted, 
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Prediction"))
  
  cat("Time:",(proc.time() - ptm),"\n")
}
