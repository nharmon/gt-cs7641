# Nathan Harmon
# CS7641, Spring 2015, Supervised Learning Assignment
# k-Nearest Neighbors Analysis of Diabetes Dataset

#install.packages("class")
#install.packages("dummies")
#install.packages("gmodels")
require("class")
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

levels(dbdata$readmitted) <- c("YES", "YES", "NO")

# Set up training sizes and a common testing_set:
training_set_sizes <- c(5870,11740,17610,23480,29350,35220,41090,
                        46960,52830,58700,64570,70440,76310,82180)

testing_set <- dbdata[(nrow(dbdata)-9999):nrow(dbdata),1:(ncol(dbdata)-1)]
testing_set_labels <- dbdata$readmitted[(nrow(dbdata)-9999):nrow(dbdata)]

# Check distribution of testing_set
table(testing_set_labels)

# Partial Training Sets
for (i in 1:length(training_set_sizes)) {
  training_set <- dbdata[1:training_set_sizes[i],1:(ncol(dbdata)-1)]
  training_set_labels <- dbdata$readmitted[1:training_set_sizes[i]]
  
  cat("-----------------------------------------------------------------\n")
  cat("For k = 3. Size of training set: ",nrow(training_set),"\n")
  
  ptm <- proc.time()
  prediction <- knn(train = training_set, 
                    test = testing_set,
                    cl = training_set_labels, 
                    k = 3,
                    use.all = TRUE)
  cat("Time:",(proc.time() - ptm),"\n")
  
  tab <- table(testing_set_labels,prediction)
  error <- (tab[2]+tab[3])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
}

# Create our full training set and its labels
training_set <- dbdata[1:(nrow(dbdata)-10000),1:(ncol(dbdata)-1)]
training_set_labels <- dbdata$readmitted[1:(nrow(dbdata)-10000)]

#training_set <- dbdata[1:90, ]
#training_set_labels <- dbdata$readmitted[1:90]

# Perform our k-Nearest Neighbor Analysis for multiple values of k
for (i in 1:10) {
  cat("-----------------------------------------------------------------\n")
  cat("For k =",i,":\n")
  
  ptm <- proc.time()
  prediction <- knn(train = training_set, 
                    test = testing_set,
                    cl = training_set_labels, 
                    k = i,
                    use.all = TRUE)
  cat("Time:",(proc.time() - ptm),"\n")
  
  CrossTable(x = testing_set_labels,
             y = prediction,
             prop.r = FALSE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             dnn = c("Actual", "Prediction"))
  
  tab <- table(testing_set_labels,prediction)
  error <- (tab[2]+tab[3])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
}

# Perform an analysis of performance as a function of testing size

training_set <- dbdata[(nrow(dbdata)-9999):nrow(dbdata),1:(ncol(dbdata)-1)]
training_set_labels <- dbdata$readmitted[(nrow(dbdata)-9999):nrow(dbdata)]

for (i in 1:length(training_set_sizes)) {
  training_set <- dbdata[1:training_set_sizes[i],1:(ncol(dbdata)-1)]
  training_set_labels <- dbdata$readmitted[1:training_set_sizes[i]]
    
  cat("-----------------------------------------------------------------\n")
  cat("For k = 3. Size of testing set: ",nrow(training_set),"\n")
  
  ptm <- proc.time()
  prediction <- knn(train = training_set, 
                    test = testing_set,
                    cl = training_set_labels, 
                    k = 3,
                    use.all = TRUE)
  cat("Time:",(proc.time() - ptm),"\n")
  
  tab <- table(testing_set_labels,prediction)
  error <- (tab[2]+tab[3])/nrow(testing_set)
  cat("Testing Error:",error,"\n")
}
