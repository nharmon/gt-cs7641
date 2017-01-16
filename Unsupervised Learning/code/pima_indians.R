# Nathan Harmon
# CS7641, Spring 2015, Unsupervised Learning and Dimensionality Reduction
# Pima Indians Dataset

# Uncomment if you need to install
#install.packages("caret")
#install.packages("cluster")
#install.packages("mclust")
#install.packages("fastICA")
#install.packages("neuralnet")
#install.packages("nFactors")
#install.packages("gmodels")
require("caret")
require("cluster")
require("mclust")
require("fastICA")
require("neuralnet")
require("nFactors")
require("gmodels")

# Functions
maxfactor <- function(x) {
  return(which(x == max(x)))
}

vecnorm <- function (x) {
  p <- 2
  if (!is.numeric(x) && !is.complex(x))
    stop("mode of x must be either numeric or complex")
  if (is.numeric(x))
    x <- abs(x)
  else x <- Mod(x)
  return(.Fortran("d2norm", as.integer(length(x)), as.double(x),
                  as.integer(1), double(1), PACKAGE = "mclust")[[4]])
}

rca <- function(data, p = 2) {
  n <- ncol(data)
  u <- rnorm(n)
  u <- u/vecnorm(u)
  v <- rnorm(n)
  v <- v/vecnorm(v)
  Q <- cbind(u, v - sum(u * v) * u)
  dimnames(Q) <- NULL
  Data <- as.matrix(data) %*% Q
  Data
}

# Data is from:
# https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
pimadata <- read.csv("pima-indians-diabetes.data", 
                     header = FALSE)

# Column names are from:
# https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.names
colnames(pimadata) <- c("Pregnancies",
                        "Glucose",
                        "DiastolicBP",
                        "TSFT",
                        "Serum2Hr",
                        "BMI",
                        "DPF",
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

# Scale our data. Note this new set is not classified.
pimadata_use <- as.data.frame(lapply(pimadata[1:8], scale))

# Classify the data
pimadata_use[9] <- pimadata[9]+1

table(pimadata_use[9])

# Graph points and label from data classification
clusplot(pimadata_use[,1:8], pimadata_use[,9]-1, 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-6,4), ylim=c(-6,6),
         main=NULL,xlab='',ylab='',sub=NULL)

###
### k-Means Clustering
###

kmeans_cluster <- kmeans(pimadata_use[,1:8],2)$cluster

table(kmeans_cluster)

cor(kmeans_cluster,pimadata_use[,9])
cov(kmeans_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = kmeans_cluster-1,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(pimadata_use[,1:8], kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,4), ylim=c(-6,6),
         main=NULL,xlab='',ylab='',sub=NULL)

###
### Expectation Maximization Clustering
###

em_result <- Mclust(pimadata_use[1:8])
em_cluster <- apply(em_result$z, c(1), maxfactor)

table(em_cluster)

cor(em_cluster,pimadata_use[,9])
cov(em_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "EM"))

# Graph points and label from k-Means cluster
clusplot(pimadata_use[,1:8], em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,4), ylim=c(-6,6),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(em_result)

###
### PCA
###

pca_result <- princomp(pimadata_use[,1:8])

summary(pca_result)
loadings(pca_result)

clusplot(pca_result$scores[,1:3], pimadata_use[,9], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-6,4), ylim=c(-5,2),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(pca_result,type="lines")
biplot(pca_result)

# K-means Cluster
pca_kmeans_cluster <- kmeans(pca_result$scores[,1:3],2)$cluster

cor(pca_kmeans_cluster,pimadata_use[,9])
cov(pca_kmeans_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = pca_kmeans_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(pca_result$scores[,1:3], pca_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,4), ylim=c(-5,2),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Clustering
pca_em_result <- Mclust(pca_result$scores[,1:3])
pca_em_cluster <- apply(pca_em_result$z, c(1), maxfactor)

cor(pca_em_cluster,pimadata_use[,9])
cov(pca_em_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = pca_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "EM"))

# Graph points and label from k-Means cluster
clusplot(pca_result$scores[,1:3], pca_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,4), ylim=c(-5,2),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### ICA
###

ica_result <- fastICA(pimadata_use[,1:8], 4)

clusplot(ica_result$S, pimadata_use[,9], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-5,5), ylim=c(-4,5),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(ica_result$S[,1], type="l",xlab='',ylab='')
plot(ica_result$S[,2], type="l",xlab='',ylab='')
plot(ica_result$S[,3], type="l",xlab='',ylab='')
plot(ica_result$S[,4], type="l",xlab='',ylab='')

#boxplot(ica_result$W)

# K-means Cluster
ica_kmeans_cluster <- kmeans(ica_result$S,2)$cluster

cor(ica_kmeans_cluster,pimadata_use[,9])
cov(ica_kmeans_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = ica_kmeans_cluster-1,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(ica_result$S, ica_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-5,5), ylim=c(-4,5),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
ica_em_result <- Mclust(ica_result$S)
ica_em_cluster <- apply(ica_em_result$z, c(1), maxfactor)

cor(ica_em_cluster,pimadata_use[,9])
cov(ica_em_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = ica_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "EM"))

# Graph points and label from k-Means cluster
clusplot(ica_result$S, ica_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-5,5), ylim=c(-4,5),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### Random Components Analysis
###

rca_result <- rca(pimadata_use[,1:8])

plot(rca_result)

clusplot(rca_result, pimadata_use[,9], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-4,6), ylim=c(-3,5),
         main=NULL,xlab='',ylab='',sub=NULL)


# K-means Cluster
rca_kmeans_cluster <- kmeans(rca_result,2)$cluster

cor(rca_kmeans_cluster,pimadata_use[,9])
cov(rca_kmeans_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = rca_kmeans_cluster-1,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(rca_result, rca_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-4,6), ylim=c(-3,5),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
rca_em_result <- Mclust(rca_result)
rca_em_cluster <- apply(rca_em_result$z, c(1), maxfactor)

cor(rca_em_cluster,pimadata_use[,9])
cov(rca_em_cluster,pimadata_use[,9])


CrossTable(x = pimadata_use$Diagnosis-1, 
           y = rca_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "EM"))

# Graph points and label from k-Means cluster
clusplot(rca_result, rca_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-4,6), ylim=c(-3,5),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### Exploratory Factor Analysis
###

ev <- eigen(cor(pimadata_use[,1:8]))
ap <- parallel(subject=nrow(pimadata_use[,1:8]),
               var=ncol(pimadata_use[,1:8]),
               rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

plotnScree(nS, main='')

efa_result <- factanal(pimadata_use[,1:8], 3, scores="regression")

clusplot(efa_result$scores, pimadata_use[,9], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-3,5), ylim=c(-5,5),
         main=NULL,xlab='',ylab='',sub=NULL)

# K-means Cluster
efa_kmeans_cluster <- kmeans(efa_result$scores,2)$cluster

cor(efa_kmeans_cluster,pimadata_use[,9])
cov(efa_kmeans_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = efa_kmeans_cluster-1,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(efa_result$scores, efa_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-3,5), ylim=c(-5,5),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
efa_em_result <- Mclust(efa_result$scores)
efa_em_cluster <- apply(efa_em_result$z, c(1), maxfactor)

cor(efa_em_cluster,pimadata_use[,9])
cov(efa_em_cluster,pimadata_use[,9])

CrossTable(x = pimadata_use$Diagnosis-1, 
           y = efa_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("Diagnosis", "EM"))

# Graph points and label from k-Means cluster
clusplot(efa_result$scores, efa_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-3,5), ylim=c(-5,5),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### Neural Network
###

pimadata_use <- pimadata_use[,1:8]
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 1)
pimadata_use <- cbind(pimadata_use, pimadata$Diagnosis == 0)
names(pimadata_use)[9]  <- "Diabetic"
names(pimadata_use)[10] <- "Normal"

pimadata_train <- pimadata_use[1:(nrow(pimadata_use)-100), ]
pimadata_test  <- pimadata_use[(nrow(pimadata_use)-99):nrow(pimadata_use),9]
pimadata_test$Diagnosis <- as.integer(xor(1,pimadata$Diagnosis[(nrow(pimadata_use)-99):nrow(pimadata_use)]))

# PCA
pca_train <- as.data.frame(pca_result$scores[1:(nrow(pimadata_use)-100),1:3])
pca_train <- cbind(pca_train, pimadata_train$Diabetic)
pca_train <- cbind(pca_train, pimadata_train$Normal)
names(pca_train)[4] <- "Diabetic"
names(pca_train)[5] <- "Normal"

pca_test  <- as.data.frame(pca_result$scores[(nrow(pimadata_use)-99):nrow(pimadata_use),
                                      1:3])

ptm <- proc.time()

pca_model <- neuralnet(Diabetic+Normal ~
                         Comp.1 + Comp.2 + Comp.3,
                       data = pca_train,
                       hidden = 0)

cat("Time:",(proc.time() - ptm),"\n")

plot(pca_model)

pca_prediction <- compute(pca_model, pca_test)$net.result

pca_classification <- apply(pca_prediction, c(1), maxfactor)-1
#pca_prediction <- c('Diabetic', 'Normal')[pca_classification]

table(pimadata_test$Diagnosis,pca_classification)

# ICA
ica_train <- as.data.frame(ica_result$S[1:(nrow(pimadata_use)-100),1:4])
ica_train <- cbind(ica_train, pimadata_train$Diabetic)
ica_train <- cbind(ica_train, pimadata_train$Normal)
names(ica_train)[5] <- "Diabetic"
names(ica_train)[6] <- "Normal"

ica_test  <- as.data.frame(ica_result$S[(nrow(pimadata_use)-99):nrow(pimadata_use),
                                             1:4])

ptm <- proc.time()

ica_model <- neuralnet(Diabetic+Normal ~
                         V1 + V2 + V3 + V4,
                       data = ica_train,
                       hidden = 0)

cat("Time:",(proc.time() - ptm),"\n")

plot(ica_model)

ica_prediction <- compute(ica_model, ica_test)$net.result
ica_classification <- apply(ica_prediction, c(1), maxfactor)-1

table(pimadata_test$Diagnosis,ica_classification)

# RCA
rca_train <- as.data.frame(rca_result[1:(nrow(pimadata_use)-100),])
rca_train <- cbind(rca_train, pimadata_train$Diabetic)
rca_train <- cbind(rca_train, pimadata_train$Normal)
names(rca_train)[3] <- "Diabetic"
names(rca_train)[4] <- "Normal"

rca_test  <- as.data.frame(rca_result[(nrow(pimadata_use)-99):nrow(pimadata_use),
                                             1:2])

ptm <- proc.time()

rca_model <- neuralnet(Diabetic+Normal ~
                         V1 + V2,
                       data = rca_train,
                       hidden = 0)

cat("Time:",(proc.time() - ptm),"\n")

plot(rca_model)

rca_prediction <- compute(rca_model, rca_test)$net.result
rca_classification <- apply(rca_prediction, c(1), maxfactor)-1

table(pimadata_test$Diagnosis,rca_classification)

# EFA
efa_train <- as.data.frame(efa_result$scores[1:(nrow(pimadata_use)-100),])
efa_train <- cbind(efa_train, pimadata_train$Diabetic)
efa_train <- cbind(efa_train, pimadata_train$Normal)
names(efa_train)[4] <- "Diabetic"
names(efa_train)[5] <- "Normal"

efa_test  <- as.data.frame(efa_result$scores[(nrow(pimadata_use)-99):nrow(pimadata_use),
                                      1:3])

ptm <- proc.time()

efa_model <- neuralnet(Diabetic+Normal ~
                         Factor1 + Factor2 + Factor3,
                       data = efa_train,
                       hidden = 0)

cat("Time:",(proc.time() - ptm),"\n")

plot(efa_model)

efa_prediction <- compute(efa_model, efa_test)$net.result
efa_classification <- apply(efa_prediction, c(1), maxfactor)-1

table(pimadata_test$Diagnosis,efa_classification)


###
### Clustered Neural Network
###

comp_data <- as.data.frame(cbind(kmeans_cluster,
                                 em_cluster,
                                 pca_kmeans_cluster,
                                 pca_em_cluster,
                                 ica_kmeans_cluster,
                                 ica_em_cluster,
                                 rca_kmeans_cluster,
                                 rca_em_cluster,
                                 efa_kmeans_cluster,
                                 efa_em_cluster))
comp_data <- cbind(comp_data, pimadata_use$Diabetic)
comp_data <- cbind(comp_data, pimadata_use$Normal)
names(comp_data)[11] <- "Diabetic"
names(comp_data)[12] <- "Normal"

comp_training <- comp_data[1:292,]
comp_testing  <- comp_data[293:392,]

ptm <- proc.time()

comp_model <- neuralnet(Diabetic+Normal ~
                          kmeans_cluster + em_cluster +
                          pca_kmeans_cluster + pca_em_cluster +
                          ica_kmeans_cluster + ica_em_cluster + 
                          rca_kmeans_cluster + rca_em_cluster + 
                          efa_kmeans_cluster + efa_em_cluster,
                          data = comp_training,
                          hidden = 0)

cat("Time:",(proc.time() - ptm),"\n")

plot(comp_model)

comp_prediction <- compute(comp_model, comp_testing[,1:10])$net.result
comp_classification <- apply(comp_prediction, c(1), maxfactor)-1

table(pimadata_test$Diagnosis,comp_classification)



