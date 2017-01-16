# Nathan Harmon
# CS7641, Spring 2015, Unsupervised Learning and Dimensionality Reduction
# Wine Quality Dataset

# Uncomment if you need to install
#install.packages("cluster")
#install.packages("mclust")
#install.packages("fastICA")
#install.packages("nFactors")
#install.packages("gmodels")
require("cluster")
require("mclust")
require("fastICA")
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

#Data From: http://archive.ics.uci.edu/ml/datasets/Wine+Quality
winedata <- read.csv("winequality.csv")

winedata_use <- as.data.frame(lapply(winedata[1:11], scale))
winedata_use[12] <- winedata[12]

clusplot(winedata_use[,1:11], winedata_use[,12], 
         color=TRUE, shade=FALSE, 
         labels=4, lines=0, xlim=c(-10,10), ylim=c(-6,15),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### k-Means Clustering
###

kmeans_cluster <- kmeans(winedata_use[,1:11],10)$cluster

table(kmeans_cluster)
cor(kmeans_cluster,winedata_use[,12])
cov(kmeans_cluster,winedata_use[,12])

# Graph points and label from k-Means cluster
clusplot(winedata_use[,1:11], kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-10,10), ylim=c(-6,15),
         main=NULL,xlab='',ylab='',sub=NULL)

###
### Expectation Maximization Clustering
###

em_result <- Mclust(winedata_use[,1:11])
em_cluster <- apply(em_result$z, c(1), maxfactor)

table(em_cluster)
cor(em_cluster,winedata_use[,12])
cov(em_cluster,winedata_use[,12])

clusplot(winedata_use[,1:11], em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-10,10), ylim=c(-6,15),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(em_result)

###
### PCA
###

pca_result <- princomp(winedata_use[,1:11])

summary(pca_result)
loadings(pca_result)

clusplot(pca_result$scores[,1:3], winedata_use[,12], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-5,8), ylim=c(-5,7),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(pca_result,type="lines")
biplot(pca_result)

# K-means Cluster
pca_kmeans_cluster <- kmeans(pca_result$scores[,1:3],10)$cluster

cov(pca_kmeans_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = pca_kmeans_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(pca_result$scores[,1:3], pca_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-5,8), ylim=c(-5,7),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Clustering
pca_em_result <- Mclust(pca_result$scores[,1:3])
pca_em_cluster <- apply(em_result$z, c(1), maxfactor)

cov(pca_em_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = pca_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(pca_result$scores[,1:3], pca_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-5,8), ylim=c(-5,7),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### ICA
###

ica_result <- fastICA(winedata_use[,1:11], 4)

clusplot(ica_result$S, winedata_use[,12], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-6,12), ylim=c(-10,5),
         main=NULL,xlab='',ylab='',sub=NULL)

plot(ica_result$S[,1], type="l",xlab='',ylab='')
plot(ica_result$S[,2], type="l",xlab='',ylab='')
plot(ica_result$S[,3], type="l",xlab='',ylab='')
plot(ica_result$S[,4], type="l",xlab='',ylab='')

#boxplot(ica_result$W)

# K-means Cluster
ica_kmeans_cluster <- kmeans(ica_result$S,10)$cluster

cor(ica_kmeans_cluster,winedata_use[,12])
cov(ica_kmeans_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = ica_kmeans_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(ica_result$S, ica_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,12), ylim=c(-10,5),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
ica_em_result <- Mclust(ica_result$S)
ica_em_cluster <- apply(ica_em_result$z, c(1), maxfactor)

cor(ica_em_cluster,winedata_use[,12])
cov(ica_em_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = ica_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(ica_result$S, ica_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-6,12), ylim=c(-10,5),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### Random Components Analysis
###

rca_result <- rca(winedata_use[,1:11])

plot(rca_result)

clusplot(rca_result, winedata_use[,12], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-7,7), ylim=c(-10,8),
         main=NULL,xlab='',ylab='',sub=NULL)


# K-means Cluster
rca_kmeans_cluster <- kmeans(rca_result,10)$cluster

cor(rca_kmeans_cluster,winedata_use[,12])
cov(rca_kmeans_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = rca_kmeans_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(rca_result, rca_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-7,7), ylim=c(-10,8),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
rca_em_result <- Mclust(rca_result)
rca_em_cluster <- apply(rca_em_result$z, c(1), maxfactor)

cor(rca_em_cluster,winedata_use[,12])
cov(rca_em_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = rca_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(rca_result, rca_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-7,7), ylim=c(-10,8),
         main=NULL,xlab='',ylab='',sub=NULL)


###
### Exploratory Factor Analysis
###

ev <- eigen(cor(winedata_use[,1:11]))
ap <- parallel(subject=nrow(winedata_use[,1:11]),
               var=ncol(winedata_use[,1:11]),
               rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

plotnScree(nS, main='')

efa_result <- factanal(winedata_use[,1:11], 3, scores="regression")

clusplot(efa_result$scores, winedata_use[,12], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-4,8), ylim=c(-17,10),
         main=NULL,xlab='',ylab='',sub=NULL)

# K-means Cluster
efa_kmeans_cluster <- kmeans(efa_result$scores,10)$cluster

cor(efa_kmeans_cluster,winedata_use[,12])
cov(efa_kmeans_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = efa_kmeans_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "k-Means"))

# Graph points and label from k-Means cluster
clusplot(efa_result$scores, efa_kmeans_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-4,8), ylim=c(-17,10),
         main=NULL,xlab='',ylab='',sub=NULL)

# EM Cluster
efa_em_result <- Mclust(efa_result$scores)
efa_em_cluster <- apply(efa_em_result$z, c(1), maxfactor)

cor(efa_em_cluster,winedata_use[,12])
cov(efa_em_cluster,winedata_use[,12])

CrossTable(x = winedata_use$quality, 
           y = efa_em_cluster,
           prop.r = FALSE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE,
           dnn = c("quality", "EM"))

# Graph points and label from k-Means cluster
clusplot(efa_result$scores, efa_em_cluster, 
         color=TRUE, shade=TRUE, 
         labels=4, lines=0, xlim=c(-4,8), ylim=c(-17,10),
         main=NULL,xlab='',ylab='',sub=NULL)
