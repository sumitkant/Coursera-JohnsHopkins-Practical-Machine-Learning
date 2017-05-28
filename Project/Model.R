# Load Training Data set
pml <- read.csv("pml-training.csv")
str(pml)

# Look for mssing values
colmeans <- sapply(pml, function(x){mean(is.na(x))})
missing <- colmeans > 0
pml.1 <- pml[,!missing]

# Near Zero variance variables
library(caret)
nzv <- nearZeroVar(pml.1, saveMetrics = T)
pml.2 <- pml.1[,!nzv$nzv]
pml.2 <- pml.2[,-1]

# Subsetting for subject 1
carlitos <- subset(pml.2, pml.2$user_name == "carlitos")
dim(carlitos)

# Singular Value Decomposition
numeric <- sapply(carlitos, is.numeric)
carlitos.numeric <- carlitos[,numeric]
svd1 = svd(scale(carlitos.numeric))

# PLotting Left Singular vector
ncol(svd1$u)
par(mfrow=c(5,11), mar = c(2,2,1,1))
for (x in 1:55){
  plot(svd1$u[,x], pch = 20, col = carlitos$classe)
}

# Finding max conributor
ncol(svd1$v)
par(mfrow=c(5,11), mar = c(2,2,1,1))
for (x in 1:55){
  plot(svd1$v[,x], pch = 20, col = carlitos$classe)
  abline()
}

names(carlitos)

maxContrib <- which.max(svd1$v[,2])
maxContrib2 <- which.max(svd1$v[,5])

distanceMatrix <- dist(carlitos[,c(maxContrib2,maxContrib)])
hclustering <- hclust(distanceMatrix)
source("mypclust.R")
par(mfrow =c(1,1))
myplclust(hclustering, lab.col = unclass(carlitos$classe))

names(carlitos)[maxContrib]
names(carlitos)[maxContrib2]


# K means clustering
kclust <- kmeans(carlitos.numeric, centers = 5, nstart = 150)
table(kclust$cluster, carlitos$classe)
par(mfrow = c(1,1), mar = c(5,4,1,1))


library(caret)
set.seed(123)
system.time(boostFit <- train(classe ~ ., method = "gbm", 
                  data = pml.2, 
                  verbose = F, 
                  trControl = trainControl(method = "cv", number = 10)))
boostFit
plot(boostFit)

testing <- read.csv("pml-testing.csv", header  = T)
predictions <- predict(boostFit, newdata = testing)
predictions


# system.time(boostFit.nocv <- train(classe ~ ., method = "gbm", 
#                               data = pml.2, 
#                               verbose = F))
# 
# 
