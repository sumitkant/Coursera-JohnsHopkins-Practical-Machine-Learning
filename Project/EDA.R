# Load Training Data set
pml <- read.csv("pml-training.csv")
str(pml)

# Find variables with missing values
sapply(pml, function(x){mean(is.na(x))*100})

carlitos <- subset(pml, pml$user_name == "carlitos")
# Counts of the activities A being the correct method while others being incorrect
table(carlitos$classe)

# Plotting average accelaration for carlitos
par(mfrow =c(1,3), mar =c(5,4,1,1))
plot(carlitos[,116], col = carlitos$classe, ylab = names(carlitos)[116])
plot(carlitos[,117], col = carlitos$classe, ylab = names(carlitos)[117])
plot(carlitos[,118], col = carlitos$classe, ylab = names(carlitos)[118])
legend("bottomright", legend = unique(carlitos$classe), col = unique(carlitos$classe), pch =1)

# Clustering based on average acceleration
par(mfrow = c(1,1))
source("mypclust.R")
distanceMatrix <- dist(carlitos[, 116:118])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(carlitos$classe))
# Clustering is a littlble bit messy and there is no clear pattern emerging

# Look at acceleration of forearm instead of dumbell
par(mfrow =c(1,3), mar =c(5,4,1,1))
plot(carlitos[,154], col = carlitos$classe, ylab = names(carlitos)[154])
plot(carlitos[,155], col = carlitos$classe, ylab = names(carlitos)[155])
plot(carlitos[,156], col = carlitos$classe, ylab = names(carlitos)[156])
legend("bottomright", legend = unique(carlitos$classe), col = unique(carlitos$classe), pch =1)

# Total forearm acceleration

par(mfrow =c(1,1))
plot(carlitos[,140], col = carlitos$classe, ylab = names(carlitos)[140])
#Gives us nothing

# Roll pitch yaw
par(mfrow =c(1,3), mar =c(5,4,1,1))
plot(carlitos[,122], col = carlitos$classe, ylab = names(carlitos)[122])
plot(carlitos[,123], col = carlitos$classe, ylab = names(carlitos)[123])
legend("bottomright", legend = unique(carlitos$classe), col = unique(carlitos$classe), pch =1)
plot(carlitos[,124], col = carlitos$classe, ylab = names(carlitos)[1234])


# Clustering on Roll pitch yaw
par(mfrow = c(1,1))
source("mypclust.R")
distanceMatrix <- dist(carlitos[, 122:124])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(carlitos$classe))

# Max roll pitch yaw max_roll_forearm"         "max_picth_forearm"    "max_yaw_forearm"   

par(mfrow =c(1,3), mar =c(5,4,1,1))
plot(carlitos[,131], col = carlitos$classe, ylab = names(carlitos)[131])
plot(carlitos[,132], col = carlitos$classe, ylab = names(carlitos)[132])
legend("bottomright", legend = unique(carlitos$classe), col = unique(carlitos$classe), pch =1)
plot(carlitos[,133], col = carlitos$classe, ylab = names(carlitos)[133])


## Clustering on max roll pitch
par(mfrow = c(1,1))
source("mypclust.R")
distanceMatrix <- dist(carlitos[,131:133])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(carlitos$classe))



# SINGULAR VALUE DECOMPOSITION - takes only numeric arguments
nums <- sapply(carlitos, is.numeric)
carlitos <- carlitos[, nums]

svd1 = svd(scale(carlitos, center = TRUE, scale = apply(carlitos, 2, mean, na.rm = T )))

?scale

par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)



# Removing variables with NA values
pml.cleaned <- pml[, !colSums(is.na(pml))]

# Removind variables with no values
pml.final <- pml.cleaned[, !(sapply(pml.cleaned, function(x){sum(x == "")})) ]
# validation dataset

library(caret)
inTrain <- createDataPartition(y = pml.final$classe, p = 0.7, list =FALSE )
training <- pml.final[inTrain, ]
validation <- pml.final[-inTrain, ]

library(ggplot2)
qplot(classe, fill = user_name, data = training)


