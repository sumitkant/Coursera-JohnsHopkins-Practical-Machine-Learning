# Load Training Data set
pml <- read.csv("pml-training.csv")
str(pml)

# Find variables with missing values
colSums(is.na(pml)) 

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

