# QUES 4 and 5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,c(1,58:69)]
testing = adData[-inTrain,c(1,58:69)]

# Preprocess
preObj <- preProcess(training[,-1], method="pca", thresh = 0.8)

modelwithoutpca <- train( diagnosis~.,
                          data = training,
                          method = "glm")

modelwithPCA <- train(diagnosis~., pcaComp = 7, method="glm", preProcess= "pca", data = training)

confusionMatrix(predict(modelwithPCA, newdata = testing[,-1]),testing$diagnosis)
confusionMatrix(predict(modelwithoutpca, newdata = testing[,-1]),testing$diagnosis)


# QUES 2 and 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
?cut2
library(ggplot2)
qplot(concrete$index, concrete$CompressiveStrength , color = cut2(concrete$Age,  g =10))

cor(concrete$CompressiveStrength, concrete$FlyAsh)
qplot((concrete$Superplasticizer))
