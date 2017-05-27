# QUES 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

## Subsetting data based on case variable

inTrain <- createDataPartition(y = segmentationOriginal$Case, p =0.7, list = FALSE )
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain,]
set.seed(125)

# Fitting CART model using rpart method
modelFit <- train(Class ~ .,
                  data = training,
                  method ="rpart")
modelFit$finalModel

library(rpart.plot)
library(rattle)
fancyRpartPlot(modelFit$finalModel)


# QUES 3
library(pgmm)
data(olive)
olive = olive[,-1]

inTrain <- createDataPartition(y = olive$Area, p =0.7, list = FALSE )
training <- olive[inTrain, ]
testing <- olive[-inTrain,]
newdata = as.data.frame(t(colMeans(olive)))

modelFit <- train(Area~., data = olive, method = "rpart")
modelFit$finalModel

fancyRpartPlot(modelFit$finalModel)

predict(modelFit, newdata)


# Ques 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

names(trainSA)
set.seed(13234)
modelFt <- train(chd ~ (tobacco + age + typea + obesity +ldl + alcohol), 
                 data =  trainSA,
                 method = "glm",
                 family = "binomial"
                 )
prediction <- predict(modelFt, newdata = testSA)
predictiontrain <- predict(modelFt, newdata = trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# Misc
missClass(testSA$chd, prediction)
missClass(trainSA$chd, predictiontrain)


# Ques 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
as.factor(vowel.test$y)
as.factor(vowel.train$y)

set.seed(33833)
rf <- train(y~., data = vowel.train, 
            method = "rf",
            importance = TRUE)

varImp(rf)

rf2 <- randomForest(y~., data = vowel.train)
importance(rf2,type = 2)
