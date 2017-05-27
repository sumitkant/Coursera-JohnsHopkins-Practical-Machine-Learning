## QUES 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(ElemStatLearn)
library(caret)

data(vowel.train)

data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)

mod.rf <- train( y~., data = vowel.train, method = "rf")
mod.gbm <- train( y~., data = vowel.train, method = "gbm")

pred.rf <- predict(mod.rf, vowel.test)
pred.gbm <- predict(mod.gbm, vowel.test)

confusionMatrix(pred.rf, vowel.test$y)$overall
confusionMatrix(pred.gbm, vowel.test$y)$overall

predDF <- data.frame(pred.rf, pred.gbm, y = vowel.test$y)

sum(pred.rf[predDF$pred.rf == predDF$pred.gbm] == 
      predDF$y[predDF$pred.rf == predDF$pred.gbm]) / 
  sum(predDF$pred.rf == predDF$pred.gbm)

## QUES 2 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

#  MODELS
mod.rf <- train( diagnosis~., data = training, method = "rf")

mod.gbm <- train( diagnosis~., data = training, method = "gbm")

mod.lda <- train( diagnosis~., data = training, method = "lda")

# PREDICTIONS
pred.rf <- predict(mod.rf, testing)

pred.gbm <- predict(mod.gbm, testing)

pred.lda <- predict(mod.lda, testing)

# STACKED PREDICTORS  
predDF <- data.frame(pred.rf,pred.gbm, pred.lda, diagnosis = testing$diagnosis)

combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)

combPred <- predict(combModFit, predDF)

# ACCURACY
confusionMatrix(combPred, testing$diagnosis)$overall[1]

confusionMatrix(pred.rf, testing$diagnosis)$overall[1]

confusionMatrix(pred.gbm, testing$diagnosis)$overall[1]

confusionMatrix(pred.lda, testing$diagnosis)$overall[1]

# QUES 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

mod.fit <- train(CompressiveStrength~., data = training, method = "lasso")

mod.fit$finalModel

plot.enet(mod.fit$finalModel, xvar = "penalty")


# QUES 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(lubridate) # For year() function below

dat = read.csv("Week 4/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

tstest = ts(testing$visitsTumblr)

library(forecast)

fit <- bats(tstrain)

fcast <- forecast(fit, level = 95, h = dim(testing)[1])

sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]
  
# QUES 5 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

library(e1071)

mod.svm <- svm(CompressiveStrength ~ ., data = training)

pred.svm <- predict(mod.svm, testing)

accuracy(pred.svm, testing$CompressiveStrength)
