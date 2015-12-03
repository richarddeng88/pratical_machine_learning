#Q1
library(ElemStatLearn)
data(vowel.train); data(vowel.test) 
train <- vowel.train; test <- vowel.test

set.seed(33833); library(caret)
rf_model <- train(y~., method="rf",data=train)
rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, test$y)
confusionMatrix(test$y, rf_pred)$overall['Accuracy']

gbm_model <- train(y~., method="gbm", data=train)

rfmodel <- suppressMessages(train(y~., data=vowel.train, method="rf"))

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]








