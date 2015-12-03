#Q1
library(ElemStatLearn)
data(vowel.train); data(vowel.test) 
train <- vowel.train; test <- vowel.test
train$y <- factor(train$y, levels = c(1:11), labels = c(1:11))
test$y <- factor(test$y, levels = c(1:11), labels = c(1:11))

set.seed(33833); library(caret)
rf_model <- train(y~., method="rf",data=train)  # random forest
rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, test$y) #0.61

gbm_model <- train(y~., method="gbm", data=train, verbose=FALSE) #boosting method
gbm_pred <- predict(gbm_model,test)
confusionMatrix(gbm_pred,test$y) # 0.526


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

set.seed(62433); library(caret)
rf_model <- train(diagnosis~., method="rf",data=training)  # random forest
rf_pred <- predict(rf_model, testing)

gbm_model <- train(diagnosis~., method="gbm", data=training, verbose=FALSE) #boosting method
gbm_pred <- predict(gbm_model,testing)

lda_model <- train(diagnosis~., method="lda", data=training) #LDA method
lda_pred <- predict(lda_model,testing)

predDF <- data.frame(rf_pred, gbm_pred, lda_pred, diagnosis=testing$diagnosis)
combModel <- train(diagnosis~., method="rf", data = predDF)
com_pred <- predict(combModel, predDF)

confusionMatrix(rf_pred, testing$diagnosis)$overall[1]
confusionMatrix(gbm_pred,testing$diagnosis)$overall[1]
confusionMatrix(lda_pred,testing$diagnosis)$overall[1]
confusionMatrix(com_pred, testing$diagnosis)$overall[1]


#Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
lasso_model <- train(CompressiveStrength~., data = training, method="lasso")
library(elasticnet)
plot.enet(lasso_model$finalModel, xvar="penalty", use.color=T)
#the coefficient path shows that the variable Cement is the last coefficient to be set to zero as the penalty increases.

ridge_model <- train(CompressiveStrength~., data = training, method="ridge")
library(elasticnet)
plot.enet(ridge_model$finalModel, xvar="penalty", use.color=T)


# Q4
library(lubridate)  # For year() function below
dat = read.csv("data/practical_machine_l/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
    dim(testing)[1]

#Q5
set.seed(3523); library(caret)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325); library(e1071); library(forecast)
svm_model <- svm(CompressiveStrength~., data = training)
svm_pred <- predict(svm_model, testing)
accuracy(svm_pred, testing$CompressiveStrength)

