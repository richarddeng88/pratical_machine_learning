library(ISLR)
library(caret)

set.seed(300)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = Smarket$Direction,p = 0.75,list = FALSE)
training <- Smarket[indxTrain,]
testing <- Smarket[-indxTrain,]

#Checking distibution in origanl data and partitioned data
prop.table(table(training$Direction)) * 100

prop.table(table(testing$Direction)) * 100

#Preprocessing
trainX <- training[,names(training) != "Direction"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#Training and train control
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

plot(knnFit)
knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Direction )
mean(knnPredict == testing$Direction)

#Now verifying 2 class summary function

ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit, print.thres = 0.5, type="S")

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Direction )

mean(knnPredict == testing$Direction)

#Trying to plot ROC curve to check specificity and sensitivity
library(pROC)
knnPredict <- predict(knnFit,newdata = testing , type="prob")
knnROC <- roc(testing$Direction,knnPredict[,"Down"], levels = rev(testing$Direction))
knnROC

plot(knnROC, type="S", print.thres= 0.5)

##Applying Random Forest to see the performance improvement
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)

# Random forrest
rfFit <- train(Direction ~ ., data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
plot(rfFit)

rfPredict <- predict(rfFit,newdata = testing )
confusionMatrix(rfPredict, testing$Direction )
mean(rfPredict == testing$Direction)

#With twoclasssummary
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
# Random forrest
rfFit <- train(Direction ~ ., data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

plot(rfFit)

#Trying plot with some more parameters
plot(rfFit, print.thres = 0.5, type="S")

rfPredict <- predict(rfFit,newdata = testing )
confusionMatrix(rfPredict, testing$Direction )

#Ploting ROC curve
library(pROC)
rfPredict <- predict(rfFit,newdata = testing , type="prob")
rfROC <- roc(testing$Direction,rfPredict[,"Down"], levels = rev(testing$Direction))
rfROC


plot(rfROC, type="S", print.thres= 0.5)












