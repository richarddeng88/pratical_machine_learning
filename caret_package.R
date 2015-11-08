library(caret)
library(kernlab)
data(spam)
## data spliting
intrain <- createDataPartition(y=spam$type,p=0.75, list=F)
training <- spam[intrain,]
testing <- spam[-intrain,]

## train the training data using glm, 
set.seed(1)
modelfit <- train(type~., data = training, method= "glm")
    # here we can add an argument using preprocess()
modelfit <- train(type~., data = training, preProcess=c("center","scale"), method= "glm")
modelfit
modelfit$finalModel

prediction <- predict(modelfit, newdata=testing)
prediction

confusionMatrix(prediction, testing$type)

## standardize -- the training set using preProcess function
preobj <- preProcess(training[,-58], method = c("center","scale"))  ## a process center and scale the data
ccc <- predict(preobj,training[,-58]) ## apply the process to the data using predict()
mean(ccc$capitalAve) ##check the mean for one column
sd(ccc$capitalAve) ## check the sd for one column, and sd must equal 1

par(mfrow=c(1,2)); hist(ccc$capitalAve); qqnorm(ccc$capitalAve)

     # we can apply the same prepross to the same testing data. 
ttt <- predict(preobj, testing[,-58])
mean(ttt$capitalAve)
sd(ttt$capitalAve)



## standardize -- BOX-COX transforms
preobj <- preProcess(training[,-58], method = c("BoxCox"))  ## try make them look like normal data using maximun likihood
ccc <- predict(preobj,training[,-58]) ## apply the process to the data using predict()
mean(ccc$capitalAve) ##check the mean for one column
sd(ccc$capitalAve) ## check the sd for one column, and sd must equal 1

par(mfrow=c(1,2)); hist(ccc$capitalAve); qqnorm(ccc$capitalAve)

    # we can apply the same prepross to the same testing data. 
ttt <- predict(preobj, testing[,-58])
mean(ttt$capitalAve)
sd(ttt$capitalAve)



## standardize -- Imputing data









