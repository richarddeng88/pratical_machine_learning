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



## standardize -- Imputing data ( K nearest imputation)
set.seed(13343)

  # make some values NA
selectNA <- rbinom(dim(training)[1],size=1, prob = 0.5)==1
training$cap <- training$capitalAve
training$cap[selectNA] <- NA  # the newly created variable has 50% NA

  # impute and standardize using ( k nearest imputation)
preobj <- preProcess(training[,-58], method = "knnImpute")
cap <- predict(preobj, training[,-58])

  # standardize the value
mean(cap$cap)
sd(cap$cap)
cap$cap <- (cap$cap - mean(cap$cap))/sd(cap$cap)
mean(cap$cap)
sd(cap$cap)

  # check if the imputation works relativly well
quantile(cap$cap-cap$capitalAve) # most of them are very close to 0. 
quantile((cap$cap-cap$capitalAve)[selectNA]) # just see the NA vluse we created. 



## 
library(caret)
library(kernlab)
data(spam)
## data spliting
intrain <- createDataPartition(y=spam$type,p=0.75, list=F)
training <- spam[intrain,]
testing <- spam[-intrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0 # every variable has correlation 1 with itself. So i don't need to care the diag(M)
which(M>0.8, arr.ind = T)

names(spam)[c(32,34)]
plot(spam[,32],spam[,34])




