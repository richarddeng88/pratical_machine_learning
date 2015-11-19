# Q3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


select <- grep("^IL",names(training))
train_3 <- training[,select]

pc <- preProcess(train_3, method = "pca", pcaComp = 3)
pc <- preProcess(train_3, method = "pca", thresh = 0.8)
pc$rotation


#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# FEATURE SELECTION : select features starting with "IL" name. 
select <- grep("^IL",names(training))
train_4 <- training[,c(select,1)]
test_4 <- testing[,c(select,1)]

# TRAIN MODEL : LOGISTIC REGRESSION : on the original data
    library(caret)
    model <- train(train_4$diagnosis ~. , method = "glm", data=train_4)
    pred <- predict(model, testing)
    confusionMatrix(pred, testing$diagnosis)

# IMPROVE PERFORMANCE 
    # TRAIN MODEL : PCA REGRESSION : find PCs that explains 80% of total variance, then glm using these PCs
    pc <- preProcess(train_4[,1:12], method = "pca", thresh = 0.8)  #
    
    pc_train <- predict(pc, train_4[,1:12]) # build new PCs for traiing 
    pc_test <- predict(pc, test_4[,1:12])   # build new PCs for testing
    
    model <- train(train_4$diagnosis ~. , method = "glm", data=pc_train)
    pred <- predict(model, pc_test)
    confusionMatrix(pred, test_4$diagnosis) # we see that accuracy increased from 0.65 to 0.72

# CODE IMPROVEMENT -- however, the accuracy is not exatly same with the code before, why? 
    model <- train(train_4$diagnosis ~. , method="glm", preProcess="pca", data = train_4)
    confusionMatrix(test_4$diagnosis, predict(model, test_4))