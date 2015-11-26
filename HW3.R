# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
seg <- segmentationOriginal
table(seg$Class)

library(dplyr)
train <- seg %>% filter(Case =="Train") %>% select(-Case)
test <- seg %>% filter(Case =="Test") %>% select(-Case)

set.seed(125)
model <- train(Class~., method="rpart", data=train)
print(model$finalModel)

library(rattle)
fancyRpartPlot(model$finalModel)


#Q3
library(pgmm)
data(olive)
olive = olive[,-1]
table(olive$Area)

sapply(olive, function(x){sum(is.na(x))})

newdata = as.data.frame(t(colMeans(olive)))

# train the model
library(caret)
model <- train(Area~., method="rpart", data=olive)
print(model$finalModel)

predict(model, newdata)
# the result is 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average 
# value of Area as a numeric variable in the leaf predicted for newdata. 

#because the response is interger not factor. 
olive$Area <- factor(olive$Area, levels = c(1:9), labels = c(1:9))

library(caret)
model <- train(Area~., method="rpart", data=olive)
print(model$finalModel)

predict(model, newdata) # the result is 9 


#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# train the model
model <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, data= trainSA, method="glm", family="binomial")
summary(model)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

ts_pred <- predict(model, testSA)
missClass(testSA$chd, ts_pred)

tn_pred <- predict(model, trainSA)
missClass(trainSA$chd, tn_pred)


# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- factor(vowel.train$y, levels = c(1:11), labels = c(1:11))
vowel.test$y <- factor(vowel.test$y, levels = c(1:11), labels = c(1:11))

set.seed(33833)
#train the model
model <- train(y~., method="rf", data=vowel.train)
varImp(model)



