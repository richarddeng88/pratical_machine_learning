library(caret)
library(kernlab)
data(spam)
## data spliting
intrain <- createDataPartition(y=spam$type,p=0.75, list=F)
training <- spam[intrain,]
testing <- spam[-intrain,]

set.seed(1)
modelfit <- train(type~., data = training, method= "glm")
modelfit
modelfit$finalModel

prediction <- predict(modelfit, newdata=testing)
prediction

confusionMatrix(prediction, testing$type)




