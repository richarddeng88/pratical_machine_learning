library(caret)
library(kernlab)
data(spam)
## data spliting
intrain <- createDataPartition(y=spam$type,p=0.75, list=F)
training <- spam[intrain,]
testing <- spam[-intrain,]


## fit the model "glm
set.seed(1)
modelfit <- train(type~., data = training, method= "glm")
modelfit
modelfit$finalModel

args(train.default)
args(trainControl)










