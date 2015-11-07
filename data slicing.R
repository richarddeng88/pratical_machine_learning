library(caret)
library(kernlab)
data(spam)
## data spliting
intrain <- createDataPartition(y=spam$type,p=0.75, list=F)
training <- spam[intrain,]
testing <- spam[-intrain,]

## k fold
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain = T)
sapply(folds, length)







