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

## resmapling
set.seed(1)
folds2 <- createResample(y=spam$type, times = 10, list = T)
sapply(folds2, length)

## time slicing
set.seed(1)
tme <- 1:1000
folds3 <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds3)



