library(caret);library(kernlab); data(faithful); fa <- faithful; set.seed(333)
## data spliting
intrain <- createDataPartition(y=fa$waiting,p=0.5, list=F)
trainfa <- fa[intrain,]
testfa <- fa[-intrain,]

plot(trainfa$waiting, trainfa$eruptions, xlab="waiting", ylab="duration", pch=19, col="blue")
lm1 <- lm(eruptions~waiting, data=trainfa)
summary(lm1)
plot(trainfa$waiting, trainfa$eruptions, xlab="waiting", ylab="duration", pch=19, col="blue")
lines(trainfa$waiting, lm1$fitted,lwd=3)

newdata <- data.frame(waiting=80)
predict(lm1, newdata)

    # calculate RMSE ON training
sqrt(sum((lm1$fitted - trainfa$eruptions)^2))

sqrt(sum((predict(lm1, newdata=testfa)- testfa$eruptions)^2))


    ## same process with caret



## with multiple covariates
library(caret);library(ISLR);library(ggplot2); data(Wage); wage <- Wage; 
wage <- subset(wage, select=-c(logwage))
summary(wage)

## data spliting
intrain <- createDataPartition(y=fa$waiting,p=0.5, list=F)
trainfa <- fa[intrain,]
testfa <- fa[-intrain,]


















