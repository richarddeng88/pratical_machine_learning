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


## PCA in R
smallspam <- spam[,c(32,34)]
pr <- prcomp(smallspam)
plot(pr$x[,1],pr$x[,2]) # we plot the first PC and the second PC, 

pr$rotation # PC1 is = 0.7061498* num858 + 0.7080625 * num415

## PCA on the whole spam data
pr <- prcomp(log10(spam[,-58]+1)) # run PCA on the whole spam data, and we use log10 +1 to make the data look a little bit more gaussian, becasue some of the variables are very unmormal looking. often we do that for PCA
typecolor <- ((spam$type=="spam")*1+1)
plot(pr$x[,1],pr$x[,2],col= typecolor, xlab="PC1", ylab="PC2")

# PCA with caret
pre <- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp = 2) # pcaComp - the number of PCA component to compute
spamPC <- predict(pre, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col= typecolor)

# using the PCs to run glm regression
pre <- preProcess(log10(training[,-58]+1), method = "pca", pcaComp = 2) 
trainPC <- predict(pre, log10(training[,-58]+1))
modelfit <- train(training$type~., method="glm", data= trainPC)

testPC <- predict(pre, log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelfit, testPC))


## just conbine into one preporcess() function
modelfit <- train(training$type~., method="glm", preProcess="pca", data=training) # one difference in this way is that you didn't define how many PCs are using, the accuracy is a litter different with the previous code. 
confusionMatrix(testing$type,predict(modelfit, testing))

