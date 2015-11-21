data(iris); library(ggplot2)
names(iris)

# have a try k means, see if it can do prediction. the answer is not at all. 
    z_iris <- scale(iris[,1:4])
    set.seed(2345)
    c <- kmeans(z_iris, 3)
    c$cluster ; c$centers; summary(c) ; c$size
    iris$clusters <- c$cluster

# DATA SPLITING
    library(caret)
    inTrain <- createDataPartition(y=iris$Species, p=0.8, list=F)
    training <- iris[inTrain,]
    testing <- iris[-inTrain,]
    
    qplot(Petal.Width,Sepal.Width,colour=Species,data=training)
    
# TRAIN THE MODEL
    modFit <- train(Species ~ .,method="rpart",data=training)
    print(modFit$finalModel)
    
    # PLOT THE TREE
    plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
    text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
    
    #Prettier plots
    library(rattle) # require to install "rpart.plot" package first. 
    fancyRpartPlot(modFit$finalModel)
    
    #evaluatting the performance
    pred <- predict(modFit,newdata=testing)
    confusionMatrix(pred, testing$Species)
    
    
    
    
    
    
    
    
    
    