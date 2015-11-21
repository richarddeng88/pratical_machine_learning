#Basic idea: a .Resample cases and recalculate prediction; b. Average or majority vote
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess - I resample my Ozone data 10 different times , refit the model for 10 deifferent times, 
                # and then average those values
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
    ss <- sample(1:dim(ozone)[1],replace=T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2) # span being how smooth that line will be
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155)) # predict on the exact same data itself
}

    # plot the results. 
    plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
    for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
    lines(1:155,apply(ll,2,mean),col="red",lwd=2)
    ## the proof shows bagging always has a lower varability but a similar bias. 
    
# alternatively, you can build your bagging function in caret by yourself. 
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate)) 
        # B=10, the number of the subsamples i like to take in the example
        # bagcontrol tells me how i am gonna to fit the model. fit, predict, aggregate. read the document carefully. 



#Example of custom bagging (continued)
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

# different parts of the bagging function
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate






