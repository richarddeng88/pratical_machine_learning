library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

# DATA SPLITTING
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# FEATURE PLOT
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")


qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)

qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

# CUT2 - MAKING 
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

    #Boxplots with cut2
    p1 <- qplot(cutWage,age, data=training,fill=cutWage,
                geom=c("boxplot"))
    p1
    
    #Boxplots with points overlayed
    library(ggplot2);library(gridExtra)
    p2 <- qplot(cutWage,age, data=training,fill=cutWage,
                geom=c("boxplot","jitter"))
    grid.arrange(p1,p2,ncol=2)
    
    #Density plots
    qplot(wage,colour=education,data=training,geom="density")
    
    
    
    
    
    
    
    
    