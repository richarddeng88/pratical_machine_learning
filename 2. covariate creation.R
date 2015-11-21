library(caret);library(kernlab); library(ISLR)
data(Wage); wage <- Wage
intrain <- createDataPartition(y=wage$wage, p=0.7, list=F)
training <- wage[intrain,]; testing <- wage[-intrain,]

## common covariates to add - dummy variable  ( tranform from quantitative to quanlitatvie)
table(training$jobclass)
dummies <- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training)) # two new columns will be show:

## removing zero covariate
nsv <- nearZeroVar(training, saveMetrics = T) # nearZeroVar diagnoses predictors that have one unique value (i.e. are zero variance predictors)
nsv

## spline basis
library(splines)
bsbasis <- bs(training$age, df=3) # we want 3 df of polinomial for this variable
head(bsbasis,30)

    # fiting curves with splines
lm1 <- lm(wage~bsbasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata= training), col="red", pch=19, cex=0.5)

predict(bsbasis, testing$age) # i will predict this variable using bs() fuction and plung it into the prediction model 







