##DATA READING
train_pml <- read.csv("data/practical_machine_l/pml-training.csv",stringsAsFactors = T)
test_pml <- read.csv("data/practical_machine_l/pml-testing.csv",stringsAsFactors = T)

## PREPROCESSING

    # FEATURE SELECTION
    # DEAL COMLUMNS WITH NAs
    na <- sapply(test_pml, function(x){sum(is.na(x))})
    training_knn <- subset(train_pml, select = !(names(train_pml) %in% names(na[na==20])))
    testing_knn <- subset(test_pml, select = !(names(test_pml) %in% names(na[na==20])))
    
    # REMOVE DESCREIPTIVE FEATURES. 
    excludecols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                     "cvtd_timestamp", "num_window", "new_window")
    training_knn <- training_knn[, !names(training_knn) %in% excludecols]
    testing_knn <- testing_knn[, !names(testing_knn) %in% excludecols]
    
    
    # REMOVE ZERO COVARIATE # i don't use nearZeroVar() because it's hard to do the same on test dataset
    length <- sapply(training_knn, function(x){length(unique(x))})
    length[length<3]

    # NORMALIZE DATA both training_knn and testing_knn
    normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
    tem <- rbind(training_knn[,-53], testing_knn[-53])
    t_n <- as.data.frame(lapply(tem, normalize))
    training_nu<- cbind(t_n[-20:-1,], training_knn[53])
    testing_nu<- t_n[19623:19642,]
    
    # DATA SPLITING
    library(caret); set.seed(1001)
    intrain <- createDataPartition(y=training_nu$classe, p=0.8, list = F)
    training <- training_nu[intrain,]
    validation <- training_nu[-intrain,]

## TRAINING THE MODEL
library(neuralnet)
n <- names(training)
f <- as.formula(paste('classe ~', paste(n[!n %in% 'classe'], collapse = ' + ')))
# concrete_model <- neuralnet(f, data=training)
# plot(concrete_model)

library(nnet)
a = nnet(classe~., data=training,size=5,maxit=10000)


## EVALUATING MODEL PERFORMANCE
model_result <- compute(concrete_model, testing[,1:8])
pred <- model_result$net.result
# because it's not classification problem, we use correlation instead of confusion matrix. 
cor(pred, testing$strength)

## IMPROVING PERFORMANCE
concrete_model <- neuralnet(strength~cement + slag + ash + water+ superplastic + 
                                coarseagg + fineagg + age, data=training, hidden = 5)
plot(concrete_model)

model_result <- compute(concrete_model, testing[,1:8])
pred <- model_result$net.result
cor(pred, testing$strength)
    