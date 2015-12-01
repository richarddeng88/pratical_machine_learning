##DATA READING
train_pml <- read.csv("data/practical_machine_l/pml-training.csv",stringsAsFactors = T)
test_pml <- read.csv("data/practical_machine_l/pml-testing.csv",stringsAsFactors = T)


## PREPROCESSING

    # DATA SPLITING
    library(caret); set.seed(1001)
    intrain <- createDataPartition(y=train_pml$classe, p=0.8, list = F)
    training <- train_pml[intrain,]
    validation <- train_pml[-intrain,]

    # FEATURE SELECTION (PREPROCESSING)
        # DEAL COMLUMNS WITH NAs
        na <- sapply(test_pml, function(x){sum(is.na(x))})
        training <- subset(training, select = !(names(training) %in% names(na[na==20])))
        #test_na <- subset(test_pml, select = !(names(test_pml) %in% names(na[na==20])))
    
        # REMOVE ZERO COVARIATE
        nsv <- nearZeroVar(training) # optiong:  nearZeroVar(training,saveMetrics = T)
                                    # nearZeroVar diagnoses predictors that has one unique value
                                    # or that have very few unique values. 
        training <- training[,-nsv]

        # REMOVE DESCREIPTIVE FEATURES. 
        excludecols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                          "cvtd_timestamp", "num_window")
        training <- training[, !names(training) %in% excludecols]
        
## CORRELATION CHECK, #####I DON'T KNOW WHAT I SHOULD DO.#####
M <- abs(cor(training[,c(-3,-4,-58)])) # leave out the charater type
diag(M) <- 0 # every variable has correlation 1 with itself. So i don't need to care the diag(M)
which(M>0.8, arr.ind = T)

## MODEL TRAIN(RANDOM FOREST) - WE WILL USE random forest as our model IMPLEMENTED IN THE randomForest PACKAGE
library(randomForest)
rfModel <- randomForest(classe~., data=training, importance= T, ntrees=10)

## MODEL VALIDATION - VALIDATION SET ACCURACY
pre_validation <- predict(rfModel,validation)
print(confusionMatrix(pre_validation, validation$classe)) ## acuurancy = 0.9967


## TESTSET PREDICTION
pre_test <- predict(rfModel,test_pml)
pre_test

## OUTPUT THE RESULT ACCORDING TO THE INSTRUCTURE. 
answers <- as.vector(pre_test)

pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                    col.names = FALSE)
    }
}

pml_write_files(answers)