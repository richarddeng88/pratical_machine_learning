########################## KNN METHOD

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
        training_knn<- cbind(t_n[-20:-1,], training_knn[53])
        testing_knn <- t_n[19623:19642,]
        # DATA SPLITING
        library(caret); set.seed(1001)
        intrain <- createDataPartition(y=training_knn$classe, p=0.8, list = F)
        training_body <- training_knn[intrain,][,-53]
        validation_body <- training_knn[-intrain,][,-53]
        training_label <- training_knn[intrain,][,53]
        validation_label <- training_knn[-intrain,][,53]

## CORRELATION CHECK, #####I DON'T KNOW WHAT I SHOULD DO.#####
M <- abs(cor(training[,c(-3,-4,-58)])) # leave out the charater type
diag(M) <- 0 # every variable has correlation 1 with itself. So i don't need to care the diag(M)
which(M>0.8, arr.ind = T)

##======================================================================================
library(class)
knn_values=c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,110,120,140,180,200)
t=0
knn.accuracy= rep(0,length(knn_values))
for (i in knn_values){
    t=t+1
    pred_vlidation=knn(training_body,validation_body, training_label, k=i)
    knn.accuracy[t]= mean(pred_vlidation == validation_label)
}
knn.accuracy

# [1] 0.9869997 0.9780780 0.9737446 0.9676268 0.9658425 0.9518226 0.9528422 0.9457048 0.9403518 0.9357634
#[11] 0.8852919 0.8557227 0.8281927 0.8083100 0.7856232 0.7675249 0.7540148 0.7420342 0.7313281 0.7239358
#[21] 0.7142493 0.7015040 0.6719347 0.6609737

plot(knn_values, knn.accuracy, type = "l")
##======================================================================================

## VALIDATION PREDICTION and EVALUATION- WE WILL USE KNN as our model IMPLEMENTED IN THE class PACKAGE
library(class)
pred_vlidation<- knn(training_body,validation_body, training_label, k=100 )
print(confusionMatrix(pred_vlidation, validation_label))  
#report: if not normalize, the ACUURACY IS ONLY 0.65 (k=100)
#        after mormalization the accuracy has been increased to 0.7346 (k=100)

## PERFORMACE INCREASE
    
    # try different K - k=50
    pred_vlidation<- knn(training_body,validation_body, training_label, k=50 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    ##   when change k =50, accuracy has improved to 0.7917
    
    # try different K - k=20    
    pred_vlidation<- knn(training_body,validation_body, training_label, k=20 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    ##   when change k =50, accuracy has improved to 0.8766    
    
    # try different K - k=5 
    pred_vlidation<- knn(training_body,validation_body, training_label, k=5 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    ##   when change k =5, accuracy has improved to 0.9618
    
    # try different K - k=2 
    pred_vlidation<- knn(training_body,validation_body, training_label, k=2 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    ##   when change k =2, accuracy has improved to 0.9824  
    
    # try different K - k=1 
    pred_vlidation<- knn(training_body,validation_body, training_label, k=1 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    ##   when change k =1, accuracy has improved to 0.9898  

# PERFORMANCE INCREASE 2
    # USE Z-SCORE SDANDARDIZE DATA
    t_z <- as.data.frame(scale(training_knn[,-53]))
    training_knn_z<- cbind(t_z, training_knn[53])
    
    # DATA SPLITING
    library(caret); set.seed(1001)
    intrain <- createDataPartition(y=training_knn_z$classe, p=0.8, list = F)
    training_body <- training_knn_z[intrain,][,-53]
    validation_body <- training_knn_z[-intrain,][,-53]
    training_label <- training_knn_z[intrain,][,53]
    validation_label <- training_knn_z[-intrain,][,53]    

# PREDICTING
    pred_vlidation<- knn(training_body,validation_body, training_label, k=50 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    # AFTER Z-SCORE standardization, accuracy imporoved to 0.8318 with k=50
    
    pred_vlidation<- knn(training_body,validation_body, training_label, k=5 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    # AFTER Z-SCORE standardization, accuracy imporoved to 0.9674 with k=5
    
    pred_vlidation<- knn(training_body,validation_body, training_label, k=2 )
    print(confusionMatrix(pred_vlidation , validation_label))  
    # AFTER Z-SCORE standardization, accuracy imporoved to 0.9819 with k=2

## TESTING DATA PREDICTION
pred_test <- knn(training_knn[,-53], testing_knn, training_knn[,53],k=2)

pred_test <- knn(training_body, testing_knn, training_label,k=5)
## OUTPUT THE RESULT ACCORDING TO THE INSTRUCTURE. 
answers <- as.vector(pred_test)

pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                    col.names = FALSE)
    }
}

pml_write_files(answers)
