train_pml <- read.csv("data/practical_machine_l/pml-training.csv",stringsAsFactors = T)
test_pml <- read.csv("data/practical_machine_l/pml-testing.csv",stringsAsFactors = T)

# DEAL WITH NA
na <- sapply(test_pml, function(x){sum(is.na(x))})
train_na <- subset(train_pml, select = !(names(train_pml) %in% names(na[na==20])))
test_na <- subset(test_pml, select = !(names(test_pml) %in% names(na[na==20])))


# TRAIN DATA SPLITTING FOR MODEL TESTING
train_na <- train_na[,c(-1,-2)]
library(caret); set.seed(1001)
intrain <- createDataPartition(y=train_na$classe, p=0.8, list = F)
training <- train_na[intrain,]
testing <- train_na[-intrain,]


## UNIQUENESS CHECK - REMOVE ZERO COVARIATE
length <- sapply(training, function(x){length(unique(x))})
length[length==2]  # there is no clumn with only one value
nsv <- nearZeroVar(training, saveMetrics = T) # nearZeroVar diagnoses predictors that has one unique value
                                        # or that have very few unique values. 


## CORRELATION CHECK 
M <- abs(cor(training[,c(-3,-4,-58)])) # leave out the charater type
diag(M) <- 0 # every variable has correlation 1 with itself. So i don't need to care the diag(M)
which(M>0.8, arr.ind = T)



