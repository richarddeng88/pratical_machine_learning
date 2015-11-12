pml_train <- read.csv("data/practical_machine_l/pml-training.csv", stringsAsFactors = F)
pml_test <- read.csv("data/practical_machine_l/pml-testing.csv")

#NA, ony 93 variable left

na <- sapply(pml_test, function(x){sum(is.na(x))}) 
pml_train_NA <- subset(pml_train, select = !(names(pml_train) %in% names(na[na>00])))
sum(is.na(pml_train_NA))

# deal with test data
# tna <- sapply(pml_test, function(x){sum(is.na(x))}) 
# tna[tna>0]













