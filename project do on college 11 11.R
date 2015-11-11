train_pml <- read.csv("data/practical_machine_l/pml-training.csv")
test_pml <- read.csv("data/practical_machine_l/pml-testing.csv")

# DEAL WITH NA
na <- sapply(test_pml, function(x){sum(is.na(x))})
train_na <- subset(train_pml, select = !(names(train_pml) %in% names(na[na==20])))
test_na <- subset(test_pml, select = !(names(test_pml) %in% names(na[na==20])))








