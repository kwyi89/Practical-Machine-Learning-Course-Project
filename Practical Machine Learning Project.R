library(caret)
library(randomForest)

# While loading the data, replace columns with empty values with NA
if (!file.exists("pml-testing.csv")) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(fileURL, "pml-testing.csv")
}

if (!file.exists("pml-training.csv")) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(fileURL, "pml-training.csv")
}

pml_testing <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))
pml_training <- read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!",""))

# Delete all columns with NA values.
pml_training <- pml_training[,colSums(is.na(pml_training)) == 0]
pml_testing <- pml_testing[,colSums(is.na(pml_testing)) == 0]


pml_training   <-pml_training[,-c(1:7)]
pml_testing <-pml_testing[,-c(1:7)]

# Set the seed to make this analysis reproducible
set.seed(1324)

# Make the percentage of the data to be 90% training
sample <- createDataPartition(pml_training$class,list=F,p=0.9)
subtraining <- pml_training[sample,]
subtesting <- pml_training[-sample,]

# Prediction model using random forest
training_rf <- randomForest(classe ~., data=subtraining, method="class")
prediction <- predict(training_rf, subtesting, type = "class")

# Show the result using testing data set
confusionMatrix(prediction, subtesting$classe)

final_answer <- predict(training_rf, pml_testing, type="class")