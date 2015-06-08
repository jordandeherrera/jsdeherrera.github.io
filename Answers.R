# Add relevant libraries
library(rmarkdown)
library(caret)
library(ggplot2)
library(randomForest)

#import data set
df <- read.csv("pml-training.csv")

# Create data frame for numeric test
numerictest <- data.frame(row = as.numeric(), test = as.logical())

# Test each column for numeric
for(i in 1:length(names(df)))
{
  newline <- data.frame(row = i, test = is.numeric(df[,i]))
  numerictest <- rbind(numerictest,newline)
}

# Select only columns that are numeric
numerictest[numerictest$test==TRUE,1]

# Create new clean data frame from which only numeric columns exist
cleandf <- df[,numerictest[numerictest$test==TRUE,1]]

# Filter "cleaned" data frame so that only columns without NAs are included for predictive purposes
cleandf <- cleandf[,colSums(is.na(cleandf)) == 0]

# Add classe back in
cleandf <- cbind(cleandf,df$classe)

# Rename column name to "classe"
colnames(cleandf)[57] <- "classe"

#Create training and testing sets
inTrain <- createDataPartition(y=cleandf$classe, p=0.7, list=FALSE)
Training <- cleandf[inTrain,]
Testing <- cleandf[-inTrain,]

#Create recursive partitioning model
modFitRPART <- train(classe ~ ., method="rpart", data=Training, preProc = c("center","scale"))

#Create confusion matrix to evaluate model
cmRPART <- confusionMatrix(Testing$classe, predict(modFitRPART,Testing))

#Create gbm model
modFitGBM <- train(classe ~ ., method="gbm", data=Training, preProc = c("center","scale"))

#Create confusion matrix to evaluate model
cmGBM <- confusionMatrix(Testing$classe, predict(modFitGBM,Testing))

#Create random forest model
modFitRF <- randomForest(x,y)

#Create confusion matrix to evaluate model
cmRF <- confusionMatrix(Testing$classe, predict(modFitRF,Testing))

# Function to submit answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}