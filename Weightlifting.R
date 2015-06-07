# Add relevant libraries
library(rmarkdown)
library(caret)
library(ggplot2)

#import data set
df <- read.csv("pml-training.csv")

#clean data frame
cleandf <- na.omit(df)

#Create training and testing sets
inTrain <- createDataPartition(y=cleandf$classe, p=0.7, list=FALSE)
Training <- cleandf[inTrain,]
Testing <- cleandf[-inTrain,]

