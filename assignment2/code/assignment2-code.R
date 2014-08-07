# Coursera Data Analysis - Assignment 2
# https://www.coursera.org/course/dataanalysis
#
# See ../writing/assignment2-analysis.pdf for more detailed documentation.

# Load librarys
library(e1071)
library(ggplot2)
library(RColorBrewer)

# Download the mobile phone user activity data
# NOTE: Data no longer available online.
#download.file(url="https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", destfile="./data/samsungData.rda", method="curl")

## Load data
load(file="../data/samsungData.rda")

# Data munging and exploratory analysis
###############################
rawdata <- samsungData
rawdata$subject <- as.integer(samsungData$subject)
rawdata$activity <- as.factor(rawdata$activity)

summary(rawdata)

# Separate training and test data
trainset <- subset(rawdata, subject == 1 | subject == 3 | subject == 5 | subject == 6)
testset <- subset(rawdata, subject == 27 | subject == 28 | subject == 29 | subject == 30)

trainset <- subset(trainset, select = -subject)
testset <- subset(testset, select = -subject)

dim(trainset)
dim(testset)

# Prediction model training and prediction
##########################################

# Using Support Vector Machine
model <- svm(activity~., data = trainset)
prediction <- predict(model, testset)
summary(model)
summary(prediction)

# Results
##########################################

# Confusion table
tab <- table(pred = prediction, true = testset$activity)
tab
classAgreement(tab)
