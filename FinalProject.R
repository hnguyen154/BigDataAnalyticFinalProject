rm(list = ls())
setwd("/Users/haonguyen/Documents/GitHub/BigDataAnalyticFinalProject")

# install.packages("kknn")
library(kknn)
# install.packages("class")
library(class)
library(e1071)  
library(lattice)
library(ggplot2)
library(caret)
# install.packages("DMwR")
library(DMwR)    # For KNN
# install.packages("ggvis")
library(ggvis)

#IMPORT DATA - Original dataset has splitted by 70% training and 30% testing
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

#To understand what is each column associated with what.
features<-read.table("./UCI HAR Dataset/features.txt")

#DATA EXPLORE
## Check null value
sum(is.na(X_train)) 
sum(is.na(y_train)) 
sum(is.na(X_test)) 
sum(is.na(y_test)) 

dim(X_train)
dim(X_test)
length(y_train)

## Data Visualization




#---------------------------------KNN---------------------------------
class_train = y_train[,1]
class_test = y_test[,1]
## A 5-nearest neighbors model with no normalization
knn5_pred <- knn(train = X_train, test = X_test, cl = class_train, k=5)
NROW(knn5_pred) # compare size with y_test

##Evaluation KNN_5
table(knn5_pred , class_test)
confusionMatrix(table(knn5_pred , class_test))


## A 10-nearest neighbors model with no normalization
knn10_pred <- knn(train = X_train, test = X_test, cl = class_train, k=10)
NROW(knn10_pred) # compare size with y_test

##Evaluation KNN_10
table(knn10_pred , class_test)
confusionMatrix(table(knn10_pred , class_test))

## A 25-nearest neighbors model with no normalization
knn25_pred <- knn(train = X_train, test = X_test, cl = class_train, k=25)
NROW(knn25_pred) # compare size with y_test

##Evaluation KNN_25
table(knn25_pred , class_test)
confusionMatrix(table(knn25_pred , class_test))






