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
library(dplyr)
library(randomForest)
# install.packages("devtools")
library(devtools)
#devtools::install_github('araastat/reprtree')
library(reprtree)

##---------------------------------IMPORT DATA---------------------------------
#Original dataset has splitted by 70% training and 30% testing
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

y_train[,'V1'] = as.factor(y_train[,'V1'])
y_test[,'V1'] = as.factor(y_test[,'V1'])

#To understand what is each column associated with what.
features<-read.table("./UCI HAR Dataset/features.txt")

##---------------------------------DATA EXPLORE---------------------------------
## Check null value
sum(is.na(X_train)) 
sum(is.na(y_train)) 
sum(is.na(X_test)) 
sum(is.na(y_test)) 

#dim(X_train)
#dim(X_test)
#length(y_train)

#----------------------------------Data Visualization----------------------------



#----------------------------------Without Ensembles------------------------------------
##---------------------------------KNN---------------------------------
class_train = y_train[,1]
class_test = y_test[,1]

#------1st train---------
## A 5-nearest neighbors model with no normalization
knn5_pred <- knn(train = X_train, test = X_test, cl = class_train, k=5)
#NROW(knn5_pred) # compare size with y_test
##Evaluation KNN_5
table(knn5_pred , class_test)
confusionMatrix(table(knn5_pred , class_test))

#------2nd train---------
## A 10-nearest neighbors model with no normalization
knn10_pred <- knn(train = X_train, test = X_test, cl = class_train, k=10)
##Evaluation KNN_10
table(knn10_pred , class_test)
confusionMatrix(table(knn10_pred , class_test))

#------3rd train---------
## A 25-nearest neighbors model with no normalization
knn25_pred <- knn(train = X_train, test = X_test, cl = class_train, k=25)
##Evaluation KNN_25
table(knn25_pred , class_test)
confusionMatrix(table(knn25_pred , class_test))

# *** Not such different in change in K-neighbor 5/10/25
# *** Class 3 and 4 did not have a google accuracy. Best classes are 1 and 6.
#Accuracy ~ 90%

##---------------------------------Random Forest---------------------------------
#------1st train---------
data.rf=randomForest(class_train ~ ., data=X_train, ntree=100, mtry=2, importance=TRUE)
data.rf #Confustion matrix
varImpPlot(data.rf)
## Mean Decrease in Accuracy - The decrease in accuracy as a result of this permuting is averaged over all trees, and is used as a measure of the importance of variable j in the random forest. 
## Mean Decrease in Gini -  calculates each feature importance as the sum over the number of splits (across all tress) that include the feature, proportionally to the number of samples it splits.

#Predicting using random forest model
X_test$pred_rf<-predict(object = data.rf,X_test)
X_test$pred_rf.prob<-predict(object = data.rf,X_test,type="prob")

#Checking the accuracy of the random forest model
confusionMatrix(class_test,X_test$pred_rf)
#Accuracy ~ 92%

#Print Tree
#reprtree:::plot.getTree(data.rf)
#tree <- getTree(data.rf, k=1, labelVar=TRUE)
#realtree <- reprtree:::as.tree(tree, data.rf)

#------2nd train---------
#Train 200 Trees and do.trace=100
data.rf2=randomForest(class_train ~ ., data=X_train, ntree=200, mtry=2,do.trace=100, importance=TRUE)
data.rf2 #Confustion matrix
varImpPlot(data.rf2)

#Predicting using random forest model
X_test$pred_rf2<-predict(object = data.rf2,X_test)
X_test$pred_rf.prob2<-predict(object = data.rf2,X_test,type="prob")

#Checking the accuracy of the random forest model
confusionMatrix(class_test,X_test$pred_rf2)

#Accuracy ~ 92%

##---------------------------------------SVM-------------------------------------
# Fitting model
fit <-svm(class_train ~ ., data = X_train)
summary(fit)
#Predict Output 
predicted= predict(fit,X_test)
confusionMatrix(class_test,predicted)

#Accuracy ~ 95%

##--------------------------------Neutral Network--------------------------------



#----------------------------------With Ensembles--------------------------------
