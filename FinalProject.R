rm(list = ls())
setwd("/Users/haonguyen/Documents/GitHub/BigDataAnalyticFinalProject")

# install.packages("class")
library(class)
# install.packages("e1071")
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
# install.packages("mlbench")
library(mlbench)
# install.packages("caret")
library(caret)
# install.packages("caretEnsemble")
library(caretEnsemble)
library(tidyverse)
library(h2o)


##---------------------------------IMPORT DATA---------------------------------
#Original dataset has splitted by 70% training and 30% testing

X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

#To understand what is each column associated with what.
features<-read.table("./UCI HAR Dataset/features.txt")

#----------------------------------Data Visualization----------------------------

## PIE CHART TO SHOW DISTRIBUTION OF ALL SIX ACTIVITIES IN TRAINING RESULTS

df_activity <- y_train

df_activity <- df_activity %>%
  mutate(Activity = case_when(V1 == 1 ~ 'WALKING',
                              V1 == 2 ~ 'WALKING_UPSTAIRS',
                              V1 == 3 ~ 'WALKING_DOWNSTAIRS',
                              V1 == 4 ~ 'SITTING',
                              V1 == 5 ~ 'STANDING',
                              V1 == 6 ~ 'LAYING'))


theme_set(theme_classic())

pie <- ggplot(df_activity, aes(x = "", fill = factor(Activity))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Activity", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Activity - TRAINING", 
       caption="Source: y_train")

pie + coord_polar(theta = "y", start=0)

#***********************************************************************
# To check tBodyAcc-max()-XYZ values are between -1 and +1
#***********************************************************************

df_filtered_features <- features %>%
  mutate(Col_Names = paste("V",features$V1, sep = "")) %>%
  filter(str_detect(V2, pattern = "max()")) %>%
  select(Col_Names, V2)

X_train_max <- X_train %>%
  select(c(df_filtered_features$Col_Names))

ggplot(X_train_max, aes(x=(1:7352))) + 
  geom_point(mapping = aes(y = X_train_max$V10), color = 'blue') +
  geom_point(mapping = aes(y = X_train_max$V11), color = 'black') +
  geom_point(mapping = aes(y = X_train_max$V12), color = 'red') +
  labs(x = "Observations",y="Values" , title="tBodyAcc-max()-XYZ value plot")

#***********************************************************************
# To check all angle() values are between -1 and +1
#***********************************************************************
df_filtered_features_2 <- features %>%
  mutate(Col_Names = paste("V",features$V1, sep = "")) %>%
  filter(str_detect(V2, pattern = "angle()")) %>%
  select(Col_Names, V2)

X_train_angle <- X_train %>%
  select(c(df_filtered_features_2$Col_Names))

ggplot(X_train_angle, aes(x=(1:7352))) + 
  geom_point(mapping = aes(y = X_train_angle$V555), color = 'blue') +
  geom_point(mapping = aes(y = X_train_angle$V556), color = 'black') +
  geom_point(mapping = aes(y = X_train_angle$V557), color = 'red') +
  geom_point(mapping = aes(y = X_train_angle$V558), color = 'green') +
  geom_point(mapping = aes(y = X_train_angle$V559), color = 'magenta') +
  geom_point(mapping = aes(y = X_train_angle$V560), color = 'yellow') +
  labs(x = "Observations", y="Values" , title="angle() values plot")

<<<<<<< HEAD

##------------------------------DATA PREPROCESSING-----------------------------
class_train = as.factor(y_train[,1])
class_test = as.factor(y_test[,1])

#Change column name in label of training and testing
y.train <- as_tibble(y_train)
y.train = y.train %>% 
  rename(
    y = V1
  )
dim(y_train)

y.test <- as_tibble(y_test)
y.test = y.test %>% 
  rename(
    y = V1
  )

dim(y.test)

#Combine
train.df = data.frame(X_train, y.train)
valid.df = data.frame(X_test, y.test)


## Check null value
sum(is.na(train.df)) 
sum(is.na(valid.df)) 


#Check redundant data
res_train <- cor(X_train)
res_test <- cor(X_test)
=======
## PIE CHART TO SHOW DISTRIBUTION OF ALL SIX ACTIVITIES IN TRAINING RESULTS

df_activity <- y_train

df_activity <- df_activity %>%
  mutate(Activity = case_when(V1 == 1 ~ 'WALKING',
                              V1 == 2 ~ 'WALKING_UPSTAIRS',
                              V1 == 3 ~ 'WALKING_DOWNSTAIRS',
                              V1 == 4 ~ 'SITTING',
                              V1 == 5 ~ 'STANDING',
                              V1 == 6 ~ 'LAYING'))


theme_set(theme_classic())

pie <- ggplot(df_activity, aes(x = "", fill = factor(Activity))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Activity", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Activity - TRAINING", 
       caption="Source: y_train")

pie + coord_polar(theta = "y", start=0)

#***********************************************************************
# To check tBodyAcc-max()-XYZ values are between -1 and +1
#***********************************************************************

df_filtered_features <- features %>%
                        mutate(Col_Names = paste("V",features$V1, sep = "")) %>%
                        filter(str_detect(V2, pattern = "max()")) %>%
                        select(Col_Names, V2)
  
X_train_max <- X_train %>%
  select(c(df_filtered_features$Col_Names))

ggplot(X_train_max, aes(x=(1:7352))) + 
  geom_point(mapping = aes(y = X_train_max$V10), color = 'blue') +
  geom_point(mapping = aes(y = X_train_max$V11), color = 'black') +
  geom_point(mapping = aes(y = X_train_max$V12), color = 'red') +
  labs(x = "Observations",y="Values" , title="tBodyAcc-max()-XYZ value plot")

#***********************************************************************
# To check all angle() values are between -1 and +1
#***********************************************************************
df_filtered_features_2 <- features %>%
  mutate(Col_Names = paste("V",features$V1, sep = "")) %>%
  filter(str_detect(V2, pattern = "angle()")) %>%
  select(Col_Names, V2)

X_train_angle <- X_train %>%
  select(c(df_filtered_features_2$Col_Names))

ggplot(X_train_angle, aes(x=(1:7352))) + 
  geom_point(mapping = aes(y = X_train_angle$V555), color = 'blue') +
  geom_point(mapping = aes(y = X_train_angle$V556), color = 'black') +
  geom_point(mapping = aes(y = X_train_angle$V557), color = 'red') +
  geom_point(mapping = aes(y = X_train_angle$V558), color = 'green') +
  geom_point(mapping = aes(y = X_train_angle$V559), color = 'magenta') +
  geom_point(mapping = aes(y = X_train_angle$V560), color = 'yellow') +
  labs(x = "Observations", y="Values" , title="angle() values plot")
>>>>>>> 4dcff03c2312da3510dd569f57a9bca78edd1488

#----------------------------------Without Ensembles------------------------------------
##---------------------------------KNN---------------------------------
# *****Need to try rectangular, triangular, etc.
#------1st train---------
## A 5-nearest neighbors model with no normalization
knn5_pred <- knn(train = train.df, test = valid.df, cl = train.df$y, k=5)
#NROW(knn5_pred) # compare size with y_test
##Evaluation KNN_5
table(knn5_pred , valid.df$y)
confusionMatrix(table(knn5_pred , valid.df$y))

#------2nd train---------
## A 10-nearest neighbors model with no normalization
knn10_pred <- knn(train = train.df, test = valid.df, cl = train.df$y, k=10)
##Evaluation KNN_10å
table(knn10_pred , valid.df$y)
confusionMatrix(table(knn10_pred , valid.df$y))

#------3rd train---------
## A 25-nearest neighbors model with no normalization
knn25_pred <- knn(train = train.df, test = valid.df, cl = train.df$y, k=25)
##Evaluation KNN_25
table(knn25_pred , valid.df$y)
confusionMatrix(table(knn25_pred , valid.df$y))
#------------------------
# *** Not such different in change in K-neighbor 5/10/25
# *** Class 3 and 4 did not have a google accuracy. Best classes are 1 and 6.

#Accuracy ~ 90%

##---------------------------------Random Forest---------------------------------
#------1st train---------
## DO NOT RUN (~20-30 mins)

data.rf=randomForest(class_train ~ ., data=X_train, ntree=100, mtry=2, importance=TRUE)
data.rf #Confustion matrix
varImpPlot(data.rf)
## Mean Decrease in Accuracy - The decrease in accuracy as a result of this permuting is averaged over all trees, and is used as a measure of the importance of variable j in the random forest. 
## Mean Decrease in Gini -  calculates each feature importance as the sum over the number of splits (across all tress) that include the feature, proportionally to the number of samples it splits.

#Predicting using random forest model
X_test$pred_rf<-predict(object = data.rf,X_test)

#Checking the accuracy of the random forest model
confusionMatrix(class_test,X_test$pred_rf)

#Accuracy ~ 90.4%

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
fit <-svm(y ~ ., data=train.df)
summary(fit)

# Find weights that will be applied to features in prediction
# Analogous to "regression coefficients"
weights <- t(fit$coefs) %*% fit$SV 
weights

## predict class: Training
svm_pred_train <- predict(fit, newdata = train.df[,-562])
confusionMatrix(svm_pred_train, train.df$y)

## predict class: Validation
svm_pred_valid <- predict(fit, newdata = valid.df[,-562])
confusionMatrix(svm_pred_valid, valid.df$y)

#Accuracy ~ 95%

###-------------------------Tuning-------------------------
#DO NOT RUN: (TOOK ~2 HOURS TO RUN)


## Train a best linear-kernel support vector classifier
linear.tune <- tune.svm(y ~ ., data=train.df,
                        kernel = "linear",
                        cost = c(0.001, 0.01, 0.1, 1, 5, 10)) #Cost is the price of the misclassification ( from 0.1 (10^(-1)) to 100 (10^2) in multiples of 10.)

summary(linear.tune)

best.linear <- linear.tune$best.model

linear.pred.train <- predict(best.linear, newdata = train.df)
confusionMatrix(poly.pred.train, train.df$y)

linear.pred.test <- predict(best.linear, newdata = valid.df)
confusionMatrix(poly.pred.test, valid.df$y)

plot(linear.tune)

#Accuracy ~ 96%

# Train a best polynomial-kernel support vector machine
poly.tune <- tune.svm(y ~ ., data=train.df,
                      kernel = "polynomial",
                      degree = c(3, 4, 5),
                      coef0 = c(0.1, 0.5, 1, 2, 3, 4)) 
summary(poly.tune)

best.poly <- poly.tune$best.model

poly.pred.train <- predict(best.poly, newdata = train.df)
confusionMatrix(poly.pred.train, train.df$y)

poly.pred.test <- predict(best.poly, newdata = valid.df)
confusionMatrix(poly.pred.test, valid.df$y)

#Accuracy ~ 95%

##--------------------------------Neutral Network--------------------------------
#Neural Network Using h2o Library
write_rds(X_train, "x_train.rds")
write_rds(X_test, "x_test.rds")
write_rds(y.train, "y_train.rds")
write_rds(y.test, "y_test.rds")

#Clean memory space
rm(X_train)
rm(X_test)
rm(y_train)
rm(y_test)
rm(y.train)
rm(y.test)

x_train_rds <- read_rds("x_train.rds")
x_test_rds <- read_rds("x_test.rds")
y_train_rds <- read_rds("y_train.rds")
y_test_rds <- read_rds("y_test.rds")

h2o.init(nthreads = -1)

data_h2o <- as.h2o(  
  bind_cols(y_train_rds, x_train_rds),  
  destination_frame= "train.hex"
)

new_data_h2o <- as.h2o(  
  bind_cols(y_test_rds, x_test_rds),  
  destination_frame= "test.hex"
)

splits <- h2o.splitFrame(data = data_h2o,                          
                         ratios = c(0.7, 0.15), # 70/15/15 split                         
                         seed = 1234
)

train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o  <- splits[[3]] # from training data

y <- "y"                          # column name for outcome
x <- setdiff(names(train_h2o), y) # column names for predictors

m1 <- h2o.deeplearning(  
  model_id = "dl_model_first",   
  x = x,  
  y = y,  
  training_frame = train_h2o,   
  validation_frame = valid_h2o, ## validation dataset: used for scoring and 
                                ## early stopping
  #activation="Rectifier",      ## default
  #hidden=c(200,200),           ## default: 2 hidden layers, 200 neurons each  
  epochs = 1                    ## one pass over the training data
  )

summary(m1)

test_prediction = h2o.predict(m1, newdata = as.h2o(test_h2o[1]))

h2o.performance(m1, newdata = as.h2o(test_h2o))

#SAVE MODEL
h2o.saveModel(object = m1,     # the model you want to save              
              path = getwd(),  # the folder to save              
              force = TRUE)    # whether to overwrite an existing file
model_filepath = str_c(getwd(), "dl_model_first") #dl_model_first is model_id
m1 <- h2o.loadModel(model_filepath) 

h2o.shutdown()

#----------------------------------With Ensembles--------------------------------
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
#Cross Validation = 10-folds.

seed <- 7
metric <- "Accuracy"

## Stacking Method
algorithmList <- c('svmLinear', 'svmRadial', 'lda', 'qla', 'knn')

set.seed(seed)
models <- caretList(y ~ ., data=train.df, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

#Kappa: 
#    0.01–0.20 as none to slight
#    0.21–0.40 as fair
#    0.41– 0.60 as moderate
#    0.61–0.80 as substantial
#    0.81–1.00 as almost perfect agreement.




