# Importing the dataset
rm(list = ls())
dataset=read.csv('heart.data.csv')
#Displaying the count of null values per column

colSums(is.na(dataset))

# Missing data
#na. rm = TRUE to exclude missing values
dataset$biking[is.na(dataset$biking)]<-mean(dataset$biking, na.rm = TRUE)
dataset$heart.disease[is.na(dataset$heart.disease)]<-mean(dataset$heart.disease, na.rm = TRUE)
dataset$smoking[is.na(dataset$smoking)]<-mean(dataset$smoking, na.rm = TRUE)
colSums(is.na(dataset))
#Create multiple copies of the dataset with no missing data
dataset1<-dataset
dataset2<-dataset
dataset3<-dataset
dataset4<-dataset

##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
library(caret)
split=sample.split(dataset1$heart.disease, SplitRatio=.8)
training_set=subset(dataset1, split==TRUE)
testing_set=subset(dataset1,split==FALSE)
# Fitting Multiple Linear Regression to the Training set
Regressor_MLR<-lm(formula=heart.disease~.,data=training_set)


# Predicting the Validation set results
y_pred=predict(Regressor_MLR,newdata=testing_set)
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease,y_pred)

new <- data.frame(biking=45.0972026577219,smoking=21.3856201266171 )
predict(Regressor_MLR, newdata = new)
new <- data.frame(biking=8.27974338829517,smoking=6.42371952545363 )
predict(Regressor_MLR, newdata = new)
new <- data.frame(biking=42.3458634307608,smoking=20.7413275417639 )
predict(Regressor_MLR, newdata = new)
new <- data.frame(biking=30.7742540906183,smoking=23.6101749747759 )
predict(Regressor_MLR, newdata = new)


########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset2$heart.disease, SplitRatio=.8)
training_set=subset(dataset2, split==TRUE)
testing_set=subset(dataset2,split==FALSE)
# Fitting SVR to the dataset
library(e1071)

# Predicting the Validation set results
regressor_SVR=svm(formula=heart.disease~.,data=training_set, 
                  type='eps-regression',
                  kernal='radian')
y_pred=predict(regressor_SVR,newdata=testing_set)
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease,y_pred)

#new <- data.frame( )
new <- data.frame(biking=45.0972026577219,smoking=21.3856201266171 )
predict(regressor_SVR, newdata = new)
new <- data.frame(biking=8.27974338829517,smoking=6.42371952545363 )
predict(regressor_SVR, newdata = new)
new <- data.frame(biking=42.3458634307608,smoking=20.7413275417639 )
predict(regressor_SVR, newdata = new)
new <- data.frame(biking=30.7742540906183,smoking=23.6101749747759 )
predict(regressor_SVR, newdata = new)


########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset3$heart.disease, SplitRatio=.8)
training_set=subset(dataset3, split==TRUE)
testing_set=subset(dataset3,split==FALSE)
# Fitting to the dataset
library(rpart)
regressor_DT=rpart(formula=heart.disease~.,data=training_set)
                   #control=rpart.control(minsplit=1))

# Predicting the Validation set results
y_pred=predict(regressor_DT,newdata=testing_set)
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease,y_pred)
#new <- data.frame( )
new <- data.frame(biking=45.0972026577219,smoking=21.3856201266171 )
predict(regressor_DT, newdata = new)
new <- data.frame(biking=8.27974338829517,smoking=6.42371952545363 )
predict(regressor_DT, newdata = new)
new <- data.frame(biking=42.3458634307608,smoking=20.7413275417639 )
predict(regressor_DT, newdata = new)
new <- data.frame(biking=30.7742540906183,smoking=23.6101749747759 )
predict(regressor_DT, newdata = new)


########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset4$heart.disease, SplitRatio=.8)
training_set=subset(dataset4, split==TRUE)
testing_set=subset(dataset4,split==FALSE)
# Fitting to the dataset
library(randomForest)
set.seed(1234)
regressor_RF=randomForest(x=training_set[,1:2],
                          y=training_set$heart.disease,
                          ntree=20)
# Predicting the Validation set results
y_pred=predict(regressor_RF,newdata=testing_set)
RMSE(testing_set$heart.disease, y_pred)
R2(testing_set$heart.disease,y_pred)

#new <- data.frame( )
new <- data.frame(biking=45.0972026577219,smoking=21.3856201266171 )
predict(regressor_RF, newdata = new)
new <- data.frame(biking=8.27974338829517,smoking=6.42371952545363 )
predict(regressor_RF, newdata = new)
new <- data.frame(biking=42.3458634307608,smoking=20.7413275417639 )
predict(regressor_RF, newdata = new)
new <- data.frame(biking=30.7742540906183,smoking=23.6101749747759 )
predict(regressor_RF, newdata = new)

#####################################################

