#Setting the working Directory
setwd("C:/Users/Bhaskar/Documents/all")
#Reading the file from PC
titanic.train<-read.csv("train.csv",stringsAsFactors=FALSE,header=TRUE)
titanic.test<-read.csv("test.csv",stringsAsFactors=FALSE,header=TRUE)
str(titanic.train)
str(titanic.test)
#Multiple Imputation to fill in the missing values
install.packages("mice")
library(mice)
#Subset our data frame
simple<-titanic.train[c("Embarked","Age","Fare","Sex","Pclass","Parch","SibSp")]
summary(simple)
set.seed(123)
#Imputation Step
imputed<-complete(mice(simple))
summary(imputed)
titanic.train$Embarked<-imputed$Embarked
titanic.train$Age<-imputed$Age
titanic.train$Fare<-imputed$Fare
#Cross Validation
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
#First define how many folds we want
numFolds<-trainControl(method="CV",number=10)
#Now get the possible Value of Complexity Parameter
cpgrid<-expand.grid(.cp=seq(0.01,0.5,0.01))
#Now we perforn cross validation using the train function
train(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=titanic.train,method="rpart",trControl=numFolds,tuneGrid=cpgrid)
#We get the cp value of 0.01
#Imputation for test set
simple1<-titanic.test[c("Embarked","Age","Sex","Fare","SibSp","Parch","Pclass")]
summary(simple1)
imputed1<-complete(mice(simple1))
summary(imputed1)
titanic.test$Age<-imputed1$Age
titanic.test$Fare<-imputed1$Fare
#Categorical Casting
titanic.train$Pclass<-as.factor(titanic.train$Pclass)
titanic.train$Sex<-as.factor(titanic.train$Sex)
titanic.train$Embarked<-as.factor(titanic.train$Embarked)
titanic.train$Survived<-as.factor(titanic.train$Survived)
titanic.test$Pclass<-as.factor(titanic.test$Pclass)
titanic.test$Sex<-as.factor(titanic.test$Sex)
titanic.test$Embarked<-as.factor(titanic.test$Embarked)
#Build the model
titanic.model<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=titanic.train,method="class",cp=0.01)
#Predictor
Survived<-predict(titanic.model,newdata=titanic.test,type="class")
PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived
write.csv(output.df,file="Kaggle_Submission.csv",row.names=FALSE)
