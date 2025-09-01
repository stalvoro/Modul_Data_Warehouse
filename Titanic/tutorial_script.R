#Set working directory and import datafiles
setwd("C:/Users/avoro/Desktop/csv.-Dateien-20240515/Titanic")
train <- read.csv("C:/Users/avoro/Desktop/csv.-Dateien-20240515/Titanic/train.csv")
test <- read.csv("C:/Users/avoro/Desktop/csv.-Dateien-20240515/Titanic/test.csv")
# view some data
str(train)
table(train$Survived)
prop.table(table(train$Survived))
#write all as Survived null in test.csv
test$Survived <- rep(0, 418)
#give average of male and female survivors
prop.table(table(train$Sex, train$Survived),1)
#write all passengers as dead
test$Survived <- 0
#write all female passengers as alive
test$survived[test$Sex == 'female'] <- 1
#create attribute Child
train$Child <- 0
#assign child to all bellow the age of 18
train$Child[train$Age < 18] <-1
#show percentage of survivors of male female and if they were a child or not
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
#setup function to categorize survivors based of their fare ticket
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
#show percentage of female and male survivors based on the paid fare ticket
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN = function(x){sum(x)/length(x)})



#include library for decision trees
library(rpart)
#install some packages for meaningful visualization
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#use fit combining all metrics into one tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)