##Predicting Credit Card Fraudulent Transactions Using Synthetic Data Generation
rm(list=ls())

# Libraries
library(ggplot2)
library(corrplot)
library(ROSE)
library(rpart)

#  Loading dataset
data=read.csv("C:\\Users\\AJIT\\Documents\\creditcard.csv")

#  Preprocessing
sum(is.na(data))    ## No Missing data

# Exploratory Data analysis
## Know about data
str(data)

## Discriptive measures
summary(data)

# Correlation
cordata=subset(data,select=-c(Time,Class,Amount))
corre=cor(cordata)
corre
corrplot(corre, order = "FPC", method = "color",
         type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

## Distribution of Class variable 
table(data$Class)
prop.table(table(data$Class))*100
ggplot(data,aes(x=Class))+geom_bar(color="green",fill="red")
ggplot(data, aes(x = Class, y = Amount)) + geom_boxplot(color="blue") +
  ggtitle("Distribution of transaction amount by class")

# Data Spliting
size<- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size =size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

# Methods for Imbalanced Classification Problem
###below methods are sampling methods used for imbalanced dataset.

## Undersampling
## Oversampling
## Synthetic data generation
## Cost sensitive Learning

# Modelling
## Decision tree without sampling method
dt<- rpart(Class~ .,train)
pred<- predict(dt,test)
accuracy.meas(test$Class, pred)

roc.curve(test$Class, pred, plotit = T)

## Decision tree with sampling method
data.rose <- ROSE(Class~.,train, seed = 1)$data
table(data.rose$Class)
dt.rose <- rpart(Class ~ .,data.rose)
pred.tree.rose <- predict(dt.rose,test)
accuracy.meas(test$Class, pred.tree.rose)

roc.curve(test$Class, pred.tree.rose,plotit = T)

## Logistic Regression Without Sampling
glm=glm(Class~.,train,family = binomial)
pre<- predict(glm,test)
accuracy.meas(test$Class, pre)
roc.curve(test$Class, pre,plotit = T)

## Logistic Regression With Sampling
glm=glm(Class~.,data.rose,family = binomial)
pre<- predict(glm,test)
accuracy.meas(test$Class, pre)
roc.curve(test$Class, pre,plotit = T)