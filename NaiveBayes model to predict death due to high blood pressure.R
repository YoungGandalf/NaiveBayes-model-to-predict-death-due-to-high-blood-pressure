#Adam Hereth
#Using NaiveBayes model to predict death due to high blood pressure

library(datasets)
library(tidyverse)
library(e1071)
library(readxl)

setwd("C:/Users/Adam/Desktop/Spring 2021/Data Science CMSC 462/files")
dataH = read_excel("heartFailure.xlsx")
summary(dataH)

#testing .75  #training .25
sample_size <- floor(0.75 * nrow(dataH))

## set the seed to make partition reproducible
set.seed(69)
train_ind <- sample(seq_len(nrow(dataH)), size = sample_size)

train <- dataH[train_ind, ]
test <- dataH[-train_ind, ]

#train model on death events
NVmodel <- naiveBayes(DEATH_EVENT ~ ., data = train)
preds <- predict(NVmodel, newdata = test)
conf_matrix <- table(test$high_blood_pressure, test$DEATH_EVENT)

conf_matrix

#Examine NVModel
NVmodel

NVmodel$tables

##run to see correlation between high blood pressure and death
NVmodel$apriori
summary(train$DEATH_EVENT)
train$high_blood_pressure[train$DEATH_EVENT == 1] %>% sd()
train[train$DEATH_EVENT == 1,]$high_blood_pressure %>% summary()