# Created by Nguyen Van Chuong -GIST -Korea

#rm(list=ls())
library('reshape2')
library('mlbench')
library('e1071')


data(BreastCancer)
dataset=BreastCancer[2:11]
dataset=dataset[-c(24,41,140,146,159,165,236,250,276,293,295,298,316,322,412,618),]


dataset$Class = factor(dataset$Class,
                         levels = c('benign', 'malignant'),
                         labels = c(0, 1))
# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
no_run=200
#set.seed(123) 
for (k in 1:no_run){
  
  split=sample.split(dataset$Class, SplitRatio = 0.75)
  training_set=subset(dataset, split ==TRUE)
  test_set=subset(dataset, split ==FALSE)
  
  # Feature Scaling
  #training_set[-5] = scale(training_set[-5])
  #test_set[-5] = scale(test_set[-5])
  
  # Fitting classifier to the training set
  #install.packages(e1071)
  #library(e1071)
  classifier =svm(formula = Class ~ .,
                  data= training_set,
                  type= 'C-classification',
                  kernel='radial')
  
  
  # predicting the test set results

  y_pred = predict(classifier, newdata = test_set[-10])
  
  
  if (k==1){
    cm=table(test_set[, 10], y_pred)
  } 
  else 
  {
    cm=cm + table(test_set[, 10], y_pred)
  }
  
}

cm=cm/no_run

