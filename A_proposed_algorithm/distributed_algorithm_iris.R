# This file is created by Chuong Van Nguyen, GIST, South Korea

# Importing the dataset
rm(list=ls())
dataset = read.csv('iris_raw_data.csv')

# set parameters

library('reshape2')
iter = 100
delt = 0.05
no_run = 100
sig_scale = 0.01
alpha=1
beta=1
a=3

# encoding the label feature

dataset$Species = factor(dataset$Species,
                         levels = c('setosa', 'versicolor', 'virginica'),
                         labels = c(1, 2, 3))

# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
for (k in 1:no_run){
# set.seed(123) 
split=sample.split(dataset$Species, SplitRatio = 0.75)
training_set=subset(dataset, split ==TRUE)
test_set=subset(dataset, split ==FALSE)

# Seperate data and label
training_set_data=training_set[,-ncol(dataset)]
test_set_data=test_set[,-ncol(dataset)]
training_set_label=training_set[,ncol(dataset)]
test_set_label=test_set[,ncol(dataset)]

# Feature Scaling
#training_set_data = scale(training_set_data)
#test_set_data = scale(test_set_data)

# perform distributed algorithm

#lamda=1/(sig_scale*(max(training_set_data)-min(training_set_data)))
lamda=10
estimate_label= matrix("values",1, length(test_set_label))

for(i in 1 : nrow(test_set_data)){
  
  x = as.numeric(test_set_data[i,])
  
  for (j in 1:iter){
    
    dif=as.matrix(training_set_data)-matrix(1,nrow(training_set_data),1)%*%as.numeric(x)
    
    # exponential
    #v = exp(-sqrt(rowSums(dif^2))*lamda) %*% dif
    
    # exponential alpha
    #v = exp((-(sqrt(rowSums(dif^2)))^alpha)*lamda) %*% (dif^beta)
    
    # exponential alpha
    v = 3^(((-(sqrt(rowSums(dif^2)))^alpha)*lamda)) %*% (dif^beta)
    
    # exponential polynomial
    #v = exp((-(sqrt(rowSums(dif^2)))^alpha)*lamda) %*% (dif^2)
    
    x = x+v*delt
  }
  
  vec = rowSums(dif^2)
  ind <- which(vec==min(vec))
  
  estimate_label[i] <- as.character(training_set_label[ind[1]])
  #d_ts_new[i,] <- x
}

if (k==1){
  cm=table(test_set_label,estimate_label)
} else {
  cm=cm + table(test_set_label,estimate_label)
}

}

cm=cm/no_run
