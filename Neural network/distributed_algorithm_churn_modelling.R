# This file is created by Chuong Van Nguyen, GIST, South Korea

# Importing the dataset
rm(list=ls())
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]


# set parameters

library('reshape2')
iter = 100
delt = 0.05
no_run = 1
sig_scale = 0.01
alpha=2
beta=5

# encoding the label feature

dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
for (k in 1:no_run){
# set.seed(123) 
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Seperate data and label
training_set_data=training_set[,-ncol(dataset)]
test_set_data=test_set[,-ncol(dataset)]
training_set_label=training_set[,ncol(dataset)]
test_set_label=test_set[,ncol(dataset)]

# Feature Scaling
#training_set_data = scale(training_set_data)
#test_set_data = scale(test_set_data)

# perform distributed algorithm

lamda=1/(sig_scale*(max(training_set_data)-min(training_set_data)))^5
estimate_label= matrix("values",1, length(test_set_label))

for(i in 1 : nrow(test_set_data)){
  
  x = as.numeric(test_set_data[i,])
  
  for (j in 1:iter){
    
    dif=as.matrix(training_set_data)-matrix(1,nrow(training_set_data),1)%*%as.numeric(x)
    
    # exponential
    #v = exp(-sqrt(rowSums(dif^2))*lamda) %*% dif
    
    # exponential alpha
    #v = exp((-(sqrt(rowSums(dif^2)))^alpha)*lamda) %*% (dif^beta)
    
    # exponential polynomial
    v = exp((-(sqrt(rowSums(dif^2)))^alpha)*lamda) %*% (dif^2)
    
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
