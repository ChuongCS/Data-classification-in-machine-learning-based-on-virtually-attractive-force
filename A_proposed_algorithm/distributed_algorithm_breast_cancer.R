
rm(list=ls())
dataset = read.csv('breast_cancer.csv')
dataset =dataset[,2:11]

library('reshape2')
library('mlbench')
library('e1071')
iter = 100
delt = 0.05
no_run = 100
sig_scale = 0.01
alpha=10
beta=1



#data(BreastCancer)
#dataset=BreastCancer[2:11]
#dataset=dataset[-c(24,41,140,146,159,165,236,250,276,293,295,298,316,322,412,618),]
#write.csv(dataset, file ='breast_cancer.csv')

#dataset$Bare.nuclei=ifelse(is.na(dataset$Bare.nuclei),
#ave(dataset$Bare.nuclei, FUN=function(x) mean(x, na.rm = TRUE)),
#dataset$Bare.nuclei)


dataset$Class = factor(dataset$Class,
                       levels = c('benign', 'malignant'),
                       labels = c(0, 1))

# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
for (k in 1:no_run){
  # set.seed(123) 
  split=sample.split(dataset$Class, SplitRatio = 0.75)
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
  lamda=100
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
      v = 10^(((-(sqrt(rowSums(dif^2)))^alpha)*lamda)) %*% (dif^beta)
      
      
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
