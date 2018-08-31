# This file is created by Chuong Van Nguyen, GIST, South Korea

# Importing the dataset
#rm(list=ls())
dataset = read.csv('iris_raw_data.csv')

# encoding the label feature
dataset$Species = factor(dataset$Species,
                         levels = c('setosa', 'versicolor', 'virginica'),
                         labels = c(1, 2, 3))
# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
# set.seed(123) 
split=sample.split(dataset$Species, SplitRatio = 0.75)
training_set=subset(dataset, split ==TRUE)
test_set=subset(dataset, split ==FALSE)


# Feature Scaling
training_set[-ncol(training_set)] = scale(training_set[-ncol(training_set)])
test_set[-ncol(test_set)] = scale(test_set[-ncol(test_set)])

# Fitting ANN to the Training set
#install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1) # to optimize number of course
model = h2o.deeplearning(y = 'Species',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(3,3),
                         epochs = 10,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-ncol(test_set)]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)


# Making the Confusion Matrix
cm = table(test_set[, ncol(test_set)], y_pred)
