

#rm(list=ls())
library('reshape2')
library('mlbench')
library('e1071')


rm(list=ls())
dataset = read.csv('breast_cancer.csv')
dataset=dataset[,2:11]

dataset$Class = as.numeric(factor(dataset$Class,
                       levels = c('benign', 'malignant'),
                       labels = c(0, 1)))
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
#training_set[-10] = scale(training_set[-10])
#test_set[-10] = scale(test_set[-10])

# Fitting ANN to the Training set
#install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1) # to optimize number of course
model = h2o.deeplearning(y = 'Class',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(3,3),
                         epochs = 100,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-10]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)

# h2o.shutdown()