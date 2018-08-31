# This file is created by Chuong Van Nguyen, GIST, South Korea

# Importing the dataset
dataset = read.csv('iris_raw_data.csv')

# encoding the label feature

dataset$Species = factor(dataset$Species,
                         levels = c('setosa', 'versicolor', 'virginica'),
                         labels = c(1, 2, 3))



# Spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
no_run=100
#set.seed(123) 
for (k in 1:no_run){
  
split=sample.split(dataset$Species, SplitRatio = 0.75)
training_set=subset(dataset, split ==TRUE)
test_set=subset(dataset, split ==FALSE)

# Feature Scaling
#training_set[-5] = scale(training_set[-5])
#test_set[-5] = scale(test_set[-5])

# Fitting classifier to the training set
#install.packages(e1071)
library(e1071)
classifier =svm(formula = Species ~ .,
                data= training_set,
                type= 'C-classification',
                kernel='radial')


# predicting the test set results
y_pred = predict(classifier, newdata = test_set[-5])


    if (k==1){
        cm=table(test_set[, 5], y_pred)
             } 
    else 
            {
       cm=cm + table(test_set[, 5], y_pred)
            }

}

cm=cm/no_run

