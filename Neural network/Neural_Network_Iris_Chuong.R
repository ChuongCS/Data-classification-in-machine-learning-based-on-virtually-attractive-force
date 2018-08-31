# This file is created by Chuong Van Nguyen, GIST, South Korea

# Install packages
#install.packages('neuralnet')
#install.packages('ggplot2')
#install.packages('nnet')
#install.packages('dplyr')
#install.packages('reshape2')
#install.packages('reshape')
#install.packages("magrittr")

# import library
library(magrittr)
library(neuralnet)
library(ggplot2)
library(nnet)
library(dplyr)
library(reshape2)
library(reshape)
library(caTools)

num_run = 20

# read data
dataset = read.csv('iris_raw_data.csv')
#set.seed(123) 
# Spliting the dataset into training set and test set
#install.packages('caTools')
for (k in 1:num_run){
  
split=sample.split(dataset$Species, SplitRatio = 0.75)
training_set=subset(dataset, split ==TRUE)
test_set=subset(dataset, split ==FALSE)

# plot data
exploratory_iris <- melt(dataset)
exploratory_iris %>%
  ggplot(aes(x = factor(variable), y = value)) +
  geom_violin() +
  geom_jitter(height = 0, width = 0.1, aes(colour = Species), alpha = 0.7) +
  theme_minimal()

# Convert your observation class and Species into one hot vector.
labels_training_set <- class.ind(as.factor(training_set$Species))
labels_test_set <- class.ind(as.factor(test_set$Species))

# Standardize column vector
standardiser <- function(x){(x-min(x))/(max(x)-min(x))}

# Standardize the predictors by using lapply
training_set[, 1:4] <- lapply(training_set[, 1:4], standardiser)
test_set[, 1:4] <- lapply(test_set[, 1:4], standardiser)

# Combine your one hot labels and standardized predictors.
pre_process_training_set <- cbind(training_set[,1:4], labels_training_set)
pre_process_test_set <- cbind(test_set[,1:4], labels_test_set)

# Define your formula that your neuralnet will be run on. You’ll need to use the as.formula function here.
f <- as.formula("setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")

# Create a neural network with two hidden layer of size 14, 10 and 5
iris_net <- neuralnet(f, data = pre_process_training_set, hidden = c(14, 10,5), act.fct = "tanh", linear.output = FALSE)

# Plot  neural network.
plot(iris_net)

# predict with test_set
iris_preds <- neuralnet::compute(iris_net, pre_process_test_set[, 1:4]) #

# compute model accuracy
origi_vals <- max.col(pre_process_test_set[, 5:7])
pr.nn_2 <- max.col(iris_preds$net.result)

#MA=round(mean(pr.nn_2==origi_vals)*100, 2)
#print(paste("Model Accuracy: ", MA, "%.", sep = ""))


if (k==1){
  MA=round(mean(pr.nn_2==origi_vals)*100, 2)
} else {
  MA=MA + round(mean(pr.nn_2==origi_vals)*100, 2)
}

print(paste("Model Accuracy: ", MA, "%.", sep = ""))

}

MA=MA/num_run

print(paste("Model Accuracy: ", MA, "%.", sep = ""))

