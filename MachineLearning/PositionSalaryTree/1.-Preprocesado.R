# Data Preprocessing Template

library(tidyverse)

# Importing the dataset
dataset = read.csv("Datasets/Position_Salaries.csv")
dataset <- dataset %>% select(-Position)
View(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

