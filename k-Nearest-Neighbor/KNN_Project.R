#KNN
#Iris flower data set
#load the ISLR package to acess the iris data set
library(ISLR)
# print(head(iris))
#observation: since all the predictor features are numerical and the target feature is categorical, the KNN algo is a good choice.
# print(str(iris))
#observation: small data set.

#standardize the data
#normalization refers to rescaling the values of a feature between 0 and 1 using the formula -> X = (X - X.sub.min) / (X.sub.max - X.sub.min)
#standardization refers to rescaling the values of a feature such that the new mean is 0 and the standard deviation is 1. formula used is X = (X - X.sub.mean) / X.sub.std
#scale() function performs standardization on all the columns of a numeric data set.
#store the target feature column for training data set to be fed to the KNN model and for testing data set for evaluation.
#the target feature column for the training data set is fed to the KNN model as a vector only with the length same as that of the observations in the training data frame used to train the knn model.
#for the test data set it is a vector as well.
species <- iris['Species']
# print(species)
standardized.iris <- scale(iris[,-5])
#check if the scaling worked using var()
# print(var(standardized.iris[,1]))

#join the standardized iris data set with the target feature column
#use cbind() function
joined.iris <- cbind(standardized.iris, species)
# print(head(joined.iris))

#test and train splits
#to check the shape of a data frame use dim() and to check the length of a vector use length().
#use caTools
library(caTools)
set.seed(101)
sample <- sample.split(joined.iris$Species, SplitRatio = 0.7)
library(dplyr)
train <- filter(joined.iris, sample == T)
train.df <- train[,1:4]
# print(head(train.df))
# print(str(train.df))
# print(dim(train.df))
train.target <- train[,5]
# print(head(train.target))
# print(str(train.target))
# print(length(train.target))
test <- filter(joined.iris, sample == F)
test.df <- test[,1:4]
# print(head(test.df))
# print(str(test.df))
# print(dim(test.df))
test.target <- test[,5]
# print(head(test.target))
# print(str(test.target))
# print(length(test.target))

#build the KNN model, load the class package
library(class)
# predicted.species <- knn(train.df, test.df, train.target, k = 1)
#misclassification error
# missclass.error <- mean(test.target != predicted.species)
# print(predicted.species)
# print(missclass.error)
#observation: the error rate is 4.4% which is good but it is for a data set which is too small.

#choosing a k value using elbow method
#run a for loop for building knn models for k values from 1 to 10, store the corresponding missclassification errors for plotting a scatter plot
#to perform elbow method for choosing the best k value.
#first create two empty vectors for storing the vectors of predicted target feature values for test data and corresponding misclassification errors respectively. 
#assign both of them to NULL.
store.predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  #to get the same random variance while building the knn models and get the same results every time
  set.seed(101)
  predicted.species <- knn(train.df, test.df, train.target, k = i)
  # store.predicted.species[i] <- predicted.species
  error.rate[i] <- mean(test.target != predicted.species)
}

print(error.rate)

#plot the error rates as a scatter plot with a connecting line
#create a data frame with k values and corresponding error rates
k.values <- 1:10
error.rate.df <- data.frame(k.values, error.rate)
# print(error.rate.df)
#plot a scatter plot with a dotted red colored connecting line using the lty(line style argument) set as dotted and color argument set as red.
#to add a size to the line use size argument.
library(ggplot2)
pl <- ggplot(error.rate.df, aes(x = k.values, y = error.rate)) + geom_point() + geom_line(lty = 'dotted', color = 'red', size = 1) + theme_bw()
print(pl)
#observation: the plot is a little weird, the error drops from k = 1 to k = 2, the best k value is k = 2 as the error rate is 2.2%, the error rate is the lowest for k = 2 to 6,
#then it increases and then decreases again, this is because our data is pretty small and elbow method is little weird to use here.
