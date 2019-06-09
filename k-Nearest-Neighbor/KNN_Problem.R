#use the caravan data set which comes with ISLR package.
#install the ISLR package using install.packages('ISLR') function.
#load the ISLR library using the library(ISLR) function.
# install.packages('ISLR')
library(ISLR)
# print(str(Caravan))
#observation: this data set is ideal for KNN. Customer data based on 86 features out of those 85 are numerical demographic based
#predictor features and the last feature is the categorical target feature that we need to predict for new unseen customers.
#predict yes or no weather a customer will buy insuarance from the caravan company.
# print(summary(Caravan$Purchase))
#observation: only around 6% of the people purchased the insuarance.


#clean the data
#drop the rows with NA value(s) in them.
# print(anyNA(Caravan))
#observation: there are no NA values

#we want to standardize the scales of the features in the data set. Since KNN depends on computing the
#distance between the observations, we do not want any feature with a higher scale of magnitude than the others affect the computation of distance measure
#more than the ones which have lower scale of magnitude. We want all the features to be in the same scale of magnitude.

#to check the scale of a feature, we can see the variance for the values of that feature.
#lets check the variance for the values of the features to see the difference in scales among them.
#use the var() function and pass the column whose variance for it's values we want to see.
# print(var(Caravan[,1]))
# print(var(Caravan[,2]))
#there is huge difference in the scales of the columns/features.

#standardize all the predictor variables
#store the target variable separately as a vector which is the last column in the data set.
#The target column is provided separately for train data set while building the KNN model and perdicting using the built KNN model.
purchase <- Caravan[,86]
# print(head(purchase))

#standardize all the predictor variables using the scale() function which centers and scales numeric data sets.
#dataframe[,-column.index] selects all the rows with all the columns except the column.index mentioned after the '-' sign. 
standardized.Caravan <- scale(Caravan[,-86])

#check the scales of the first and second columns again.
# print(var(standardized.Caravan[,1]))
# print(var(standardized.Caravan[,2]))

#observation: predictor features of the caravan column are in the same scale.

#train and test split on our data
#instead of using caTools this time we are going to do a simple split for this simple problem for KNN.

#test data set
#first 1000 rows for test data
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

#train data set
#get all the rows except the rows used for test data set
#dataframe[-row.index,] selects all the rows except the row.index(s) mentioned after the '-' sign with all the columns. 
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

#build the model
#load the class library using library(class), this library contains the KNN function.
library(class)
#set the seed to get the same random variance while building KNN, so that Jose and I get the same results.
set.seed(101)

#use the knn() from the class package, the first argument is the training data set without the target variable column, the second argument is the test data set without the target variable column
#the third argument is a vector containing the values of the target feature for the training data set. We are predicting the values of the target feature for the test data set and storing them in a vector.
#the fourth argument is the value for K.
#we just provide the training data set for building the model as the training part of KNN just involves sorting of the
#training data set based off the distance measures computed for all the test data points.
#and then the predictions are made based on the training data labels and the k value provided.
#that is why the knn takes different arguments than the previous seen ML algo's and their formulas.
# predicted.purchase <- knn(train.data, test.data, train.purchase, k = 5)
# print(head(predicted.purchase))

#evaluate the model by computing the misclassification rate
#we can use the table(test.purchase, predicted.purchase) function as well to compute the confusion matrix for each k value used.
# for K = 1
# missclass.error <- mean(test.purchase != predicted.purchase)
# print(missclass.error)
#the evaluation of the error rate(how good or bad it is) depends on what context is the model going to be used in(how much of a error rate you can tolerate).
#this error rate is 11.6%

#for k = 3
# print(missclass.error)
#it becomes better with 7.4%

#for k = 5
# print(missclass.error)
#it becomes more better with 6.6%

#important step while working with KNN is choosing the K value.(use elbow method)
#either we can manually set different K values and choose the one which retuns the minimum missclassification error.
#or we can choose by running a for loop.(this is called the elbow method for choosing the k value)

#create a vector for holding vectors of predicted values of target variable of test data set and a vector for corresponding missclassification
#error rates.
#set both of them to NUll

store.predicted.purchase <- NULL
error.rate <- NULL

#run a for loop 20 times for building the knn model and making predictions for the test data set and storing
#the predictions made and computed error rate for us to choose the predictions made with the lowest corresponding
#error rate. We do this process for k values from 1 to 20.

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
  store.predicted.purchase[i] <- predicted.purchase  
  error.rate[i] <- mean(test.purchase != predicted.purchase)
  confusion.matrix <- table(test.purchase, predicted.purchase)
  # print('-----------------confusion matrix--------------------')
  # print(i)
  # print(confusion.matrix)
}

# print(error.rate)
#observation: the error rate gets stagnant at 5.9% and the lowest it went was 5.8%, there fore we can use the predictions made corresponding to that
# missclassification error

#visualize the K Elbow method
library(ggplot2)
#plot the missclass error we computed for k values from 1 to 20
#create a dataframe with one column for k values(create a nuerical vector from 1 to 20) and the other column has the corresponding error rates(use the already computed error.rate).
k.values <- 1:20
error.df <- data.frame(k.values, error.rate)
# print(error.df)

#do a scatterplot for the computed error rates for the corresponding k values
#to add a connecting line in a scatterplot use the geom_line() function in the geometry layer, 
#also pass the lty argument setted to dotted and color as red to get a red dotted connecting line.
pl <- ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point() + geom_line(lty = 'dotted', color = 'red')
# print(pl)

#observation: as the k value increases the error rate decreases.
#but after around k = 9 the error rate gets stationary and does not drop further, we can observe a flat line.
#that is where the elbow is, and we will choose k = 9 as the optimal k value after seeing what we have done for the training set(how much data cleaning we have done).




