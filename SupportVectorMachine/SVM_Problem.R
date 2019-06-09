#use the iris data set from ISLR library
library(ISLR)
# print(head(iris))
#the e1071 package has the SVM in it.
#install the package using install.package('e1071') function
# install.packages('e1071')
#load the library e1071 using the library(e1071) function which has the SVM in it.
library(e1071)
#call the help function on SVM to know more about it.
#the linear kernel is used to draw a decision boundary which is a line.
#the other kernels are used to separate the non linearly separable data by drawing a hyper plane as a decision boundary.
#the radial basis kernel is chosen as default.
#the gamma argument is specified for all kernels except the linear kernel. Default is 1/data dimension.
#cost argument: it is cost of constraints violation (specifies how much mistakes can be tolerated by the Soft margin SVM) (default: 1)-it is the 'C'-constant of the regularization term in the Lagrange formulation.

#build the model
#use the svm() to build the svm model, the formula and data for training are provided as seen before.
# model <- svm(Species ~ ., data = iris)
# print(summary(model))
#observation: the formula used to build the model
#the parameters for type of model, kernel used, the cost and the gamma value.
#no of support vectors in total and for each class mentioned alongside the classes in the data set.


#to predict the values for the test data set use the predict() function.
#pass the fitted model as the first argument and the test data set as 'new data' argument.
#since we did not do any train test split we will predict for the train data here. Use only the first 4 predictor columns.
# pred.values <- predict(model, newdata = iris[1:4])
#confusion matrix
# confusion.matrix <- table(pred.values, iris$Species)
# print(confusion.matrix)
#observation: it performs almost perfectly on the train data as it should but makes 2 mistakes of misclassifying versicolor
#and virginica each.


#tuning our model
#cost parameter is used to define the cost function of the soft margin SVM. It allows some mistakes to be made on the
#training data while classifying them and allows some of those to placed on the wrong side of the margin. This usually leads to
#a better model which does not overfit by not learning the potential noise in the traning data and thus generalize better. The amount of 
#mistakes allowed is defined by the cost parameter. By defining this we are actually controlling which points to be the support vectors.

#for non linearly separable data we use kernel tricks to perform classification by drawing a hyper plane without 
#projecting the data into a higher dimensional space. This kernel trick makes use of a non linear kernel function 
#to do so and that involves a gamma parameter which is the free parameter of the gaussian radial basis function which itself is just a formula. 
#small gamma value means a gaussian with a large variance which means the influence of the support vectors is more.
#large gamma means the variance is small and the support vectors does not have a widespread influence.
#large gamma leads to high bias and low variance models and vice verca for the small gamma values.

#example of how to tune these gamma and cost values and let the computer choose the best cost and gamma values.
#to do so we use the tune() function
#tune() function is a generic method to tune hyperparameters of statistical methods using a grid search.
#the first argument is the svm function to specify the statistical method whose hyperparameters we are going to tune.
#the tune() will train a bunch of svm models and choose the model which has the best fit alongside it's tuned hyperparameters.
#the train.x is used to specify the training data with predictor features only.
#the train.y is used to specify the training data with the target feature only for which we are training the model to predict on test data.
#choose the kernel using the kernel argument. here we will choose the radial kernel.
#then the ranges agrument specifies the range of gamma and cost values we want to train the model with.
#the ranges argument takes a list of parameter names which have a vector of values assigned to each.
#the df[n:m] notation returns the dataframe with all the rows and columns with n to m index values.
# tune.results <- tune(svm, train.x = iris[1:4], train.y = iris[,5], ranges = list(cost = c(0.5, 1, 1.5), gamma = c(0.1, 0.5, 0.7)))
# print(summary(tune.results))
#observation: the tune() does a grid search to get the best statistical method/model that we are building for the all combination of the provided values for the parameters.
#it returns the best model with the combination of tuned parameter values which have the lowest error rate.
#in our case we got the svm model with cost and gamma parameters to be 1 and 0.1 which have the least error of 0.033 as the best fit.
#we can also work around these combination of cost and gamma parameter values to check if we can get a better model with less error rate.

#we can use the recently found tuned values of cost and gamma parameters to train a tuned SVM model for our data set.
#specify the kernel to use using kernel argument, the default is radial.
#specify the tuned cost and gamma parameter values using cost and gamma arguments.
# tuned.svm <- svm(Species ~ ., data = iris, kernel = 'radial', cost = 1, gamma = 0.1)
#check the summary
# print(summary(tuned.svm))
#observation: the formula used to build the model
#the parameters for type of model, kernel used, the cost and the gamma value.
#no of support vectors in total and for each class mentioned alongside the classes in the data set.


#first we found the tuned cost and gamma parameter values for our model using tune() function and then trained a tuned svm model for our data set
#using those tuned parameter values.
