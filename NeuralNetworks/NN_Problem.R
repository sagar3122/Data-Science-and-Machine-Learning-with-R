#NN with R
#neural network package in R.
#use the Boston dataset in the MASS package in R
#use install.package('MASS') function to install MASS package.
# install.packages('MASS')
#load the library using library(MASS) function
library(MASS)
# print(head(Boston))
#observations: consists of features for the houses in Boston.
#the target column is the median value of owner occupied homes in thousands of dollars(medv).
#it is like the median value/cost for the homes curently owned by someone and having those feature values.
#this problem is a regression problem, where we will predict a continuous value for a given set of feature values.
# print(str(Boston))

#check for the missing data
# print(any(is.na(Boston)))
#or try
# print(anyNA(Boston))

#assign Boston data frame to data variable
data <- Boston
# print(head(data))

#train a NN model
#preprocess the data
#while training a NN model it is a good practice to normalize the data.
#if normalized data is not used for training then we might get useless results or the model might not converge training
#in the pre defined amount of iterations, thus not learning good weights for making predictions on the test samples.
#scaling/normalizing the data can be done using different statistical methods such as min-max scale or z normalization etc.

#we will use the scale() function in R to normalize our data.
#first we will grab the min and max values of each column in the data set using apply(), max() and min() functions.
#use the margin argument in apply() function to indicate whether we want the specified function to be applied to either the
#rows in the data set or the columns. Assign margin to 2 to apply on columns.
maxs <- apply(data, MARGIN = 2, max)
#this will return a vector of maximum values for each column of the data frame.
# print(maxs)
# print(class(maxs))
mins <- apply(data, MARGIN = 2, min)
#this will return a vector of minimum values for each column of the data frame.
# print(mins)

#we will use these min and max values for each columns to normalize our data using the scale() function.
#call help on the scale() function
#scale() is a generic function whose default method centers and/or scales the columns of a numeric matrix.
#you can perform both standardization and normalization of data with scale() function.
#it has default arguments for centering and scaling, these can be used either as logical values or numerical vectors.
#we are going to pass the min and max values that we have as numerical vectors to perform centering and scaling of data.
#if a numeric vector is assigned to the center argument and is the same length as columns in the data frame,
#then for each column, the values in the columns are subtracted from the corresponding value for that column in the vector assigned to the center argument.
#for the center argument we are going to have mins vector assigned, therefore for each column, 
#the values in the columns are subtracted from the corresponding min value for that column in the vector assigned to the center argument.
#if a numeric vector is assigned to the scale argument and is the same length as columns in the data frame,
#then for each column, the values in the columns are divided by the corresponding value for that column in the vector assigned to the scale argument.
#for the scale argument we are going to have maxs vector - mins vector assigned, therefore for each column, 
#the values in the columns are divided by the corresponding max - min value for that column in the vector assigned to the scale argument.
#it returns a numeric matrix that we need to convert into a dataframe.

scaled.data <- scale(data, center = mins, scale = maxs - mins)
# print(class(scaled.data))
#convert back to a data frame
scaled.data <- as.data.frame(scaled.data)
# print(class(scaled.data))
# print(head(scaled.data))

#now we have normalized data which is between 0 and 1 and is ready to be fed to a NN model for training.

#train and test split
library(caTools)
set.seed(101)
sample <- sample.split(scaled.data$medv, SplitRatio = 0.7)
train <- subset(scaled.data, sample == T)
# print(head(train))
# print(str(train))
test <- subset(scaled.data, sample == F)
# print(head(test))
# print(str(test))

#to start building a NN model we need the 'neuralnet' package in R.
#use install.packages('neuralnet')
# install.packages('neuralnet')
# load the package using library(neuralnet) function
library(neuralnet)
#the neuralnet() function is used to build a NN
#this function does not take in the formula in the classic way we are used to. We have to specify
#the columns we need to use as predictor feature columns using the '+' symbol as a conjunction for the column names.

#to avoid typing out these column names in the formula, we can make use of names(), paste() functions to create
#the formula without manually typing the column names.

#the names() function returns a vector of the names of the columns in a data frame.
n <- names(train)
# print(n)
# print(class(n))

#paste() function concatenates the specified R objects together by converting them into character vectors . We can specify R objects to be combined
#separated by commas. An optional 'sep' argument could also be used to specify a character string used to separate the terms in the provided  R objects. 
# An optional 'collapse' argument can be used to specify a character string to contatenate the terms in the provided R objects.
f.0 <- paste('medv ~ ',paste(n[!n == 'medv'], collapse = ' + '))
# print(f.0)
#use the as.formula() function to create a formula type R object.
f <- as.formula(f.0)
# print(f)
# print(class(f))

#train the NN model using neuralnet() function
#the first argument is the formula
#the second argument is the training data
#the third argument is 'hidden' used to specify the vector of integers which denote the number
#of neurons in the corresponding hidden layer in the MLP NN.
#we will use 5 neurons for the first hidden layer and 3 neurons for the second hidden layer.
#to decide the appropriate number of neurons in the hidden layers read the wikipedia article.
#since our problem is regression instead of classification, set the 'linear.output' argument to T.
#for classification it is set to False.
nn <- neuralnet(f, data = train, hidden = c(5,3), linear.output = T)

#to plot the trained NN use the plot() function and pass the trained NN model name.
# plot(nn)

#observation: for casual people, the NN's are a Black Box. It is hard for them
#to understand the weighted vectors for the corresponding feature columns in the data.
#the black lines show the connection between the layers and the corresponding weights for every connection.
#the blue lines shows the bias weights added in each step.
#the bias term correspond to the intercept of the linear line we draw for the regression task which
#represents the trend in the data.

#for casuals it is hard to explain the inner working of the trained NN about the fitting, weights
#or the model in generall. They consider it as a blackbox. It can be observed that the model
#converged while training and can be used to make predictions on the test data.

#predict on test data
#use the compute() function instead of the predict() function.
#first argument is the trained NN model, the second argument is the test data without the labels.
predicted.nn.values <- compute(nn, test[1:13])
#the compute() function returns a list of 2, neurons and the net results.
#for neurons there is a list of length equal to the number of hidden layers plus the output unit we have in the NN model.
#we want the net results as they are the predictions for the test samples. 
# print(str(predicted.nn.values))
# print(predicted.nn.values)

#we scaled/normalized the data before splitting and training our model, we need to undo that change to get the true predictions.
#after grabbing the predicted values from the list, we will multiply with the (max - min) value of the medv column of the original unscaled data set before splitting and add the 
#min value of the medv column from the same unsplitted unscaled original data set.
#we do this to undo the centering and scaling effect we performed on the data set earlier to normalize the data.
true.predictions <- predicted.nn.values$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
# print(head(true.predictions))

# we need to unscale the original labels for test data as well to perform evaluation.
test.actual.medv <- test$medv * (max(data$medv) - min(data$medv)) + min(data$medv)
# print(head(test.actual.medv))

#since this is a regression task and we predicted continuous values for the test samples, we can 
#compute the MSE value as a performance measure for the NN model.
MSE.nn <- mean((test.actual.medv - true.predictions) ^ 2)
#or
# MSE.nn <- sum((test.actual.medv - true.predictions) ^ 2) / nrow(test)
# print(MSE.nn)

#visualize the error by plotting the predictions made vs the actual values for the test samples.
library(ggplot2)
#create a data frame for the predictions made and the actual values for the test samples.
error.df <- data.frame(test.actual.medv, true.predictions)
# print(head(error.df))

#plot the predictions on the y axis and the actual values on the x axis of a scatter plot
#plot a line over these points in the scatterplot using the stat_smooth() in the geometry layer.
#this line estimates the how good of a prediction we did, a perfect prediction on the test samples is a straight
#linear line with a slope of 1. This line seems to be pretty good. Our nn model did a good job on test data.
pl1 <- ggplot(error.df, aes(x = test.actual.medv, y = true.predictions)) + geom_point() + stat_smooth() + theme_bw()
# print(pl1)

#Takeaways
#NN is a black box as it is difficult to interpret how model exactly works and the importance of the variables in the model.
#Need to normalize the data before training the model.



