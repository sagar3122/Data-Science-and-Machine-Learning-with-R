#we require the rpart package in R to create DTrees and RF.
#install using the install.packages('rpart') function
#install.packages('rpart')
#load the rpart library using library(rpart) function.
library(rpart)
#call help on rpart() using help('rpart') to know more about it.
#rpart() helps to build recursive partitioning and regeresion trees i.e. DTrees.
#the function takes in the formula, data etc as previous models to build DTrees.

#use the Kyphosis data set built in rpart library in R.
# print(data())
# print(str(kyphosis))
#observations: there are 81 rows and 4 features. The data set is about the spinal deformation(kyphosis) treatment in children.
#age is the age of children in months, number is the no of vertebraes involved in kyphosis, start column tells the number of top most vertebrae operated on.
#and the kyphosis column tells if it is present or not, that is what we have to predict.

# print(head(kyphosis))

#lets build our DTree model, all the predictor features are integers and the target feature is categorical, DTree is a good choice.
#use the rpart(), include the formula as we do in linear regression/logistic regression as the first argument.
#in the formula, we say predict kyphosis label using all the other features in the data set.
#assign the method argument to class as we are perofrming a classification task.
#assign the training data set using the data argument.

tree <- rpart(Kyphosis ~ ., method = 'class', data = kyphosis)

#there are a lot of functions used to examine the dtree model created.
#plotcp(fit) to plot the cross-validation results of the dtree model.
#print(fit) to print the results of the dtree model.
#plot(fit) to plot the dtree model created.
#text(fit) to label the dtree model plot.
#rsq.rpart(fit) to plot the approximate R-squared and relative error for differnet splits. This creates 2 plots.
#this plot is appropriate for anova method which is used when we do some time series analysis or regression tasks.

#use the printcp(fit) to display the cp table for the dtree model created.
#It Prints a table of optimal prunings based on a complexity parameter.
# printcp(tree)
#observation: returns the formula used for creating the d tree.
#variables/features used to create the d tree.
#root node error: misclassification error right after we do our initial split.
#there is other error related info as well. 

#to visualize the dtree model created use the plot(fit) function
#pass the uniform argument as T and the main argument to set a title of the plot.
# print(plot(tree, uniform = T, main = 'Kyphosis DTree'))
#observation: this does not plot the labels out for the Dtree, for that use the text() function
#pass the dtree model as the first argument, then pass the 'use.n' argument as T, then pass the 'all' argument as T.
#the last two arguments are to just print out all the labels for the Dtree model created.
# print(text(tree, use.n = T, all = T))
#we need to have the plot() executed before the text() to see the labels of the dtree model printed out on the dtree model plot itself.

#use the rpart.plot library instead to plot the dtree's.
#install using the install.packages('rpart.plot') function
# install.packages('rpart.plot')
#load the packages using library(rpart.plot)
library(rpart.plot)

#use the prp() of the rpart.plot library to create better plots for the dtree models created.
#pass the dtree model as the first argument.
# print(prp(tree))
#observation: the plot created using the prp() function are intuitive.


#Random Forests
#RF improve the predictive accuracies by generating a large number of bootstrap dtrees based on random samples of features used for splitting.
#Process invloved in RF
#Classify a case using each tree in the forest and then combining the results across all the trees to predict the final outcome of the RF.
#In classification it is the majority votes of the dtress in the forest.

#install the random forest library using install.packages('randomForest') function
# install.packages('randomForest')
# load the package using library(randomForest) function
library(randomForest)

#to build a RF model use the randomForest() function of the randomForest package
#the first argument is the formula just like we pass in the rpart() function for a dtree.
#here as well we are going to predict Kyphosis feature using all the other features in the data set.
#the second argument is data used to specify the training data set.
rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)
#check the help for randomForest() function
#there is a list of values held inside the random forest model, we can grab components out of the RF model.
#use the $ notation after the RF model name to grab the components held inside the built RF model.
#for example grab the predicted values for the target feature for the training data samples using 'predicted' keyword.
# print(rf.model$predicted)
#to know the number of trees grown in the RF model use the 'ntree' keyword.
# print(rf.model$ntree)
#observation: 500 is the default value of trees grown in a RF model.
#the misclassification error can be brought down by adding more Dtrees to be grown and used inside the random forest.
#but after a certain value adding more Dtrees will not help in reducing the misclassification error.
#to check that we can plot the number of DTrees used vs the misclassification error.
#to call the confusion matrix for the training data used to train the RF model use the 'confusion' keyword.
# print(rf.model$confusion)

#the RF is used more often than just a single Dtree. There are many other useful components inside a trained RF model to be used.
#RF are powerful as well as easy to implement. They are used for distributed computing, as we can have some Dtrees on one server and some on the other server
#and they are indepedent of each other, do not need to communicate to each other. We can then combine the results using votes to predict the final outcome.


#print the results of the built RF model.
# print(rf.model)
#observation: this gives the estimate of the error rates/misclassification errors.
#we see the formula used to build the RF model.
#Type is classification RF model, no of trees grown in the forest.
#total error rate is 22.22%.
#confusion matrix for the training data set used to build the RF model.

#to get the impottance of each predictor feature use the importance() function
#pass the name of RF model built as the first argument.
# print(importance(rf.model))
#observation: Start is the most important, then Age and then Number is the least important.



