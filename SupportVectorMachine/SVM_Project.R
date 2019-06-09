#SVM Model
#predict whether or not the borrower paid the loan back in full.
#read the loans csv
loans <- read.csv('loan_data.csv')
# print(head(loans))
# print(str(loans))
# print(summary(loans))

#converting inq.last.6mths, delinq.2yrs, pub.rec, not.fully.paid, credit.policy. 
#to factor type features using factor() function
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <-factor(loans$credit.policy)
# print(str(loans))

#EDA
library(ggplot2)
#histogram of fico score colored with not full paid column
#to manually change the color shades while filling a histogram based off a feature values, use the scale_fill_manual() function
#provide the 'values' argument with a vector of colors in the order you want them to be assigned to the 
#values of the feature you are filling the histogram with.
pl1 <- ggplot(loans, aes(x = fico)) + geom_histogram(aes(fill = not.fully.paid), color = 'black', position = position_stack(reverse = T), bins = 40, alpha = 0.5) + theme_bw()
pl1 <- pl1 + scale_fill_manual(values = c('green','red'))
# print(pl1)
#observations: there is a decreasing trend in the number of people who have not paid the loan in full as we go higher in fico score value.
#for fico greater than 750 there are very few people who have not paid the loan in full.
#for fico score of around 690 we have the highest number of people who have not paid the loan in full.
#generally at all fico scores most of the people do pay the loan in full.
#All these observations make sense.

#create a bar plot of purpose counts colored by not full paid column
#use position = dodge in the geometry layer.
pl2 <- ggplot(loans, aes(x = purpose)) + geom_bar(aes(fill = not.fully.paid), color = 'black', position = 'dodge') + theme_bw()
# print(pl2)
#observation: debt_consolidation has the most not paying the loan back in full.
#followed by all other and small business.
#people are taking loans for debt consolidation, credit card, all other and so on in decreasing order of interest.


#scatter plot of fico score versus interest rate
pl3 <- ggplot(loans, aes(x = int.rate, y = fico)) + geom_point(aes(color = not.fully.paid), alpha = 0.2) + theme_bw()
# print(pl3)
#observation: the people with higher fico score have lower interest rate and people with lower fico score have higher interest rate
#the people with lower fico score and higher interest rate have a slightly higher number of people not paying the loan in full
#as compared to the people with higher fico score and lower interest rates.


#build the model
#train and test split
library(caTools)
set.seed(101)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, sample == T)
test <- subset(loans, sample == F)
# print(head(train))
# print(str(train))
# print(head(test))
# print(str(test))

#load the e1071 library
library(e1071)
#train a svm model on the training data set using svm()
# model <- svm(not.fully.paid ~ ., data = train)
# print(summary(model))
#observation:
# cost:  1 
# gamma:  0.01724138 

#predict on the test data set
#pass the test data with only predictor features
# test$predictions <- predict(model, newdata = test[1:13])
# print(head(test$predictions))

#confusion matrix
# confusion.matrix <- table(test$predictions, test$not.fully.paid)
# print(confusion.matrix)
#observation: the results are not good as our model is predicting for every test sample to be fully paid by predicting 0 for all.

#accuracy
# accuracy <- mean(test$predictions == test$not.fully.paid)
# print(accuracy)

#we need to get better cost and gamma values to train the model with, therefore we need to do tuning.
#tuning the model using the tune() function.
# tune.results <- tune(svm, train.x = train[1:13], train.y = train[, 14], kernel = 'radial', ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 0.5)))
#the above line of code throws an error 
#or instead of specifying the predictor and target columns of training data set in train.x and train.y arguments, we can use the formula
# in the train.x argument as we use while fitting the model and provide the training data set in 'data' argument.
#using jose's initial cost and gamma values defined as vectors in a list for ranges argument.
# tune.results <- tune(svm, train.x = not.fully.paid ~ ., data = train, kernel = 'radial', ranges = list(cost = c(100,200), gamma = c(0.1)))
# print(tune.results)

#observation: it is a good practice to use cost and gamma values in the power of 10 early on and then
#narrow down our tuning based on the results of the previous tuning.
#jose's results: cost: 100 gamma: 0.1 error rate: 0.2054
#my own tuning gave me cost 0.1 and gamma 0.01 with an error rate of 0.1600305
#we can try more similar ranges to get better error rates.
#use these to build new svm model and check for the performance

tuned.model <- svm(not.fully.paid ~ ., data = train, kernel = 'radial', cost = 0.1, gamma = 0.01)
# print(summary(tuned.model))
#observation:
# cost:  0.1
# gamma:  0.01

#predict on test data set
#provide only the predictor feature columns
test$predictions <- predict(tuned.model, newdata = test[1:13])

#performance
#confusion matrix
confusion.matrix <- table(test$predictions, test$not.fully.paid)
# print(confusion.matrix)

#accuracy
accuracy <- mean(test$predictions == test$not.fully.paid)
# print(accuracy)

#observation: it is almost the same performance.
#we can tune more to get better cost and gamma values and thus train a better model 
#which performs better on the test data set.
#the better performance is subject to what performance measure we are using to evaluate the model.


