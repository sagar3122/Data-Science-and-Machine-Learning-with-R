#bank authentication data set from UCI repository
#NN are used a lot for computer image recognition.
#in this project we will use the wavelet transformation statistical data for images to check whether a bank note is authentic
#or not.
#this data set has features for the wavelet transformed image data such as variance, skewness, curtosis, entropy.
#check the wikipedia article on wavelet transform.
#the wavelet transform for images is used to make the computers understand the image data. Basically understand 
#the image it is looking at.
#we will provide these features of wavelet transformed statistical image data to the NN and see if it processes that
#data and detect whether the image of note is authentic or fradulent.

#class feature represents if the note is authentic or fraudulent.
#this is a classification task

#read the csv file
data <- read.csv('bank_note_data.csv')
# print(head(data))
# print(str(data))
# print(tail(data))
#shuffle the data set using sample_n() in dplyr
#use the size of sample as the no of rows in data set.
library(dplyr)
data <- sample_n(data, nrow(data))
# print(head(data))
# print(tail(data))
# print(str(data))

#it is important to scale/normalize the data before training the NN model with it.
#we won't normalize the data here because the values in the columns are in the same order of magnitude,
#the entropy values are a little smaller but that won't affect much, the values for entropy range from -5 to 5.
#in this case it is not a huge deal to not normalize the data as the values are roughly in the same order of magnitude.
#we should always scale/normalize when the max and min values in the columns of the data set have a huge difference.


#train and test split
library(caTools)
set.seed(101)
sample <- sample.split(data$Class, SplitRatio = 0.7)
train <- subset(data, sample == T)
# print(head(train))
# print(str(train))
test <- subset(data, sample == F)
# print(head(test))
# print(str(test))

#build the neural network model
library(neuralnet)
#use neuralnet() function
#prepare the formula, use the names(), paste() and as.formula() function
n <- names(train)
f <- as.formula(paste('Class ~', paste(n[!n == 'Class'], collapse = ' + ')))
# print(f)
#use 10 neurons in the first hidden layer
nn <- neuralnet(f, data = train, hidden = c(5,3), linear.output = F)
# plot(nn)

#predict using the compute() function
predictions.nn.values <- compute(nn, test[1:4])
# print(str(predictions.nn.values))
# print(head(predictions.nn.values$net.result))
#observations: the predicted values are the probabilities for the test samples to belong to class 1.

# round off these predicted probabilities to either 0 or 1 to get if they belong to class 0 or class 1.
#use the round() and sapply() funcitons
true.predictions <- sapply(predictions.nn.values$net.result, round)
# print(head(true.predictions))

#evaluate performance by creating a confusion matrix using the actual labels
confusion.matrix <- table(true.predictions, test$Class)
# print(confusion.matrix)

#100% accuracy with NN

#Compare the results by building a Random Forest model
library(randomForest)

#set the 'class' feature column as factor because in RF the target feature column should be a factor for classification tasks.
data$Class <- factor(data$Class)
# print(str(data))

#redo the train and test split
set.seed(101)
sample <- sample.split(data$Class, SplitRatio = 0.7)
train.rf <- subset(data, sample == T)
# print(head(train.rf))
# print(str(train.rf))
test.rf <- subset(data, sample == F)
# print(head(test.rf))
# print(str(test.rf))

model <- randomForest(f, train.rf)
rf.predictions <- predict(model, test.rf[1:4])
# print(rf.predictions)
# print(class(rf.predictions))

#evaluate the performance by computing a confusion matrix
confusion.matrix.rf <- table(rf.predictions, test.rf$Class)
# print(confusion.matrix.rf)

#the RF makes few mistakes in classifying the test data for both class 0 and class 1. Still it performs good,
#but the NN has a 100% accuracy for this data set.








