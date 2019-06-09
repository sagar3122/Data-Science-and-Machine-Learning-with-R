#Tree Method Project
#D Trees and RF
#In ISLR library use the college data set to train and predict Whether the college is public or private based off the features provided for the college.
library(ISLR)
# print(data())
# print(head(College))
# print(str(College))
#observation: 777 rows and 17 predictor features and 1 target feature of classifying whether college is public or private, the target column name is Private
#with 2 levels Yes or No.
df <- data.frame(College)
# print(head(df))

#EDA
#scaterplot for grad rate vs room and board cost colored by private column
library(ggplot2)
pl1 <- ggplot(df, aes(x = Room.Board, y = Grad.Rate)) + geom_point(aes(color = Private), size = 4, alpha = 0.5) + theme_bw()
# print(pl1)
#observation: with the increase in room and board cost there is an increase in the graduation rate.
# the expense for the private universities are realtively higher,
#also the grad rate at the private universities is relatively higher as well.

#histogram of full time undergrad students colored by Private column
pl2 <- ggplot(df, aes(x = F.Undergrad)) + geom_histogram(aes(fill = Private), color = 'black', bins = 50, alpha = 0.5) + theme_bw()
# print(pl2)
#observation: for lower count of full time undergraduates, a lot of private colleges have those counts as compared to public colleges.
#this means the private colleges are smaller institutions than the public colleges.
#as the full time undergrad count increases almost no private colleges have those counts but we can see some public colleges
#having those high counts of full time undergrad students.

##histogram of grad rate colored by private column.
pl3 <- ggplot(df, aes(x = Grad.Rate)) + geom_histogram(aes(fill = Private), color = 'black', bins =  50, alpha = 0.6) + theme_bw()
# print(pl3)
#observation: most colleges have a grad rate of 50 and above.
#for a grad rate of 60 and above the count of private universities is relatively high and gets higher as we look for higher grad rates.
#there should be no colleges with a grad rate greater than 100 as seen in the plot, but there is one college
#with grad rate higher than 100 which needs to corrected to 100.
#grad rates below 60 have a unifom count for both public and private universities.

# colleges with grad rate of 100 and above
library(dplyr)
#filter() removes the original index of the data frame and shows the new filtered rows assigned with a new ascending integer index starting from 1.
#whereas base subset() keeps the original index of the data frame 
college.high.grate <- df %>% subset(Grad.Rate > 100) 
# print(college.high.grate)
# print(class(college.high.grate))

# print(str(df))
#change the Cazenovia college grad rate to 100 using recode()
# df['Cazenovia College', "Grad.Rate"] <- recode(df['Cazenovia College', "Grad.Rate"], '118' = as.numeric(100))
#the same operation can also be done using simple selection of the particular cell and assinging the desired value.
df['Cazenovia College', "Grad.Rate"] <- 100
# print(df['Cazenovia College',])
# print(str(df))

#build the model
#split for train and test
library(caTools)
set.seed(101)
#difference between filter() and subset() is filter does not require conditional statements on columns conjuncted or disjuncted together.
#They are separated by commas and thus are more readable.
#Also, filter() removes the original index of the data frame and shows the new filtered rows assigned with a new ascending integer index starting from 1.
#whereas base subset() keeps the original index of the data frame. 
sample <- sample.split(df$Private, SplitRatio = 0.7)
train <- subset(df, sample == T)
# print(head(train))
# print(str(train))
test <- subset(df, sample == F)
# print(head(test))
# print(str(test))

#build the Dtree model
library(rpart)
tree <- rpart(Private ~ ., data = train, method = 'class')
# print(tree)
#to get the summary about the fitted Dtree model call summary() on it.
# print(summary(tree))
# printcp(tree)

#predict on the test data set using the predict() function
#the first argument is the trained Dtree model, and the second argument is the test data set on which we want to predict the values for target feature.
predicted.probabilities <- predict(tree, newdata = test)
# print(head(predicted.probabilities))
# print(class(predicted.probabilities))
#observation: the predicted probabilities for both Yes and No levels of the target feature are returned for each observation in the test data set.
#use the predicted probabilities to predict the actual label for the observations in the test data set.
#this can be done using the ifelse() to compare the yes and no predicted probabilities of a test observation and assign yes label to the corresponding test observation
#if the yes probability is greater than no probability otherwise assign a no label to the corresponding test observation.
# test$predictions <- ifelse(predicted.probabilities[,'Yes'] > predicted.probabilities[,'No'], 'Yes', 'No')
#this can also be done by applying a custom function(which compares the yes probabilities to 0.5 cutoff value) to the yes prediction values of all the test observations using sapply() function.
joiner <- function(x){
  if(x > 0.5){
    return('Yes')
  }else{
    return('No')
  }
}
#apply using sapply()
test$predictions <- sapply(predicted.probabilities[,'Yes'], joiner)
# print(head(test))
# print(tail(test))

#confusion matrix
confusion.matrix <- table(test$predictions, test$Private)
# print(confusion.matrix)
# accuracy
accuracy <- mean(test$Private == test$predictions)
# print(accuracy)


#plot the Dtree using the rpart.plot library to interpret it intuitively
library(rpart.plot)
#use the prp() and pass the built Dtree model as the argument
# print(prp(tree))
#observation: the root node is the f.undergrad feature which is a strong feature for splitting
#as we already saw in the plots that public schools have higher f.undergrad values than the private colleges.
#the next nodes have the feature outstate tuition, which is also strong for splitting as public schools have higher 
#outstate fee than the instate fee, whereas private colleges have similar instate and outstate fee. Thus, the attributes for following nodes are 
#chosen based on their importance in prediciting the values for the target variable which is synonomus to how pure of a split is performed by the attribute on the available training data.


#RF model
#use the randomForest library
library(randomForest)
#build a RF model using randomForest() function.
#assign the importance argument as T, this is used to measure the importance of each predictor feature in predicting the values for target feature.
#the measurement is based off how much information we get when we use an attribute to make a split in a Dtree. By information, we mean how much lopsided the split was in terms of class labels assigned to the samples which were provided before the split.
#the measure of the lopsidedness/purity/information gain out of the split by an attribute is measured using either the entropy or gini values which are used to measure the impurity in data before and after the split.
#the higher importance value/higher information gain/higher lopsisded split/higher pure split for a predictor feature the more important it is in terms of predicting the values of the target feature of a test sample.
rf.model <- randomForest(Private ~ ., data = train, importance = T)
#to get the confusion matrix of the performance of the built RF model on the training data set used for training, use the 'confusion' keyword alongside $ notation.
# print(rf.model$confusion)
#observation: performance for predicting 'Yes' for a private school is better as seen by the relatively lower missclassification error.
#This also makes sense as there is a lot of private school data as compared to public school data.

#to get the measure of importance for each predictor feature use the 'importance' keyword.
#print(rf.model$importance)
#the importance values are essentially the information gains after making a split using an attribute in all the DTress in the RF model.
#these information gain for an attribute are computed by subtracting the gini index/entropy values 
#for the samples at the nodes(child) after the split from the node(parent) before the split.
#observations: the first two columns are:
#mean raw importance score of variable x for class No
#mean raw importance score of variable x for class Yes 
#raw importance score measures how much more helpful than random a particular predictor variable is in successfully classifying data for that class label.
#third column MeanDecreaseAccuracy: it measures how much misclassification error is reduced by the inclusion of this predictor feature in the model.
#fourth column MeanDecreaseGini: #A low gini index value(high decrease in gini index value) for an attribute used for splitting in the dtrees of the RF model
#suggest about that attribute that it is a strong fature in splitting the training samples more purely and returns a good information gain after the split about the samples.
#we compute the mean of the decrease in the gini index values of the attributes used for splitting in all the Dtrees in the RF model which provides use with the importance values for those 
#features in predicting the values of target feature for the test samples.
#we can also compute the importance of groups of features in the RF model as well.
#for example for petals in petal.length, petal.width etc also for length and width likewise as well.


#predict on the test data set using predict()
test$rf.predictions <- predict(rf.model, newdata = test)
# print(head(test))

#accuracy
rf.accuracy <- mean(test$Private == test$rf.predictions)
# print(rf.accuracy)


#confusion matrix for the predictions made on test data set
rf.confusion.matrix <- table(test$rf.predictions, test$Private)
# print(rf.confusion.matrix)

#the rf model performed slightly better than the Dtree model in certain aspects such as accuracy,
#the magnitude of how much better depends on the performance measure computed.
#it also depends on what error whether type I or type II we are trying to decrease.


