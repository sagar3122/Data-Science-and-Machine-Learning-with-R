library(ggplot2)
#titanic data set -> predict if the person survived or died in the titanic disaster.
#get the train and test data set
df.train <- read.csv('titanic_train.csv')
# print(head(df.train))
# print(str(df.train))
#the cabin has missing values
#the Name column is not much of a factor in predicting for a person's survival, we can do some feature engineering and take the titles out
#of the individual names which might have affected in a person's survival status.

df.test <- read.csv('titanic_test.csv')
df.test.submit <- select(df.test, PassengerId)
# print(head(df.test.submit))
# print(str(df.test.submit))


#Exploratory data analysis
#check for missing values(NA) in the data frame
#use the amelia package to do so, it is a good package to explore and visualize missing values in the data set, install the package using install.package('Amelia')
# install.packages('Amelia')
#load the Amelia package using the library(Amelia) function.
library(Amelia)
#we will use amelia for plotting a missingness map. Plotting one help us to find the NA values in data set. Use the missmap() function.
#pass the data frame name as the first argument, then the main argument with a charcter string assigned to be used as the title for the plot, 
#then col argument to provide a vector of colors used for cells with missing values and cells with observed values respectively. 
#set the legend as false for this one.
# pl <- missmap(df.train, main = 'Missing Map', col = c('yellow', 'black'), legend = F)
# print(pl)
#observation: we can say roughly 20 percent of age data is missing.
#the proportion of age values missing is small enough to be reasonably replaced by some other form of imputation.
#cleaning data also invloves substituiting for missing data.
#we will fill in missing values in the data once we reach to the model building phase.

#do some more eda
#do a bar plot of how many people survived and how many did not
pl1 <- ggplot(df.train, aes(x = factor(Survived))) + geom_bar(fill = 'blue', alpha = 0.5) + theme_bw()
# print(pl1)
#observation: A significant number of people died more than the number of people who survived.
#bar plot for Passenger class column, we have three classes 1,2,3
pl2 <- ggplot(df.train, aes(x = Pclass)) + geom_bar(aes(fill = factor(Pclass)), alpha = 0.5) + theme_bw()
# print(pl2)
#observation: 3rd class passengers are the highest in number by far and also the 1st class passengers are a little more in number than 2nd class passengers.
#bar plot for sex column
pl3 <- ggplot(df.train, aes(x = Sex)) + geom_bar(aes(fill = factor(Sex)), alpha = 0.5) + theme_bw()
# print(pl3)
#observation: a lot of male passengers on board as compared to female passengers, almost double in number.

#histogram for age column in the data set
pl4 <- ggplot(df.train, aes(x = Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_bw()
# print(pl4)
#observation: there are relatively younger people on board around the age of 20 to 40, there are few older people,
#also there are quite a few children.

#bar plot for sibling and spouses on board
pl5 <- ggplot(df.train, aes(x = SibSp)) + geom_bar(aes(fill = factor(SibSp)), alpha = 0.5) + theme_bw()
# print(pl5)
#observation: a lot of people do not have any siblings or spouses, no have has 6 or 7 sibling or spouses.
#there is a decrease in the number of people for having them as the number of siblings and spouses increase. 

#plot a histogram for fare column in the data set
pl6 <- ggplot(df.train, aes(x = Fare)) + geom_histogram(bins = 20, fill = 'green', color = 'black', alpha = 0.5) + theme_bw()
# print(pl6)
# observation: there are lot of people travelling at lower fare as most of the people are travelling 3rd class.
#there are few people travelling at higher fares as there are fewer people in the second and first class.

#we will perform cleaning of the data by filling in the missing values, also we want our data to be cleaned before we build our logistic regression.
#we saw in the missingness map plot, there are NA values in the age column of the data set.
#we can drop the rows in the data set which have NA values for the age column but it is not a wise option as there are 177 significant number of rows which have NA values out of the 900 ish total rows.
#rather we will fill in those NA cells in those rows for the age column through imputation. We will use the mean/average value of the age column.
#we can opt for a better wiser option to use the mean/average value of the age based on Passenger class. 
#this is a better option because filling in the age as the average age of the person's Passenger class makes more sense then just 
#filling in the average age value of all the passengers belonging to all the Passenger classes. The former one will be more closer to the actual value most of the times.
#this is us essentially estimating the age value based on Passenger class of a person.

#to find out the average age values based on passenger class, we will do a box plot of passenger class vs age value
#the group argument in the aes() in geometry layer of a box plot puts a categorical feature(using the factor()) on the x axis to plot against as required by a box plot.
#this can also be achieved when we assign x = factor(feature name) in the aesthetic layer.
pl7 <- ggplot(df.train, aes(x = factor(Pclass), y = Age)) + geom_boxplot(aes(fill = factor(Pclass)), alpha = 0.4)
#to set numbers on the y axis for readability use scale_y_continuous() and use the breaks argument to provide a 
#vector of numbers to be shown at the y axis, to build that vector use the seq() function, set the max and min value
#using the same named functions and use the 'by' argument to set the period at which the numbers are shown on the y axis.
pl7 <- pl7 + scale_y_continuous(breaks = seq(min(0), max(90), by = 2 ))  + theme_bw()  
# print(pl7)

#observations: mean age values for Passenger class 1 -> 37
# mean age values for Passenger class 2 -> 29
# mean age values for Passenger class 3 -> 24

#the 1st class passengers are a little older than 2nd class passenger which themselves are a little older than 3rd class passengers.
#these ages intuitively makes sense as it requires some time to aquire a certain amount of wealth and travel 1st class.

#lets do imputation for missing age values based on the class passenger is travelling in.
#built a custom fucntion to do so.
# to get the length of a vector use length()
impute.age <- function(age, Pclass){
  out.age <- age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(Pclass[i] == 1){
        out.age[i] <- 37
      }else if(Pclass[i] == 2){
        out.age[i] <- 29
      }else{
        out.age[i] <- 24
      }
    }else{
      out.age[i] <- age[i]
    }
  }
  return(out.age)
}
#to actually impute the missing age values based on Pclass, pass the actual age and pclass columns of the data frame.
fixed.ages <- impute.age(df.train$Age, df.train$Pclass)
# print(head(fixed.ages))
#re assign the Age column with fixed ages vector
# print(any(is.na(df.train)))
df.train$Age <- fixed.ages
# print(head(df.train))
# print(any(is.na(df.train)))

#plot the missingness map to check for NA values
# pl8 <- missmap(df.train, main = 'Imputation Check', col = c('yellow','black'), legend = F)
# print(pl8)


#build our log R model
#do some final cleanup; remove features we are not going to use and check the features for correct data types.
# print(str(df.train))
#remove passengerID, Name, Cabin, Ticket column as they have too many levels in those columns to be actually useful in classification task.
#but with some feature engineering we can take the titles out of the names and cabin levels out of the cabin to be helpful in performing classification in this problem.
#we won't do any of this but just remove the unwanted features.
#use dplyr
library(dplyr)
#use select() of dplyr to select columns from the data frame
#pass the feature names that we do not want with a '-' sign in front of them as the arguments.
df.train <- select(df.train, -Name, -PassengerId, -Ticket, -Cabin)
# print(head(df.train))
# print(str(df.train))
#lets convert the Survived column, Pclass column into factor type instead of integer/numerical type.
#also we have a option to convert the SibSp and Parch columns to factor type as well since there are not many different numbers used in these integer type columns
#and it makes more sense for them to have levels in this case.
#use the factor() to do so
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)
# print(str(df.train))
#we see that embarked column has one of the feature values as empty " ", it might be an important feature value for a person to know 
#what port they came in, for us it may or may not affect the model too much to leave it like that. 

#train the model
#use the glm() function which stands for generalized linear model for log R
#the formula notation is same as y ~ x, here we will use all the features in the data frame.
#set the family argument which is used to set the description of the error distribution. It is used to specify we are using 
#logistic regression for training our ML model, the log R uses cross entropy as the objective/error function and sigmoid function as the activation function.
#for log R set family to binomial(link = 'logit')
#there are other families we can use in glm() for different ML algo's.
#set the training data set using data argument
log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = df.train)
# print(summary(log.model))
#observations : In statistics: the lower p values for the variables suggest that it is highly unlikely that the corresponding variable is not important.
#we say the corresponding significance stars and lower p values suggest the variable is important in predicting the class label.
#the coefficient estimate values are the weights corresponding to the variables which are meant to be learnt by a log R model in the training phase.
#this summary makes sense because being a male was helpful in surviving, almost everyone from class 3 died and other variables with stars make sense as well.

#predictions
#clean the df.test data frame to perform prediction
# print(head(df.test))
df.test <- select(df.test, -PassengerId, -Name, -Ticket, -Cabin)
# print(head(df.test))
# print(str(df.test))
df.test$Pclass <- factor(df.test$Pclass)
df.test$SibSp <- factor(df.test$SibSp)
df.test$Parch <- factor(df.test$Parch)
# print(str(df.test))
#imputing for missing age values
# pl8 <- missmap(df.test, name = 'Missing Map', col = c('yellow','black'), legend = F)
# print(pl8)
#box plot for finding the mean age value based on passenger class.
pl9 <- ggplot(df.test, aes(x = factor(Pclass), y = Age)) + geom_boxplot(aes(fill = factor(Pclass)), alpha = 0.4)
pl9 <- pl9 + scale_y_continuous(breaks = seq(min(0),max(90), by= 2)) + theme_bw()
# print(pl9)
#use custom function impute.test.age() to perform imputation by passing in the age and pclass columns of the test data set.
impute.test.age <- function(age, Pclass){
  out.age <- age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(Pclass[i] == 1){
        out.age[i] <- 42
      }else if(Pclass[i] == 2){
        out.age[i] <- 26.5
      }else{
        out.age[i] <- 24
      }
    }else{
      out.age[i] <- age[i]
    }
  }
  return(out.age)
}
fixed.test.ages <- impute.test.age(df.test$Age, df.test$Pclass)
# print(head(fixed.test.ages))
#re assign the age variable with fixed test ages vector(imputed age values)
df.test$Age <- fixed.test.ages
#check for imputation being performed by plotting a missingness map again.
# pl10 <- missmap(df.test, main = 'Imputation Check', col = c('Yellow','Black'), legend = F)
# print(pl10)
# #there is one missing value in fair column, lets just remove that row
# # print(str(df.test))
# df.test <- filter(df.test, is.na(df.test$Fare) == F)
# # print(any(is.na(df.test)))
# # print(head(df.test))
# # print(str(df.test))
# #missingness map one more time
# # pl11 <- missmap(df.test, main = 'Fare NA value removal check', col = c('Yellow','Black'), legend = F)
# # print(pl11)

#predict for test data set
#to check the levels of a categorical column use levesl()
# print(levels(df.test$Parch))
#since there is a new parch level in the test file, we will replace all the parch column values set to 9 to now be set to 6.
#use the recode() function in dplyr for this, pass in the column in the first argument, in the second argument assign the new value we want in place of the old value.
#format to do so, if the column is categorical, 'old value' = 'new value'
#this function returns a recoded vector.
#reassign the vector after recoding.
df.test$Parch <- recode(df.test$Parch, '9' = '6')
# print(levels(df.test$Parch))
fitted.probabilities <- predict(log.model, newdata = df.test, type = 'response')
# print(head(fitted.probabilities))
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)
# print(head(fitted.results))
df.test$Survived <- fitted.results
df.test.submit$Survived <- fitted.results
# print(head(df.test))
# print(tail(df.test))
# print(head(df.test.submit))
# print(str(df.test.submit))
#write the final data frame out to a csv file
write.csv(df.test.submit, file = 'titanic_final_submission.csv')
#got 76% accuracy on Kaggle



#split the train and test from the df.train data set for training and testing repectively. 
#We are doing this as we can then cross check the results.
#lets make the split using caTools
# library(caTools)
# set.seed(101)
# sample <- sample.split(df.train$Survived, SplitRatio = 0.7)
# final.train <- subset(df.train, sample == T )
# # print(str(final.train))
# # print(head(final.train))
# final.test <- subset(df.train, sample == F)
# # print(str(final.test))
# # print(head(final.test))
# #train the log R model using final.train data set 
# final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final.train)
# # print(summary(final.log.model))
# 
# #predict on final.test
# #for classification algo's we specify the type argument in the predict() as response,
# #which returns the prediction made as probability values(probability to belong to class 1). We then based on a cutoff probability value, we can assign test data points to a class label.  
# fitted.probabilities <- predict(final.log.model, newdata = final.test, type = 'response')
# # print(head(fitted.probabilities))
# #to assign class labels to the test data points, use the ifelse() function instead of writing a custom function with if else statements.
# #first argument is similar to a condition set for a if statement, the second argument is the action if that condition is true, 
# #the third argument is the action if that condition is not true. 
# fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)
# # print(head(fitted.results))
# #compute the misclassification error using the mean() function
# #pass the fitted.results vector and Survived column/vector from the final.test dataframe and take into
# #account the nonsimilar instances(between predicted and actual values) for the data point in the test data set
# #and taking the mean of those number of instances out of the total instances computes the misclassification error.
# #missclassification error is the error rate used to represent the no of mistakes made while making predictions.
# #missclassification error = (FN + FP) / (TP + FP + FN + TN)
# missclass.error <- mean(fitted.results != final.test$Survived)
# # print('---------------------Missclassification Error---------------------------------')
# # print(missclass.error)
# # print('----------------------------Accuracy-----------------------------------------')
# # print(1 - missclass.error)
# #it is better to use all the measures of a confusion matrix to evaluate the performance of our model rather than just accuracy.
# #to create a confusion matrix we can use the table() function and pass in the actual values of target variable in the test data set and 
# #the predicted probabilitites of the target variable for the test data set which are greater than the cutoff probability value.
# #the later one returns a logical vector for the predicted probabilities vector based on the condition of being greater than 0.5 in value.
# #the table() does all the work by comparing the actual and predicted labels for each test data point and compute all the components of the confusion matrix.
# # print(fitted.probabilities > 0.5)
# confusion.matrix <- table(final.test$Survived, fitted.probabilities > 0.5)
# # print(confusion.matrix)
# #format of a confusion matrix 
# # TN FP
# # FN TP











