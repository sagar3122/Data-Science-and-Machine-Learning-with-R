#UCI adult data set
#project includes dealing with problems in data cleaning

#get the data, read the csv file
adult <- read.csv('adult_sal.csv')
# print(head(adult))
library(dplyr)
#drop the repeated index column
adult <- select(adult, -X)
# print(head(adult))
# print(str(adult))
# print(summary(adult))

#data cleaning
#we will reduce the number of factors in the categorical columns where there are a lot of factors than it may be necessary.
#feature engineering is used to do so. We do this so that each factor level has more data points and data is more readable and interpretable.
#use table() to check the frequency of the factors of any categorical column in a data frame.
#do for type_employer column
# print(table(adult$type_employer))
# there are 1836 missing values
# two smallest groups are 'never worked' and 'without pay'
#combine these under the name 'Unemployed'
#custom function to do so which also makes use of as.character() function
factor.Unemployed <- function(x){
  if(x == 'Never-worked' | x == 'Without-pay'){
    x <- 'Unemployed'
  }else{
    x <- as.character(x)
  }
} 
#convert never worked and without pay to unemployed using the sapply() and factor.Unemployed()
adult$type_employer <- sapply(adult$type_employer, factor.Unemployed)
# print(table(adult$type_employer))

# combine State and Local gov factors into SL-gov also combine self-emp-inc and self-emp-not-inc into self-emp
#build a custom function for the same
factor.combine <- function(x){
  if(x == 'Local-gov' | x == 'State-gov'){
    x <- 'SL-gov'
  }else if(x == 'Self-emp-inc' | x == 'Self-emp-not-inc'){
    x <- 'self-emp'
  }else{
    x <- as.character(x)
  }
}
#apply this custom function using sapply()
adult$type_employer <- sapply(adult$type_employer, factor.combine)
# print(table(adult$type_employer))

#frequency of factors in marital column
# print(table(adult$marital))
#custom function to reduce the factors to Married, Not-Married and Never-Married
factor.marital <- function(x){
 
  if(x == 'Married-AF-spouse' | x == 'Married-civ-spouse' | x == 'Married-spouse-absent'){
    x <- 'Married'
  }else if(x == 'Divorced' | x == 'Separated' | x == 'Widowed'){
    x <- 'Not-Married'
  }else{
    x <- as.character(x)
  }
}

#apply this custom function using sapply()
# print(head(adult$marital))
adult$marital <- sapply(adult$marital, factor.marital)
# print(table(adult$marital))

#country column
#check the factors in country column
# print(table(adult$country))
#create a custom function to reduce the factors in country column by replacing the country names with the continents they belong to.
#first we will create character vectors containing names of countries based on continents they belong to.
Asia <- c('China','Hong','Iran','Japan','India','Cambodia','Laos','Philippines','Vietnam','Thailand','Taiwan')
North.America <- c('Puerto-Rico','United-States','Canada')
Europe <- c('England','France','Germany','Greece','Ireland','Italy','Holand-Netherlands','Poland','Hungary','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')
Other <- c('South')
factor.country <- function(x){
  
  if(x %in% Asia){
    x <- 'Asia'
  }else if(x %in% North.America){
    x <- 'North.America'
  }else if(x %in% Europe){
    x <- 'Europe'
  }else if(x %in% Latin.and.South.America){
    x <- 'Latin.and.South.America'
  }else{
    x <- 'Other'
  }
}

#apply this custom function using sapply()
adult$country <- sapply(adult$country, factor.country)
# print(table(adult$country))
# print(str(adult))
#all the custom functions applied to the factor columns converted them to character type columns, we need convert them back to factor using factor().
adult$type_employer <- factor(adult$type_employer)
adult$marital <- factor(adult$marital)
adult$country <- factor(adult$country)
#another way of doing the same thing is we can convert each entry in these columns as factor type by applying the factor() function using the sapply() function.
# print(str(adult))

#we can club the factors in education and occupation columns as well to clean the data more and see the effects of those in building the model.
#but we will move ahead and deal with the missing data in the data frame.

#missing data
#plot missingness map
library(Amelia)
#convert cells with '?' to a NA value.
#to select entries with specific values in a data frame we can put the condition for the same in the data.frame[data.frame == 'value']  bracket notation alongside the data frame name.
#and this will return a vector of entries with the specified condition on the value, but the indexes are rearranged in ascending order.
# print(class(adult[adult == '?']))
#assign the desired value to these selected entries
adult[adult == '?'] <- NA
# print(anyNA(adult))
#when we use table() on a column with NA values, the NA values of those columns are not categorized under any factor name and thus not displayed.   
#to remove the '?' factor in type_employer column which now have a frequency count of 0, we can just re-factor the column by using the factor() function.  
adult$type_employer <- factor(adult$type_employer)
adult$marital <- factor(adult$marital)
adult$country <- factor(adult$country)
# print(table(adult$type_employer))


#plot missingness map
#missmap() function from Amelia package plots a heatmap for the missing values in the dataframe.
#the arguments y.at set to a vector with single numeric element 1 and y.labels set to a vector with an empty character element are used to remove the y axis labels in the missingness map.
# pl1 <- missmap(adult, main = 'Missingness Map', y.at = c(1), y.labels = c(''), col = c('Yellow','black'), legend = F)
# print(pl1)
#observation: there are not much missing values in this data set, relatively.


#there are missing values in occupation and type_employer columns, both these columns are factor data type and thus we
#cannot just do imputation by taking mean values. 
#in this problem we are going to omit the rows which have NA values, it is not a good option always to just remove the rows with missing values.
#use the na.omit() function to do so.
adult <- na.omit(adult)
# print(anyNA(adult))
# print(str(adult))
#observarion: around 2k rows are omitted which had missing values

#plot missingness map again to check all the rows with NA values were dropped correctly.
# pl2 <- missmap(adult, main = 'Check for dropping the NA values', col = c('Yellow','Black'), y.at = c(1), y.labels = c(''))
# print(pl2)

#Exploratory Data Analysis
#some errors show up while using Amelia and cause problems for ggplot plots, use the code below to fix it.
# dev.off()
# print(str(adult))
#plot histogram of ages colored by income
library(ggplot2)
#to get the >50k color appear on top of <=50k color use position argument in geometry layer and assign it to position_stack(reverse = T)
pl3 <- ggplot(adult, aes(x = age)) + geom_histogram(aes(fill = income), color = 'black', binwidth = 1, alpha = 0.4, position = position_stack(reverse = T)) + theme_bw()
# print(pl3)
#observation: most of the people earn <= 50k 
#very few people less than 25 yrs of age make >50k, then there is increase in
#number of people making >50k as the age increases from 25, also there is a drop in data points
#for older ages as people would retire as well and therefore there is a decrease in people
#making >50k as we go towards older ages.


#plot histogram of hours worked per week
pl4 <- ggplot(adult, aes(x = hr_per_week)) + geom_histogram() + theme_bw()
# print(pl4)
# observation: a lot of people work 40 hrs a week.
#we can do feature engineering here and make new factors such as < 40 hrs , = 40 hrs or > 40 hrs for this feature. But we will just continue.


#rename country column to region column using rename() in dplyr to better reflect the values for that column.
adult <- rename(adult, region = country)
# print(str(adult))

#plot a bar plot of region with fill color defined by income class.
pl5 <- ggplot(adult, aes(x = region)) + geom_bar(aes(fill = income), color = 'black', alpha = 0.5, position = position_stack(reverse = T)) + theme_bw()
# print(pl5)
#observation: most of the data is from north america. 
#North america has the most number of people earning > 50k

#build the model
#Logistic Regression is a type of classification model. 
#In classification models, we attempt to predict the outcome of categorical dependent variables, using one or more independent variables. 
#The independent variables can be either categorical or numerical.

# print(head(adult))

#split the train and test data set
library(caTools)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.7)
df.train <- filter(adult, sample == T)
# print(head(df.train))
# print(str(df.train))
df.test <- filter(adult, sample == F)
# print(head(df.test))
# print(str(df.test))

#use the glm() function which stands for generalized linear model for log R
#the formula notation is same as y ~ x, here we will use all the features in the data frame.
#set the family argument which is used to set the description of the error distribution. It is used to specify we are using 
#logistic regression for training our ML model, the log R uses cross entropy as the objective/error function and sigmoid function as the activation function.
#for log R set family to binomial(link = 'logit')
#there are other families we can use in glm() for different ML algo's.
#set the training data set using data argument

#train the Log R model on all the features in the training data.
model <- glm(income ~ ., family = binomial(link = 'logit'), data = df.train)
#warning showed after running the code for training the model means that the model may have guessed the probability of data point(s) belonging to a class label is either 0 or 1.
# in our case it is fine as we have already cleaned our data set.
#if the data set is big and this warning shows up then we should do a check for if not a lot of data points have a probability either 0 or 1 as we do not want a lot of noisy data for training our model, then we need to do more cleaning. 
#otherwise for a few instances it is fine.
# print(summary(model))

# observation: in Log R summary we have deviance residuals instead of just residuals as we have in linear regression model summary.
#we have new AIC value.
# There are a lot of features, to reduce the number of features to the one's which significantly add to the fit of the model we use function like step().
# The step() function removes the features in an iterative manner which do not add significantly to the fit of the model. 
# It makes use of AIC.
#The Akaike information criterion (AIC) is an estimator of the relative quality of statistical models for a given set of data.
#Given a collection of models for the data, AIC estimates the quality of each model, relative to each of the other models. 
#Thus, AIC provides a means for model selection.
#Step(): Stepwise algorithm for Choosing a model for a data set by AIC.
#Select a formula-based model by AIC given an initial model assigned to the object argument.

#create a new better model using stepwise algorithm(step() function) which makes use of AIC.
# new.model <- step(model)
#we get messages informing us of the process of building a better model.
# print(summary(new.model))
#observation: The step() using AIC returned a model which kept all the features as in the previous model.
#Other than AIC, there are other comparision criterias like variable inflation factor (VIF) that we can use with the help of vif() function as another crtiteria for comparing models.

#reducing the factors in the education and occupation columns might lead to a better model fit.


#predictions on test data set
df.test$prob.predictions <- predict(model, newdata = df.test, type = 'response')
# this model built is a rank deficient fit.
#Rank deficiency means there is insufficient information in the data to estimate the desired model.
#Causes: 
#Not enough data, Solution: get data as much as possible.
#Good amount of data but contains replicated points, this does not add information to the data. 
#Information in the wrong places in the data. All information is only along few features in the data. Do better sampling of data which provides good information.
#Bad choice of model to build. Some models require huge amount of data.
#Variables are different in units or are different in scales hugely by many orders of magnitude. TO avoid the maths to fail, get the variables within common units and scale them to be in similar magnitude.
#Not scaling the data properly, first shift the data to center and then scale it so that it has a mean of nearly 0 and sd of roughly 1.


#based on cutoff probability value predict the actual class labels from the predicted probabilities.
df.test$actual.predictions <- ifelse(df.test$prob.predictions > 0.5, '>50K', '<=50K')
#compute the accuracy
accuracy <- mean(df.test$actual.predictions == df.test$income)
print('-----------------Accuracy------------------------------')
print(accuracy * 100)
print('-----------------Missclass Error-----------------------')
print((1-accuracy) * 100)

#confusion matrix
confusion.matrix <- table(df.test$income, df.test$prob.predictions > 0.5)
print('----------------Confusion Matrix-----------------------')
print(confusion.matrix)

#I have taken >50K as True and <=50K as False.
#FNR
fpr <- 548 / (548 + 6372)
print('------------------FPR-----------------------------------')
print(fpr)Choose a model by AIC in a Stepwise Algorithm
Description
Select a formula-based model by AIC.

#Recall/TPR
recall <- 1423 / (1423 + 872)
print('-----------------Recall/TPR------------------------------')
print(recall)

#Precision
precision <- 1423 / (1423 + 548)
print('-------------------Precision------------------------------')
print(precision)

#Based on what is the cost associated with the model, our model's performance is judged on that factor.
#for example we might need a model with high accuracy or with high Recall or with high Precision or with low FPR depending on the domain we are working in.

