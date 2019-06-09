# In regression problems we predict continuous values for a target variable.
#the slope/coefficient values learnt in LR for all the variables are similar to the weights learnt for the corresponding variables in log R or a neural net.
#The learning part in LR while training the model is learning the relation between the values of the input predictor
#variables and the values of the output target variable. This also includes learning the predictor variables which have
#a significant impact on predicting the values for the target variable and using those predictor variables while making 
#predictions on the test data set.  

#Linear Regression (LR)
#to regress is to drift towards the mean value for that variable usually described for the output variable given input variables.
#regression is a measure of relation between the mean value of the output variable and corresponding values of input variables.(this is same as saying depending on the values of the input variables what is the mean value for the output variable going to look like) 
#In Regression we achieve this by drawing a line which is as close to the points(correct values of output variables given input variables) on the graph as possible.
#In LR that line is a straight(linear) line.
#In least squares method/classic LR we measure the closeness of the drawn line to the points in only up and down direction. Our goal is to minimize this distance between all data points and the line.
#the best line is the one which has the minimum distance to all the data points.
#methods such as sum of squared errors/sum of absolute errors etc are used to minimize the distance between all the points and the line.
#After drawing this LR line on the given data points(done with the training part), we can then predict the value for the output variable given the values for the input variables.



#formulas in R have the general form of y ~ x.
#for LR, the y part is for the target variable, the x part is for the predictor variables which are usually multiple in number. To add predictor variables use the + sign.

#syntax in R for LR or general any ML model

#training a model
# model <- lm(y ~ x, data = df)
#here model variable stores the trained model based on the ML algo used, this is an object of the type of ML model trained.
#here we use lm() function for training a LR model(stands for LR model), this will be replaced for whichever ML model we want to train based on a ML algo.
#then we have the target and predictor variables represented using the y ~ x notation.
#the data set used for training is mentioned using the data argument.

#predicting from a model
# dtest$target.feature.column.name <- predict(model, newdata = dtest)
#use the predict function to make predictions for a data set given a trained ML model(in this case it is the LR model)
#use the trained ML model now stored in an object created under the variable name model.
#specify the data sets either the test or train data sets for which we want to predict the target variable for using the new data argument
#store the predicted value for the target variable in a new column of the data set used for prediction.


#load student performance data set
#to separate strings based on symbols other strings than ',' use sep argument in read.csv()
df <- read.csv('student-mat.csv', sep = ';')
# print(head(df))
# print(summary(df))
# print(str(df))
#check for NA (null) values in the data frame
# print(is.na(df))
#any() returns true if there is any true value returned by is.na(df) for any of the cells in the df, otherwise False.
# print(any(is.na(df)))
#check for all the features which are supposed to be factor variables with levels, are present as mentioned using the str().
#the medu and fedu are categotical/factor in nature but represented as integer(numerical) with a min and max value of 0 and 4.
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
#exploratory data analysis
#Creating visualizations is very important as we might be getting good performance with our model after training it. 
#But that could be by chance as well and we might have some perception of  the data but that could be totally wrong. 
#We get to know how our data looks like and get more insights about our data after doing visualizations.
#Visualizing the correlation between the features is highly important because this will help us understand and verify 
#which of the predictor variables have a significant impact on predicting the values for target variable when we interpret 
#the summary of the trained model basically the p values and the significance stars of the predictor variables.  
#create plots to explore data, understand correlation between features. 
#Correlation is relationship between features(2 usually) of the data points in the data set. Though in common usage it most often refers to the extent
#to which two variables have a linear relationship with each other.
#In statistics, dependence or association is any statistical relationship, whether causal or not, between two random variables or two sets of data.
#Correlation is any of a broad class of statistical relationships involving dependence, though in common usage it most often refers to the extent
#to which two variables have a linear relationship with each other. Familiar examples of dependent phenomena include the correlation between the 
#physical statures of parents and their offspring, and the correlation between the demand for a product and its price.

#Correlation plots are a great way of exploring data and seeing if there are any interaction terms.
#Let's start off by just grabbing the numeric data (we can't see correlation for categorical data):

#numeric column only
#use sapply() to apply is.numeric() to each column of df and store the logical results in a vector.
logical.numeric.cols <- sapply(df, is.numeric)
# print(logical.numeric.cols)
# print(class(logical.numeric.cols))

#filter the numeric columns for correlation
#use the cor() function from stats in R which is used to grab correlation values between columns(numeric in nature)
#this cor() function takes in the data frame with all the numeric columns and returns a data frame representing the 
#correlation values between the those numeric columns of the passed data frame.
cor.data <- cor(df[,logical.numeric.cols])
# print(cor.data)
#value of 1 defines a perfect positive linear correlation.

#plot these correlation values using third party packages.
#corrgram and are corrplot are very good packages for creating correlation plots/diagrams.
# install.packages('corrgram')
# install.packages('corrplot')

#pass the correlation data(cor.data) to corrplot() to create a correlation plot
#pass in additional argument, method = 'color' to do a color coded correlation plot.
#this visualization helps to easily understand the correlation values between the columns.
# print(corrplot(cor.data, method = 'color'))

#the dark blue diagonal represents a perfect correlation which makes sense as each feature is perfectly correlated with itself.
#the grades are highly correlated with each other bcoz if you do well on exam 1 then you are likely to do well on exam 2 and final exam.
#Similary for exam2 with other exams and final exam with other exams.
#this also holds if you are doing bad on your exams.

#we can see having good grades is inversely correlated to failures(past class failures) which also makes sense.
#mother education and father education are highly correlated as mostly couples pair up having similar education levels.


#use the corrgram(); corrplot required a data frame of correlation values between the numeric columns in the data set. In corrgram we can pass the 
#dataframe directly.
#if we just pass the data frame we will get the same output as color coded corrplot but without any labels/bars to fully understand the correlation values between the columns, in the plot. 
#the correlation plot made by corrgram is called a correlogram.
# print(corrgram(df))
#we can pass a bunch of more arguments to get a better more insightful correlation plot from the data frame without any additional filtering for correlation values of the numerical columns in the data set.
#order argument when set to true does the PCA based ordering of the features.
#set lower panel, upper panel and text panel arguments as well
#we specify the lower panel is the regular shaded box depicting the correlation between the columns
#we specify the upper panel is the pie chart depicting the correlation between the columns
#A full blue colored pie chart denotes a perfect correlation that is number 1, and the shades in the pie chart 
#depicts the type of correlation and the amount the pie is filled depicts how strong the correlation is between the columns.
#the blue is for positive correlation, the red is for inverse correlation.
# print(corrgram(df, order = T, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt))

#predict g3 scores using LR
#lets do a plot first to get more insights about it.
#build a histogram
pl <- ggplot(df, aes(x = G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# print(pl)

#we observe a lot of 0 values and also the most frequent value(mean value) is 10 which might be a result of curving.


#building the LR model
#split the data into train and test data.
#use the caTools package to make a random split for the training and testing set
# install.packages('caTools')
#set a seed using set.seed() to get the same random numbers/rows in the training data set and test data set after splitting
#so that jose and I will get the same outputs as we go along building the model.
set.seed(101)
#split the data frame randomly into train and test using the sample.split() function which splits the data from a vector
#into 2 sets based on a defined ratio. This is done by randomly assigning the logical values based on the specified split ratio to all the rows in the data set by creating a new column. 
#this random assignment of the logical values is controlled in our case by setting a seed value first. this is actually psuedo random as it will always yield in the same rows assigned the same logical value because of the seed
#even though the process is random.
#the actual split is made using the newly created logical column and the subset() and take the samples/rows with the similar logical values assigned, to store them as train and test data sets. 
#we need to choose a column on which we will split the data frame(assign the logical values to the rows), by convention choose the target variable column.
#use the splitratio argument to define the ratio/percentage of the data we will use for training data set.
sample <- sample.split(df$G3, SplitRatio = 0.7)
#this step actually creates a new column in the dataframe using random logical values with 70% of those being True, using this logical column
#we can split the data frame into training and testing data sets.
#70% data -> train
train <- subset(df, sample == T)
# print(train)
# print(str(train))
#30% data -> test
test <- subset(df, sample == F)
# print(test)

# The general model of building a linear regression model in R looks like this:
# model <- lm(y ~ x1 + x2,data)
# or to use all the features in your data
# model <- lm(y ~. , data) # Uses all features


#here model variable stores the trained LR model, this is an object of the trained LR model type.
#here we use lm() function for training a LR model(stands for LR model).
#then we have the target and predictor variables represented using the y ~ x notation.
#we are using all the features to predict the target variable.
#the data set used for training is mentioned using the data argument.
model <- lm(G3~.,data = train)

#interpret the model
# print(summary(model))
# print(str(model))
#Observations
#the call line tells the formula used to create the LR model.
#the residuals are the difference between the correct values of the target variable and the predicted values of the regression carried out for the target variable.
#the next line shows the statistical summary of the residuals.
#for regression we usually want our residuals to look like normal distribution when plotted.
#If our residuals are normally distributed, this indicates the mean of the difference between 
#our predictions and the correct values is close to 0 (good) and that when we miss, we're missing 
#both short and long of the correct value(this means the model will miss the correct value by small margin and by large margin evenly), { and the likelihood of a miss being far from the correct
#value gets smaller as the distance from the correct value gets larger.} don't understand the points in the curly brackets.
#residuals tell us how good our model is. Analogy for what residual tell us for a good model:(Dart Board analogy)
#A good model hits the bullseye some of the times, when it does not it misses closer to the bullseye and misses the buckets evenly not just hitting one bucket all the time.  

#Significance Stars:	The stars are shorthand for significance levels, with the number of asterisks displayed according to the p-value computed. *** for high significance and * for low significance. 
#In this case, *** indicates that it's unlikely that no relationship exists b/w absences(predictor variable) and G3 scores(target variable).

#The estimated coefficient is the value of slope calculated by the regression. It might seem a little confusing that the Intercept also has a value, but just think of it as a slope that is always multiplied by 1. 
#This number(slope) will obviously vary based on the magnitude of the variable(predictor variable) you're inputting into the regression, for example predicting the housing prices will vary based on the value put in for the square feet
#size of the house and the number of rooms in the house. The slope computed in this case should be normalized to be interpreted and compared with other normalized values computed for slope.
#but it's always good to spot check this number(slope) to make sure it seems reasonable.


#Learning these slope value (coefficient value) for all variables is one of the goals while training the model. And based on the  p values we learnt,
#we choose the most significant predictor variables and use the corresponding slope values to input into the regression and then make predictions off them for target 
#variable when provided with the input values of the significant predictor variables in the test data.  


# Standard Error of the Coefficient Estimate: Measure of the variability in the estimate for the coefficient. Lower means better but this number is 
#relative to the value of the coefficient. As a rule of thumb, you'd like this value to be at least an order of magnitude less than the coefficient estimate.
#but in our case it is not and we will see that it is not a big deal.

#t-value of the Coefficient Estimate:	Score that measures whether or not the coefficient for this variable is meaningful for the model. 
#You probably won't use this value itself, but know that it is used to calculate the p-value and the significance levels of the variables.

#Variable p-value:	Probability the variable is NOT relevant. You want this number to be as small as possible. 
#If the number is really small, R will display it in scientific notation.

#Significance Legend	The more punctuation there is next to your variables, the better.
#Blank=bad, Dots=pretty good, Stars=good, More Stars=very good
#therefore the P values for the variables are of super importance and help us determine variables which have significant
#impact on predicting the values for the target variable.
#In this case, *** indicates that it's unlikely that no relationship exists b/w absences(predictor variable) and G3 scores(target variable).
#the similar is the case with G2 scores(predictor variable) and G3 scores(target variable). These significance levels makes sense as absenses and G2 score will have the most impact on predicting the G3 scores, intuitively.


#R-squared:	Metric for evaluating the goodness of fit of your model. Higher is better with 1 being the best. 
#R squared score tells how good of a fit is the trained LR model for the data we are working on.
#Corresponds with the amount of variability in what you're predicting(target variable) that is explained by the model. 
#it just basically explains the variance in the values predicted for the target variable by the model.
#WARNING: While a high R-squared indicates good correlation(between the predictor variables and target variable based on the significance stars), correlation does not always imply causation(causation means a feature in the data set causes another feature to occur/exist).
#How much the predicted values vary from each other for all the test data observations or how much each prediction vary from the correct value for each observation?

#Residual Std Error / Degrees of Freedom	:The Residual Std Error is just the standard deviation of your residuals. 
#You'd like this number to be proportional to the quantiles of the residuals. For a normal distribution, 
#the 1st and 3rd quantiles should be 1.5 +/- the std error. 

#The Degrees of Freedom is the difference between the number of observations included in your training sample and the 
#number of variables used in your model (intercept counts as a variable).


#F-statistic & resulting p-value:	Performs an F-test on the model. This takes the parameters of our model (in our case we only have 1) and compares it to a model that has fewer parameters.
#In theory the model with more parameters should fit better. If the model with more parameters (your model) doesn't perform better than the model with fewer parameters, 
#the F-test will have a high p-value (probability NOT significant boost). If the model with more parameters is better than the model with fewer parameters, you will have a lower p-value.

#The DF, or degrees of freedom, pertains to how many variables are in the model. In our case there is one variable so there is one degree of freedom.


#visualize the model by plotting the residuals.
#collect the residuals of the model using the residuals() function, it returns a numeric vector
res <- residuals(model)
# print(class(res))
# print(str(res))
#there are 277 residual values equal in number to the 277 obs of the training data set used to train the model.
#now to plot the residuals using ggplot2 we need to convert the vector in a data frame.
#use the as.data.frame() function and pass the vector as the first argument.
res <- as.data.frame(res)
# print(head(res))
#the residuals are the difference between the correct values of the target variable and the predicted values of the regression carried out for the target variable(the regression line we drew).
#to build a better model we try to minimize these residual values.
#plot a histogram for the residuals to check the distribution of the residuals for our model.
#according to the dart board analogy for a good model the residuals should be normally distributed.
pl <- ggplot(res, aes(x = res)) + geom_histogram(fill = 'blue', alpha = 0.5)
# print(pl)
#If our residuals are normally distributed, this indicates the mean of the difference between 
#our predictions and the correct values is close to 0 (good) and that when we miss, we're missing 
#both short and long of the correct value(this means the model will miss the correct value by small margin and by large margin evenly), { and the likelihood of a miss being far from the correct
#value gets smaller as the distance from the correct value gets larger.} don't understand the points in the curly brackets.



#We want a histogram of our residuals to be normally distributed, something with a strong bimodal distribution 
#may be a warning that our data was not a good fit for linear regression. 
#{However, this(that our data is not a good fit for LR) can also be hidden from our model. A famous example is Anscombe's Quartet.
#(where there are four different data sets which have the same statistical info but plots on graphs are totally different and only one of them is suitable for performing LR).} I don't know if my explanation is correct.

#Even though some data sets have similar statistics info not all of them would be suitable for a LR model.
#There fore we need to visualize the data sets every time and check if they are suitable for a LR model by plotting the residual values and computing the R squared score. 

#we observe in our case the residuals with large negative values have a significant count. this is becasue our model is predicting 
# negative scores of G3 exam for students who  have performed very badly earlier on and based on other significant predictor variables 
#even though the least value someone can get is a 0.
# we will see how to deal with this as we go ahead.

#advance visualization of the model
#We can further explore this trained model by just calling plot on our model.
#What these plots represent is covered in ISLR, as well as the Wikipedia page on Regression Validation.
#use the plot() function and pass the trained model as the first argument.
# print(plot(model))
#we will get 4 advance residual plots of the trained LR model, namely:
#residuals vs fitted values
#normal Q-Q
#scale location
#residuals vs leverage plot

#Basically after looking at these plots what you will realize is that our model (behaving as a continuous line), predicted students would get negative scores on their test! 
#Let's make these all zeros when running our results against our predictions.

#the performance of the model is not measured by just interpreting the summary of the model, 
#performance of the model is measured mainly off the predictions made on the test set. 
#predictions for the test set
#use the predict() function from the built in stats package of R. Pass the model in the first argument and the test data set for which we want to make predictions in the second argument.
#the predict function knows what variable to predict and  use which of the other variables for making the predictions as everything is stored in the trained model object named model(the whole formula).
#the only point to make sure is quantity and formatting of columns of the test data set w.r.t the column names and types of data set used for training. They should be exactly the same. 
#The quantity of the observations and values in the cells of the data set could very well be different.  
G3.predictions <- predict(model, test)
# print(str(G3.predictions))
#118 predictions made same as the number of obs in the test data set.
#compute the RMSE value for the predictions made vs the correct values of the target variable for all observation in the test data set
#to do so we will create a data frame with the columns for predicted values and the correct values of the target variable for the test data set.
#use the cbind() function and pass the predicted values and the correct values as the two columns to be combined and stored in result variable which is a matrix.
results <- cbind(G3.predictions, test$G3)
# print(class(results))
#rename the columns of the result matrix using colnames() function by passing the matrix and assigning a character vector of new column names in the same order as they exist in the matrix.
colnames(results) <- c('predictions','correct values')
#convert the result matrix into a dataframe
results <- as.data.frame(results)
# print(head(results))
# print(min(results$predictions))
# print(class(results))

#we observe in our case the residuals with large negative values have a significant count. this is because our model is predicting 
#negative scores of G3 exam for students who  have performed very badly earlier on and based on other significant predictor variables 
#even though the least value someone can get is a 0.
# we will now replace the negative predictions with 0.
#create a custom function and apply it to all the values in the prediction column by passing that custom function into the
#sapply() function to take care of the negative predictions.

to_zero <- function(x){
  if (x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$predictions <- sapply(results$predictions, to_zero)
# print(head(results))
#now if we check the min value in predictions column we get a 0.
# print(min(results$predictions))

#to check the performance of the model on the test data set, we can compute several performance measures. 
#the mean squared error and root mean squared error tell us how off the model is in predicting the values same as the correct values of the target variable for the observations in the test data set.

#MSE
mse <- mean((results$`correct values` - results$predictions) ^ 2)
# print('MSE')
# print(mse)

#RMSE
#taking the squared root of mse
rmse <- mse ^ 0.5
# print('RMSE')
# print(rmse)

#a more intuitive performance measure is the R squared value for the model, that tells how well the model fits the predicted values(how good of a job the model has done while making predictions for the test data set).
#R-squared:	Metric for evaluating the goodness of fit of your model. Higher is better with 1 being the best. 
#Corresponds with the amount of variability in what you're predicting(target variable) that is explained by the model. 
#it just basically explains the variance in the values predicted for the target variable by the model.
#WARNING: While a high R-squared indicates good correlation(between the predictor variables and target variable based on the significance stars), correlation does not always imply causation(causation means a feature in the data set causes another feature to occur/exist).

#the formula for R squared value is 1- SSE(sum of squared errors) / SST(sum of squared totals)
#SSE
SSE <- sum((results$`correct values` - results$predictions) ^ 2)
#SST
#the sst is summation over the squared difference between the mean of correct values of the target variable for all the observations in the complete data set and the correct values of each observation in the test data set.
SST <- sum((mean(df$G3) - results$`correct values`) ^ 2)

#R Squared measure
R2 <- 1 - SSE / SST
print('R2')
print(R2)
#R squared value explains there is around 80 % variance in the values predicted for the target variable by the model. The higher the percentage the better the performance of model.