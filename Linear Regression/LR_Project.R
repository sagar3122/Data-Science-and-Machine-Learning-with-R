#read the csv file
bike <- read.csv('bikeshare.csv')
# print(head(bike))
# print(tail(bike))
#we want to predict based on the predictor features('various info about the day/season/temp') in the data set the value for the target feature(the count of bike rented) for that time slot.    

#exploratory data analysis
library(ggplot2)
#scatter plot count vs temp
pl <- ggplot(bike, aes(x = temp, y = count)) + geom_point(alpha = 0.1, aes(color = temp)) + theme_bw()
# print(pl)
#we can say as there is an increase in temp there is an increase in count value of bikes rented.
#Also, there are a lot of small count values for bike rented at lower temp.

#convert the datetime column values from character/factor data type to timestamp data type
# print(str(bike))
# print(summary(bike))
#as.POSTIXct() does not need to be applied to the column values using apply(), we can do it directly.
bike$datetime <- as.POSIXct(bike$datetime)
# print(str(bike))
# print(head(bike))
# print(class(bike$datetime))

#scatter plot count vs datetime with coloring of points done using the temp feature
pl1 <- ggplot(bike, aes(x = datetime, y = count)) + geom_point(aes(color = temp), alpha = 0.1)
#to add a color gradient we use scale_color_gradient() function with low and high color values/you can use scale_color_continuous() as well.
pl2 <- pl1 + scale_color_gradient(low = 'green', high = 'red') + theme_bw()
# print(pl2)
# we can observe the seasonality in temp on the datetime axis. Alongside this seasonality, there is a seasonality in 
#count of bikes rented during the summer and winter seasons of a year, with the count at winter smaller than the count at the summer.
# Also there is an increasing trend in count of bikes over the years.
#the data here is non linear
#LR is not favored for non linear data.

#correlation between temp and count
#pass in the two vectors as arguments to the cor() function
# cor.value <- cor(bike$temp, bike$count)
#or pass all the rows with a vector of just those 2 vector names of the bike data frame.
cor.value <- cor(bike[,c('temp','count')])
# print(cor.value)

#uptil now we have seen that with the increase in the temp value there is an increase in the count value for rentals. 

pl3 <- ggplot(bike, aes(x = factor(season), y = count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()
# print(pl3)
#we see there is seasonality in the data for the target variable values vs the season feature. rental Counts is high for summer and fall.
#the data is non linear and LR is not a good choice for it.
#the count of rentals is more in winter season rather than the spring season as there is an increase in rental counts over the years.
#The increasing trend sets the winter season with more rental counts than the spring season. Even though we saw rental counts get higher as temp increase.

#these increasing trend and seasonality makes the data non linear and not a good fit for performing LR.

#feature engineering
#extract hour from datetime
#use format() function to extract the hour from the timestamp object, 
#pass the timestamp object and the timestamp feature to extract as the arguments.
bike$hour <- sapply(bike$datetime, function(x){format(x, '%H')})
# print(head(bike))


#scatter plot of count vs hour
#use bike data where working day == 1
#use dplyr
library(dplyr)
bike.wd.1 <- filter(bike, workingday == 1)
#filter returns a data frame
# print(head(bike.wd.1))
# print(class(bike.wd.1))
#to fill in the gap between the points on the hour axis, we can use position_jitter() function to reposition the points with w = 1 and h = 0 arguments passed to the function which itself is assigned to the position argument in the geometry layer.
#these arguments w = 1 and h = 0 mean we are adding jitter along the width(x axis) but not along the height(y axis). 
pl4 <- ggplot(bike.wd.1, aes(x = hour, y = count)) + geom_point(aes(color = temp), position = position_jitter(w = 1, h = 0), alpha = 0.5)
#the scale_color_grandientn() provides multiple colors to the plot based on the values of the feature chosen for filling the color for the plot to create better interpretability.
#pass the vector of color names as the argument which are used in the order as they are provided for coloring the plot as we go from lower values to higher values of the feature chosen for coloring the plot.
pl5 <- pl4 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red')) + theme_bw()
# print(pl5)
#we observe that the rental count is high for the office hours in the morning and highest for the office hours in the evening.
#this seasonality is present in the data regardless of temp value. but the higher temps have relative higher values of rental counts. 


#we do the same above plot but for the non working days
bike.wd.0 <- filter(bike, workingday == 0)
#filter returns a data frame
# print(head(bike.wd.1))
# print(class(bike.wd.1))
#to fill in the gap between the points on the hour axis, we can use position_jitter() function to reposition the points with w = 1 and h = 0 arguments passed to the function which itself is assigned to the position argument in the geometry layer.
#these arguments w = 1 and h = 0 mean we are adding jitter along the width(x axis) but not along the height(y axis). 
pl4 <- ggplot(bike.wd.0, aes(x = hour, y = count)) + geom_point(aes(color = temp), position = position_jitter(w = 1, h = 0), alpha = 0.5)
#the scale_color_grandientn() provides multiple colors to the plot based on the values of the feature chosen for filling the color for the plot to create better interpretability.
#pass the vector of color names as the argument which are used in the order as they are provided for coloring the plot as we go from lower values to higher values of the feature chosen for coloring the plot.
pl5 <- pl4 + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red')) + theme_bw()
# print(pl5)
#we observe that the rental counts are high through out the day time vs night time, it forms normal distribution with a peak around 2-4 pm.
#this seasonality is present in the data regardless of temp value. but the higher temps have relative higher values of rental counts. 

#prepare the model
#convert hour column from character to numeric because we are going to use to train the model
# print(class(bike$hour))
#use sapply() and as.numeric()
bike$hour <- sapply(bike$hour, as.numeric)
# print(class(bike$hour))
#split for train and test data frames
library(caTools)
#random split using sample.split() of caTools
#since the data is time series data as it has trend and seasonality, the random splitting for train and test data set is the worst choice
#in random split the train data set might get some observations from the end of the data set, in that case it is the same as predicting 
#present values using the future values instead learning from the past values which actually is the case in the real world.
#also the LR model does not perform good on time series data.
#it might perform a little better with sequential splitting but still LR is not a good fit for time series data.
sample <- sample.split(bike$count, SplitRatio = 0.7)
train <- filter(bike, sample == T)
# print(head(train))
# print(str(train))
test <- filter(bike, sample == F)
# print(head(test))
# print(str(test))

#sequential split
#here we are splitting the bike data set based on the datetime values using the filter() and passing the data frame name and the condition on the datetime values using the as.POSIXct() as the arguments.
train.df <- filter(bike, datetime < as.POSIXct('2012-05-01'))
# print(str(train.df))
# print(tail(train.df))
test.df <- filter(bike, datetime >= as.POSIXct('2012-05-01'))
# print(str(test.df))
# print(head(test.df))

# using single predictor variable -> temp
temp.model <- lm(count~temp, data = train)
# print(summary(temp.model))
#the slope value for intercept is the y value when x = 0 for the regression line. 
#in this case where we are using the temp variable for predicting the rental counts as in the formula for training the LR model, this slope value for intercept is the rental count when the temp is 0.
#slope for intercept is beta 0. it is not good to interpret the slope for intercept alone. We will interpret the slope for temp as well, beta 1.
#the slope for temp is change in y divided by change in x i.e. the slope. this means keeping other variables same, the 1 degree increase in temp is equivalent to a increase of 9.17 in rental count.
#this is not a statement of causation, otherwise the beta 1 value would be negative if increase in temp was equivalent to decrease in rental. this beta 1 value just explains the relation between increase in temp and associated increase in rental count.
#these coefficient values for the variables are actually the slope values for the regression line computed during training, 
#the slope values of the significant variables based off the p values are used along side the input values
#for those variables(predictor variables) to predict the values for the target variables.  

#plot the residuals using residuals()
res <- residuals(temp.model)
# print(str(res))
#convert to dataframe
res <- as.data.frame(res)
# print(class(res))
# print(head(res))
#plot the histogram for residuals
pl6 <- ggplot(res, aes(x = res)) + geom_histogram(bins = 20, fill = 'blue', alpha = 0.5) + theme_bw()
# print(pl6)
#observation: 
#this LR model is not going to perform good on this time series data as the plot for residual values is not a normal distribution.

# predict the values
test$predicted <- predict(temp.model, newdata = test)
# print(head(test))
# predict for temp = 25
# using predict function
# temp <- data.frame(temp = 25)
# print(temp)
# result <- predict(temp.model, newdata = temp)
# print(result)
#using the slope value for intercept and temp from the summary
#formula is intercept slope + temp slope * (temp value we want to compute for)
# result <- 8.4324 +  9.1164 * 25
# print(result)
#mse
mse <- mean((test$count - test$predicted) ^ 2)
# print(mse)
#rmse
rmse <- mse ^ 0.5
# print(rmse)
#R Squared Score
#sse
sse <- sum((test$count - test$predicted) ^ 2)
#sst
sst <- sum((mean(bike$count) - test$count) ^ 2)
r.squared <- 1 - sse/sst
# print(r.squared)

#prepare the model using all the features except  casual, registered, datetime, atemp in data set to use as predictor features for the target feature.
model <- lm(count~. - casual - registered - datetime - atemp, data = train.df)
# print(summary(model))

#predict the values
test.df$predicted <- predict(model, newdata = test.df)
# print(head(test))

#mse
mse <- mean((test.df$count - test.df$predicted) ^ 2)
print(mse)
#rmse
rmse <- mse ^ 0.5
print(rmse)
#R Squared Score
#sse
sse <- sum((test.df$count - test.df$predicted) ^ 2) 
#sst
sst <- sum((mean(bike$count) - train$count) ^ 2)
r.squared <- 1 - sse/sst
print(r.squared)

#when using all the features except casual, registered, datetime, atemp in data set for training the model
#Earlier with random train and test, the model has a R squared score of around 0.3
#After using the sequential train and test split which is used for dealing with time series data, still the R squared score bumped up only upto around 0.4.
#this proves LR is not a good fit for time series data, this data has seasonality and increasing trend which accounts for higher total rental count for winter relative to spring season
#even though we have seen a seasonality of having higher rental counts in summer relative to winter for a year.


#regression forests are a better approach to time series data.


