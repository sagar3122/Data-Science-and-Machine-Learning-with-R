#k means
#uci red and white wine data set
#load the csv files, use the sep argument as ';' instead of ','
df1 <- read.csv('winequality-red.csv', sep = ';')
df2 <- read.csv('winequality-white.csv', sep = ';')
#add 'label' column to both the data sets indicating 'red' and 'white' wine labels repectively.
# df1['label'] <- 'red'
# df2['label'] <- 'white'
#this can also be done using sapply() and a custom function which assigns a label value for the label column
#for every row it goes through in the data frame 
df1['label'] <- sapply(df1$pH, function(x){'red'})
df2['label'] <- sapply(df2$pH, function(x){'white'})
# print(head(df1))
# print(str(df1))
# print(head(df2))
# print(str(df2))

#combine df1 and df2 into a single data frame called wine
wine <- rbind(df1, df2)
# print(head(wine))
# print(tail(wine))
wine$label <- factor(wine$label)
# print(str(wine))
#randomize the rows of the data frame using sample_n() of dplyr
#for the no of rows argument pass the number of rows in wine data frame using nrow() function.
library(dplyr)
wine <- sample_n(wine, nrow(wine))
# print(head(wine))
# print(str(wine))
# print(head(wine$label))

#EDA
#there are very less red wine counts around 1.6k relative to white wine, if there is no strong differentiable 
#feature for red wines it will be hard to cluster them properly without making much mistakes.
#histogram of residual sugar from wine colored by label column
library(ggplot2)
pl1 <- ggplot(wine, aes(x = residual.sugar)) + geom_histogram(aes(fill = label), color = 'black', bins = 50) + theme_bw()
pl1 <- pl1 + scale_fill_manual(values = c('red','light yellow')) 
# print(pl1)
#observations: most of the wines either be red or white have a lower residual sugar amount.
#most of the wines which have residual sugar amount of 4 or more are white wines as compared to red wines.

#histogram of citric acid from wine data colored by label column
pl2 <- ggplot(wine, aes(x = citric.acid)) + geom_histogram(aes(fill = label), color = 'black', bins = 50) + theme_bw()
pl2 <- pl2 + scale_fill_manual(values = c('red','light yellow')) 
# print(pl2)
#observation: most of the wines have citric level equal to or less than 0.75
#for citric level less than 0.5 the count for red wines are almost same for all those citric levels.
#for citric level less than 0.5 the count for white wines form a normal distribution for all those citric levels, with it's peak between 0.25 and 0.3.
#a lot of white wines have a citric level between 0.25 and 0.3, this might be the case because we have a bigger sample for white wines
#as compared to red wines, there fore residual sugar is not a strong differentiable feature.


#histogram of alcohol from wine data colored by label column
pl3 <- ggplot(wine, aes(x = alcohol)) + geom_histogram(aes(fill = label), color = 'black', bins = 50) + theme_bw()
pl3 <- pl3 + scale_fill_manual(values = c('red','light yellow'))
# print(pl3)
#observations: the red and white wines have roughly the same proportion of wine counts for all the alcohol values starting from 9 to 14.
#for a alcohol value from around 8.5 to 8.9 it has more white wine counts than the red wine counts.
#here the plot has a trend of going up then down and then repeat because usually the alcohol percentage is in 0.5 units or whole number units.


#scatterplot of residual sugar(y) vs citric acid(x) colored by label column.
#for scatter plots while coloring the points based off the values of a column in the data set
#we can choose the colors for the values of that feature column using scale_color_manual() function and provide the vector
#of colors in the order you want to assign to the values whose order is displayed in the legend by assigning it to the values argument.
pl4 <- ggplot(wine, aes(x = citric.acid, y = residual.sugar)) + geom_point(aes(color = label), alpha = 0.2) + theme_dark()
pl4 <- pl4 + scale_color_manual(values = c('red','light yellow'))
# print(pl4)
# observation: most of the red wines have the citric values ranging from 0 to 0.75 roughly dense in the lower and higher values as compared to middle values in terms of counts.
#most of the white wines have citric values ranging from 0.2 to 0.5
#the white wines have higher residual sugar values as compared to red wines.
#there is one outlier for white wines with residual sugar greater than 60, we can remove it to relabel it for the residual sugar measure.
#for the citric values between 0.2 and 0.5, the clustering model will have a hard time clustering since there is a lot of overlap between red and white wines.


#scatter plot of volatile acidity vs residual sugar colored by label column.
pl5 <- ggplot(wine, aes(x = volatile.acidity, y = residual.sugar)) + geom_point(aes(color = label), alpha = 0.1) + theme_dark()
pl5 <- pl5 + scale_color_manual(values = c('red','light yellow'))
# print(pl5)
#observation: most of the red wines have the volatile acidity ranging from 0.4 to 0.8
#few red wines have volatile acidity higher than that as well.
#very few red wines have volatile acidity higher than 1.2
#most of the white wines have volatile acidity between 0.1 to 0.4
#few white wines have higher than that as well.
#as seen earlier the white wines have higher residual sugar as compared to red wines in generall.
#volatile acidity could be a helpful feature in clustering since most white wines
#have range between 0.1 to 0.4 and most red wines have range between 0.4 to 0.8
#there are few red wines below 0.4 and some white wines above 0.4 so there will be some mistakes made.
#some wines are mixture of red and white wines called rose wines.


#remove the label column
clus.data <- wine[1:12]
# print(head(clus.data))
# print(str(clus.data))

#build the k means model
#build the clusters
#here we know k has to be 2
#keep nstart to be 20
wine.cluster <- kmeans(clus.data, centers = 2, nstart = 20)
#print the summary
# print(wine.cluster)
#observations: the no of clusters and no of data points assigned to those clusters are provided
#mean value for each feature for the data points is provided for each cluster, these values are actualy the centers for those clusters.
#these center values can also be called using the 'centers' keyword used after the '$' symbol notation.
# print(wine.cluster$centers)
#here in this problem the data is in 12 dimensions, therefore these center values have 12 coordinates, 
#each coordinate is the mean value of a feature for the data points in the corresponding cluster.
#the clustering vector specifying the cluster number assigned to each data point is provided.
#sse values for each cluster is provided
#components of the built k means model object are provided to be called and used.


#evaluate the clusters using the actual labels
#performance of clustering using confusion matrix
confusion.matrix <- table(wine$label, wine.cluster$cluster)
# print(confusion.matrix)

#here 1 is red wine and 2 is white wine 
#observations
#the red wines are easier to correctly group based off their feature values as there are only 85 instances
#of red wines which are not correctly grouped and there are a 1294 instances of white wines which are not correctly grouped as
#white wines.
#the volatile acidity feature played a good part in separating the red wine instances from the white wine.
#that could have been a strong feature in representing the variablility in the data.
#we observed a lot of noise in the white wines that could have been there because they are actually rose
#wines and are labelled as white wines, which actually made difficult for the model to cluster them properly based off their feature values
#as they are matching closely with those of red wines.
#since we are only using just some chemical measurements to tell the color of the wine all of which are made of fermented grape juice,
#makes sense as this does not provide good correlation between the wine and it's color to be fully white or fully red.
#leading for our model to make mistakes on white wines as they chemically look similar to red wines.
