#k means algo
#iris data set
#we will not provide the actual labels we have since k means create clusters by putting similar data points together
# based on the k value provided and the features of the samples in the data set.
#we will use the actual labels to evaluate the clusters created by k means algo.
library(ISLR)
# print(head(iris))

#plot a scatter plot of the data to visualize what to expect when using k means algo to create clusters.
library(ggplot2)
#use petal length for x axis and petal width for y axis
pl1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point(size = 4) + theme_bw()
# print(pl1)

#observations:
#based off the petal dimensions, k means will have the easiest time to cluster setosa.
#It might have a little hard time to cluster versicolor and virginica as there is some sort of overlapping
#between the data points for the two classes. It might make some clustering mistakes.


#implement k means clustering algo
#since k means randomly assigns the centers for the k clusters or points to the k clusters initially, there fore to get the same clusters
#set a seed value.
set.seed(101)
#the kmeans() function is in built in the stats package of R.
#assign the data with only predictor features in the first argument 'x', 'centers' second argument specifies the k value(no of centers/clusters),
#as we already know there are 3 classes, assign k = 3 , but this is not at all provided and is usually determined using domain expertise or methods such as elbow method.
#nstart argument is used to specify the number of random sets for the center values we would want. assign this argument as 20.
#assumption: all these sets of random values for centers are used to create separate k means models and the model which returns the
#best sse values for the clusters is chosen.
iris.cluster <- kmeans(iris[,1:4], centers = 3, nstart = 20)
#print the kmeans model name to get the summary.
# print(iris.cluster)
#observations: number of clusters and number data points in them are provided.
#for each cluster the mean value for each feature for the data points in that cluster is provided.
#clustering vector tells the cluster number to which each sample is assigned.
#SSE for each cluster is provided.
#available list of components that can be called from the trained k means model object is also provided.


#in this case the clusters formed by k means can be evaluated using the actual labels.
#to get the clustering vector from the k means model object use the 'cluster' keyword after '$' symbol.
#clustering vector tells the cluster number to which each sample is assigned.
#print(iris.cluster$cluster)

#performance 
#compute the confusion matrix
# print(iris$Species)
confusion.matrix <- table(iris.cluster$cluster, iris$Species)
#print(confusion.matrix)
#observation: some mistakes have been made while clustering
#the clusters formed are labelled with numbers, but we can figure out as we know the actual label names.  
#there is no mistake while clustering setosa class.
#there are 14 mistake while clustering versicolor class.
#there are 2 mistakes while clustering virginica class.

#cluster visualization
#we can plot out the clusters formed by the k means model on 2 of the features in the data set.
#to do so use the cluster library.
library(cluster)
#use the function called clusplot(), it is used to draw the clusters formed by the k means model on a 2D plane
#using 2 features in the data set. This function also has a generic method of partitioning.
#the first argument is used to assign the data set with the actual labels, this is used to get the values for the 2 features(chosen by clusplot() which explains the most variability) which are used to draw the points out on the plane.
# the second argument is used to assign the cluster vector formed while clustering using k means
#which contains the clusters assigned to the data points in the data set by the k means model.
#the above argument is used to assign cluster numbers to each data point drawn on the plane using various shapes.
#for formatting the plot assign color argument as T, shade argument as T, labels argument as 0 and lines argument as 0 as well.
pl2 <- clusplot(iris, iris.cluster$cluster, color = T, shade = T, labels = 0 ,lines = 0)
# print(pl2)

#observations: clusters are plotted vs the 2 features(components) which explain the most variability in the data.
#variability means the difference in the feature values of the data points belonging to different clusters.
#the setosa cluster is seperately plotted as expected, 
#also there is overlap between the clusters for versicolor and virginica classes.

#call help on clusplot to know more; it is basically used for bivariate cluster plot which can also be done using ggplot.
#this is not very useful when our data has a lot of features because this will only plot for 2 features in a 2D plane.
#for example we have a data set with 50 features and we use clusplot to plot the clusters in the data based off two features
#then that plot will only explain around 20% of the variability in the data.
#also high dimensional data can not be visualized correctly in a 2D plane based off two features.
#you can not observe clearly how the data points belonging to different clusters are actually separated. Example is a non linearly separable data. Need more features to see the separation in the clusters.
#clusplot tries to plot against 2 features in the data set which explains the most variability among the data points in the data set.

#jose did not do elbow method which invloves to compute the sse for each model built.
