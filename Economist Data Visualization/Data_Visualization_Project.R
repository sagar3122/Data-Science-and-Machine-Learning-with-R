library(ggplot2)
library(ggthemes)
#human development index vs corruption perceptions index scatterplot
#this data visualization is basically a scatterplot with a line of smooth/trend fit to our scatter plot.
library(data.table)
#fread() of the data.table reads a little faster than read.csv()
#load the data set
df <- fread('Economist_Assignment_Data.csv', drop = 1)
# print(head(df))
# print(class(df))
#create the data and aesthetic layer and color the points/observations by grouping them using the region feature
#this coloring with the region feature can be done either in the aesthetic layer or in the geometry layer using the aes() function.
#in scatterplot different number assigned to shape argument of geometry layer will result in different shapes, 1 -> empty star looking circles, 2 -> triangle, 3 -> plus etc.  
#stroke argument changes the width of the border of the shape chosen
pl <- ggplot(df, aes(CPI, HDI, color = Region)) + geom_point(size = 4, shape = 1, stroke = 2)
#add a trend line using the geom_smooth() in the geometry layer
#assigning arguments in aes() in the geometry layer for smooth line plot
#this is similar to doing linear regression over the underlying scatterplot
#argument group set to 1 is like assigning the smooth line geometry layer the data for which we are plotting the smooth/trend line for.
#lm set as model argument is linear model
#set formula argument to get a log trend line plot for the underlying scatterplot.
#set se to F to remove the grey colored smooth confidence interval area around the line plot
pl1 <- pl + geom_smooth(aes(group = 1), method = 'lm', formula = y ~ log(x), se = F, color = 'red')
#add labels to the points plotted in the scatterplot using the geom_text() in the geometry layer.
#assign a feature from the data set to the label argument in the aes() function inside the geom_text() geometry layer to actually assign 
#labels to the corresponding observation points in the scatterplot.  
# pl2 <- pl1 + geom_text(aes(label = Country))
#select a subset of character strings from the Country feature in the data set 
#and then add those as labels to the corresponding observation points in the scatterplot.
#to do this create a character vector of country names we want to select from the country feature of the data frame and put as labels.
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
#here the label argument is set to the country feature of the data set, 
#but the data argument specifies which of those values in the country column needs to be labelled to their corresponding plotted points.
#the data argument takes the subset of the country feature in the df data frame using the IN operator to do a match between 
#values in the country feature and the passed character vector of country names desired to be outputted as labels to their corresponding plotted points.
#the color for the labels is assigned and check overlap set to T automatically makes sure the text labels don't overlap with each other.
pl2 <- pl1 + geom_text(aes(label = Country), color = "gray20", data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)
#use the white economist theme from the ggthemes library
#add a co-ordinate/scaling layer using the scale_x_continuous() function to makes changes to the scales/values and labels for the x axis.
#we set the limits for the values at the x axis using limit argument which accepts a vector specifying the lower and higher limits.
#breaks argument helps us to determine at what values we want the dashes to be placed on the x axis. This also takes a vector of numericals.
#set the label name for x axis using the name argument
#set the same arguments for the y axis using the scale_y_continuous() function in the coordinate/scale layer.
pl3 <- pl2 + scale_x_continuous(name = 'Corruption Perceptions Index, 2011 (10 = least corrupt)', limits = c(0.9,10.5), breaks = 1:10) + scale_y_continuous(name = 'Human Development Index, 2011 (1 = Best)', limits = c(0.2, 1.0), breaks = c(0.2,0.4,0.6,0.8,1.0)) + theme_economist_white()
#set the title for the plot using the ggtitle() function
pl4 <- pl3 + ggtitle('Corruption and Human Development')
print(pl4)