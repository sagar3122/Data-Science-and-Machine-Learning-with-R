#money ball project
library(dplyr)
library(ggplot2)
#loading the batting file
batting <- read.csv('batting.csv')
# print(head(batting))
# print(str(batting))
# print(batting$X2B)
at.bats <- batting[,'AB']
# print(class(at.bats))
# print(head(at.bats))
X2B <- batting[,'X2B']
# print(head(X2B))
#when a data set with column names starting with numbers are loaded as a data frame, those column names are appended with an X in the front
#and accessed with this new name whereas when loaded as a data table those columns can be accessed in the following ways: dt[, "numerical.column.name", with = FALSE]
#or dt$`numerical.column.name` or dt[, `numerical.column.name`] or dt[[numerical.column.position.number]] 
#feature engineering
#adding batting average feature to the data set
batting['BA'] <- batting$H / batting$AB
# print(tail(batting$BA, 5))
#creating On base percentage
batting['OBP'] <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
# print(tail(batting$OBP, 5))
#creating X1B Singles to create Slugging Percentage
batting['X1B'] <- batting$H - batting$X2B - batting$X3B - batting$HR
# print(tail(batting$X1B, 5))
#creating Slugging Percentage
batting['SGP'] <- (batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) / batting$AB
# print(tail(batting$SGP, 5))
# print(str(batting))
batting.refined <- subset(batting, yearID >= 1985)
# print(head(batting.refined))
# print(summary(batting.refined))

#load the salary data set
salary <- read.csv('Salaries.csv')
# print(head(salary))
# print(salary$salary)
# print(head(batting.refined))
# print(summary(batting.refined))
# print(summary(salary))
#use merge(df1,df2, by = c('column1','column2',...)) to merge data frames on column names.
combo <- merge(batting.refined, salary, by = c('yearID','playerID'))
# print(head(combo))
# print(summary(combo))
#getting stats about the lost players
#lost_boys <- subset(combo, playerID == 'giambja01' | playerID == 'damonjo01' | playerID == 'saenzol01')
#subset() using the %in% operator
#subset(df, feature.name %in% c(vector of desired values))
lost_players <- c('giambja01', 'damonjo01', 'saenzol01')
lost_boys <- subset(combo, playerID %in% lost_players)
# print(lost_boys)
#we want to get the stats for the year the lost players left Oakland
lost_boys <- subset(lost_boys, yearID == 2001)
lost_boys <- lost_boys[c('playerID','H','X2B','X3B','HR','OBP','SGP','BA','AB')]
# print(lost_boys)
#combined AB
sum_AB <- sum(lost_boys$AB)
# print(sum_AB)
mean_OBP <- mean(lost_boys$OBP)
# print(mean_OBP)
sum_salary = 15000000

#find replacement players
#conditions:
#The total combined salary of the three players can not exceed 15 million dollars.
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players 
#we want stats of all players in the year 2001
combo.redefined <- subset(combo, yearID == 2001)
# print(summary(combo.redefined))
# print(class(combo.redefined))
#use plotting to see what data points satisfy the conditions
pl <- ggplot(combo.redefined, aes(x = OBP, y = salary))
pl1 <- pl + geom_point()
# print(pl1)
#after the plot we infer that a lot of players have a OBP >= 0.364 with a salary less than 8 mil,
#so we will set max limit for salary to be 8 mil and set the OBP to be greater than 0.
# combo.redefined <- filter(combo.redefined, salary <= 8000000, OBP >0)
# print(summary(combo.redefined))
# print(str(combo.redefined))
#AB for each replacement player should be around >= 450 as (1469/3) = 489.67
#we choose AB to be a little less as many players might have a higher AB.
combo.redefined <- filter(combo.redefined, salary <= 8000000, OBP >0, AB >= 450)
# print(summary(combo.redefined))
#arrange the combo.redefined with desc order of OBP
combo.redefined <- arrange(combo.redefined, desc(OBP))
# print(head(combo.redefined, 10))
options.replacement.players <- select(combo.redefined, c('playerID','AB','OBP','salary'))
# print(head(options.replacement.players, 10))
options.replacement.players <- filter(options.replacement.players, !playerID %in% lost_players)
#the first three players have the highest sum(AB) and highest mean(OBP) and fall under the price bracket of 15 mil after satisfying the conditions given.
print(head(options.replacement.players, 10))









