#data mining twitter to create word clouds of topics of what's trending on twitter.
#or what does the twitter ecosystem thinks about the topics that we feed into our model.

#install the following packages for this project
# install.packages('tm') used to work with the text data and manipulate it.
# install.packages('twitteR') used to connect to twitter
# install.packages('wordcloud')
# install.packages('RColorBrewer') #used to color the wordcloud
# install.packages('e1071') used for SVM
# install.packages('class') used for KNN

#load these libraries
library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
# library(e1071)
# library(class)

#login to your twitter account at either of the links below
#https://apps.twitter.com or https://developer.twitter.com/en/apps

#create a new twitter app at the page, fill in the details.
#we will look for the keys and tokens.

#copy the Consumer API key, Consumer API Secret key

#create access token and access token secret

#these four strings will be used to perform twitter authorization and connect our R code to twitter.

# store these as variables to be used later.
ckey <- 'consumer.api.key'
skey <- 'consumer.api.key.secret'
token <- 'access.token'
sectoken <- 'access.token.secret'

#connect to twitter
#use the setup_twitter_oauth() function which helps us to authorize ourselves to twitter.
#pass the consumer API key, consumer API secret key, access token and the access token secret as the four arguments. 
setup_twitter_oauth(ckey, skey, token, sectoken)

#search twitter to grab tweets containing specific words.
#search for 'soccer' word using the searchTwitter() function.
#pass the search word as the first argument and assign the no of tweets we want to the 'n' parameter.
#assign the language of the tweets we want to the 'lang' parameter. for english it is 'en'.

#this will return a list of the most recent 1000 tweets data with 'soccer' in them in english language. 
#tweets data object which has list of tweets data
soccer.tweets <- searchTwitter('soccer', n = 1000, lang = 'en')
# print(head(soccer.tweets))

#do help('searchTwitter') to get to know about this function. We can specify geographical regions, longitude, latitude, radius,
#dates of tweets, languages of tweets etc to retrieve specific tweets.

#to get the text out of the tweets data use the sapply() function, 
#a custom function which makes use of getText() function(used to get the text out of the tweets data for a tweets data object) used alongside the '$' symbol which is used for each tweet item in the tweets data object for the tweets data.
soccer.text <- sapply(soccer.tweets, function(x){x$getText()})
# print(head(soccer.text))

#clean the text
#remove the emoticans/emojis and create a corpus of tweets.
#use the iconv() function which is a character encoding function, it removes the emojis, accent symbols
#and other symbols that not allowed and might produce errors.
#the first argument is the data we are working on, second is the character encoding we are converting from and the
#third is the character encoding we are converting to.
soccer.text <- iconv(soccer.text, 'UTF-8', 'ASCII')
# print(head(soccer.text))

#create a corpus of the soccer texts we have using the corpus() function of tm library.
#each soccer text can be considered as a document and we are creating a corpus of those soccer texts.
#pass the vectorSource() function as the first argument to create a vector source of the soccer texts(each soccer text is considered as a single vector) with soccer.text passed in that function.
soccer.corpus <- Corpus(VectorSource(soccer.text))
# print(soccer.corpus)

#create a document term matrix using the termdocumentmatrix() function
#this is same as creating a matrix which has TF values for each term doc pair in the corpus.
#pass the corpus as the first argument, the control parameter takes the list of actions 
#to be performed before creating the doc term matrix.
#we will include remove punctuations, remove stop(common) words by providing a character vector
#of words(e.g. soccer) which also includes stopwords() function with english passed as the argument to
#remove the common english stop words in the list of actions to be performed.
#the removenumbers parameter is used to remove the numbers from the soccer texts in the corpus.
#the tolower parameter is used to convert all the soccer texts to lower case.
term.doc.matrix <- TermDocumentMatrix(soccer.corpus, control = list(removePunctuation = T,
                                                                    stopwords = c('soccer','http', stopwords('english')),
                                                                    removeNumbers = T, tolower = T))
#all the parameters in the control parameter of the termdocumentmatrix() function are the functions
#available in the tm library for data manipulation.
# print(term.doc.matrix)

#the object returned by termdocumentmatrix() function is actually not a matrix, there fore
#we need to convert it to a matrix using as.matrix() function.
term.doc.matrix <- as.matrix(term.doc.matrix)
# print(head(term.doc.matrix))

#get the word count for each word we now have after cleaning, removing stopwords, numbers etc. to be put into the wordcloud.
#to compute the freq of a word in a corpus, sum the entries for all the columns/documents belonging to a row of a term/word.
#use the sort() function, first argument is the rowSums() function with the created document term matrix passed as the argument and
#the second argument is the decreasing parameter set to T.
word.freq <- sort(rowSums(term.doc.matrix), decreasing = T)
# print(head(word.freq))
# print(class(word.freq))
#this returns a numeric vector with names of the words attached to each corresponding word count.
#we convert this vector into a dataframe with columns for word names and their corresponding frequencies.
#use the names() function to get the names of the words from the word.freq vector.
dm <- data.frame(words = names(word.freq), freq = word.freq)
# print(head(dm))

#create the wordcloud
#use the wordcloud() function from the wordcloud library
#pass the words and freq columns of the 'dm' data frame as the first two arguments.
#put the random.order parameter to False.
#choose a color scheme using the colors parameter such as brewer.pal(8,'Dark2')
#the color scheme is from the Rcolorbrewer library 
wordcloud(dm$words, dm$freq, random.order = F, colors = brewer.pal(8,'Dark2'))
#observations:
#the word size in the wordcloud represents the freq of it occuring in the corpus.
#there are links in the wordcloud as people are tweeting those links.
#put 'http' in the stop words parameter in the list for control parameter of termdocumentmatrix() function.



#other than soccer let's search for python.