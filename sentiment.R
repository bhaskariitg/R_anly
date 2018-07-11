#Read File
appletweets<-reaad.csv("APPLE-Twiitter-Sentiment.csv",header=TRUE,stringAsFactors=FALSE)
str(appletweets)
#To detect sentiment of tweets, factorize the data into categories
appletweets$sentiment<-as.factor(appletweets$sentiment)
table(appletweets$sentiment)
#Use the Bag of words approach
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
#Using Corpus(Collection of text documents) from tm package.We will use text column of our data frame
doc<-Corpus(VectorSource(apple$text))
#Preprocess the data to clean the irregularities using tm_map function
#Change all the words to lowercase
doc<-tm_map(doc,tolower)
#Now remove Punctuation
doc<-tm_map(doc,removePunctuation)
#Remove stop words
doc<-tm_map(doc,removeWords,c("apple",StopWords("english")))
#Stem Document
doc<-tm_map(doc,stemDocument)
#Extract the word frequencies to use in our problem
#Using DocumentTermMatrix to generate a matrix in which the column corresponds to words in te tweets
frequencies<-DocumentTermMatrix(doc)
frequencies
inspect(frequencies[1:5,1:10])
#find the popular terms
findFreqTerms(frequencies,lowfreq=15)
#remove the words that dont appear too often
#Keep the terms that appear in 1% or more of the tweets
sparse<-removeSparseTerms(frequencies,0.99)
#Convert sparse matrix into data frame
tweets<-as.data.frame(as.matrix(sparse))
colnames(tweets) <-make.names(colnames(tweets))
tweets$sentiment<-appletweets$sentiment
tweets$sentiment.confidence<-appletweets$sentiment.confidence
install.packages("caTools")
library(caTools)
set.seed(123)
split<-sample.split(tweets$sentiment,SplitRatio=0.7)
train<-subset(tweets, split==TRUE)
test<-subset(tweets, split==FALSE)
install.packages("randomForest")
library(randomForest)
tweetsforest<-randomForest(sentiment~.,data=train)
predictforest<-predict(tweetsforest,newdata=test)
table(test$sentiment,predictforest)
#To find accuracy of baseline model
table(test$sentiment)
install.packages("e1071")
tweetssvm<-svm(sentiment~.,data=train)
predictsvm<-predict(tweetssvm.newdata=test)
table(test$sentiment,predictsvm)