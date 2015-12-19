library(mallet)
setwd("C:\\Users\\Administrator\\OPAIRS\\class\\345173")
## Create a mallet instance list object. Right now I have to specify the stoplist
##  as a file, I can't pass in a list from R.
## This function has a few hidden options (whether to lowercase, how we
##   define a token). See ?mallet.import for details.

docs <- mallet.read.dir("C:\\Users\\Administrator\\OPAIRS\\class\\345173")
mallet.instances <- mallet.import(docs$id, docs$text,"C:/Users/Administrator/OPAIRS/Stoplist.txt",token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
k = 15
## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=k)

## Load our documents. We could also pass in the filename of a
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations,
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)
## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round nu
topic.model$train(200)
## NEW: run through a few iterations where we pick the best topic for each token,
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)
## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

## What are the top words in topic 7?
##  Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
mallet.top.words(topic.model, topic.words[1,])
mallet.top.words(topic.model, topic.words[2,])
mallet.top.words(topic.model, topic.words[3,])
mallet.top.words(topic.model, topic.words[5,])
mallet.top.words(topic.model, topic.words[6,])
mallet.top.words(topic.model, topic.words[7,])
mallet.top.words(topic.model, topic.words[10,])

library(wordcloud)
# be sure you have installed the wordcloud package
topic.num <- 1
num.top.words<-100
topic.top.words <- mallet.top.words(topic.model, topic.words[1,], 100)
wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)

num.topics<-10
num.top.words<-25
for(i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model, topic.words[i,], num.top.words)
  wordcloud(topic.top.words$words, topic.top.words$weights, c(4,.8), rot.per=0, random.order=F)
}


