library(mallet)
library(plyr)
library(ggplot2)
numberOftopics <- 30
setwd("C:/Users/Administrator/OPAIRS/R-scripts/classes")
outFolderPath <- "C:/Users/Administrator/OPAIRS/R-scripts/mallet"
folders <- list.dirs(path=".", recursive = TRUE)
classIds <- lapply(folders,function(x) unlist(strsplit(x,split="/"))[2])
classIds <- na.omit(unlist(classIds))

for (classId in classIds){
  project_path <- paste0("./",classId)
  flist <- list.files(project_path, ".txt", full.names = TRUE)
  file.copy(flist,outFolderPath)
}

documents <- mallet.read.dir(outFolderPath)
## Create a mallet instance list object. Right now I have to specify the stoplist
##  as a file, I can't pass in a list from R.
## This function has a few hidden options (whether to lowercase, how we 
##   define a token). See ?mallet.import for details.
## Create a topic trainer object.
mallet.instances <- mallet.import(documents$id, documents$text,"../stopwords.txt",token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

topic.model <- MalletLDA(num.topics=numberOftopics)

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
##  We can specify the number of iterations. Here we'll use a large-ish round number.
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
data <- data.frame(words='',weights=0, topic=0)
for (i in seq(1:numberOftopics)){
  topic <- mallet.top.words(topic.model, topic.words[i,],90000)
  topic$number <- i
  data <-  rbind.fill(data, topic)
}
mdata <- rbind.fill(data[data$words=="network",],data[data$words=="transmit",],data[data$words=="signal",]
)
agg <- aggregate(weights~number,data=mdata,sum)
ggplot(data = agg, aes(x=number,y=weights,fill=number))+geom_bar(stat = "identity")+coord_flip() 
