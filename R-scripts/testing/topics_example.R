# Load/install necessary packages
library(devtools)
#install_github("cpsievert/LDAvis")
library(LDAvis)
library(mallet)
library(Rmpfr)
library(plyr)
library(XML)
library(stringi)
project_path <- "C:\\Users\\Administrator\\OPAIRS\\class\\"
path <- paste0(project_path,"345173")
docs <- mallet.read.dir(path)
instance <- mallet.import(docs$id, docs$text,"C:/Users/Administrator/OPAIRS/Stoplist.txt",token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
model <- MalletLDA(num.topics = 50)
model$loadDocuments(instance)
freqs <- mallet.word.freqs(model)

# add infrequent words to the list of stopwords and re-import

model$train(200)
# Here, we compute the estimated topic-term distribution, incorporating the effect
# of the prior using 'smoothed = TRUE'.
phi <- (mallet.topic.words(model, smoothed = TRUE, normalized = TRUE))
# Now get the smoothed estimates of the document-topic distributions:
topic.words <- mallet.topic.words(model, smoothed = TRUE, normalized = FALSE)
doc.topics <- mallet.doc.topics(model, smoothed=T, normalized=T)

# 'count' of the number of tokens per topic (including pseudo-tokens from the priors)

vocab <- model$getVocabulary()
word.freqs <- mallet.word.freqs(model)
term.freqs <- word.freqs$term.freq
doc.tokens <- data.frame(id=c(1:nrow(doc.topics)), tokens=0)
for(i in vocab){
  # Find word if word in text
  matched <- grepl(i,docs$text)
  doc.tokens[matched,2] =doc.tokens[matched,2] +  1
}
json <-createJSON(phi = phi, 
                  theta = doc.topics, 
                  doc.length = doc.tokens$tokens, 
                  vocab = vocab,
                  term.frequency = term.freqs)
serVis(json, open.browser = TRUE)

