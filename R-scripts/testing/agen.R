library(tm)
library(SnowballC)

f <- content_transformer(function(x){
  gsub("[^[:alnum:][:space:]_-]", "", x)
}
)
list.dirs()
script.dir <- dirname(sys.frame(1)$ofile)
setwd("../OPAIRS/R-scripts/web")
out_path<- "train/370328/370329/370330/"
docs <- VCorpus(DirSource("train/370328/370329/370330/"), readerControl = list(language ="en"))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, f)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
# Inspect doc
lapply(docs[1:2], as.character)
# Creating a term-document matrix
dtm <- DocumentTermMatrix(docs,control = list(weighting =
                                                function(x)
                                                  weightTfIdf(x, normalize =
                                                                TRUE)))
m <- inspect(dtm)
DF <- as.data.frame(m, stringsAsFactors = FALSE)
write.csv(DF,paste0(out_path,"370330.csv"))

