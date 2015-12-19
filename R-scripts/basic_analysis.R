#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   
abs_path<-"C:/Users/Administrator/OPAIRS/class/345173"

cname <- file.path(abs_path)   
library(tm)   
docs <- Corpus(DirSource(cname))  
docs <- tm_map(docs, removeWords, stopwords("english"))   

library(SnowballC)   
docs <- tm_map(docs, PlainTextDocument) 

dtm <- DocumentTermMatrix(docs)   
freq <- colSums(as.matrix(dtm))   
ord <- order(freq)
freq[head(ord)]   

library(wordcloud)   
set.seed(142)   
wordcloud(names(freq), freq, min.freq=5)   