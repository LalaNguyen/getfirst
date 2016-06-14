#***************************************
# 1.Retrieve documents term from database
#***************************************
# install.packages("RPostgreSQL")
require("RPostgreSQL")
require("reshape2")
require("ggplot2")
require("plyr")
library("rbenchmark")
pass <- "makata0611"

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pass)
parsedQuery <- paste0("SELECT * from docs")
clsTree <- dbGetQuery(con, parsedQuery)

#***************************************
# 2.Build Term Document Class Matrix
#***************************************
# Retrieve smaller subset for preprocessing
subset <- clsTree[,c("id","path","keyterm")]
# For each keyterm, we find number of documents that has the term
# within each class
a <- subset
a$docCount <-1
a.agg<-aggregate(docCount~path+keyterm,data = a, sum)
a.dc <- dcast(a.agg, path~keyterm,value.var = "docCount",fill=0)
rownames(a.dc)<-a.dc[,1]
a.dc<-a.dc[,-1]
#*****************************************
# 2.Build Document Frequency Class Matrix
#*****************************************
# Get Unique Document ID with Class
DocumentClassDF<- unique(subset[,c("id","path")])
# Get total number of documents for each Class
ClassDocTabl<-table(DocumentClassDF$path)

a.dc$No.Docs <- as.numeric(ClassDocTabl)
# Convert mdata to matrix
TermClassMatrix <- as.matrix(a.dc)
InvertedTermClassMatrix <- log(TermClassMatrix[,ncol(TermClassMatrix)]/TermClassMatrix)
# For term with 0 frequency, Division will cause Inf
InvertedTermClassMatrix[InvertedTermClassMatrix==Inf]<-0

#*****************************************
# 3.Build Class Bonus Frequency Matrix
#*****************************************
# We count each class the term has frequency == 0
# Larger sums means the word is spreading through many class
# and it is not important, we can try to negate the score
# score = log(No.Classes/Number Of Classes contains w)
InvertedClassMatrix <- log(nrow(TermClassMatrix)/colSums(TermClassMatrix!=0))
# Calculate Inverted Term Class * global value
#Method 1:
#re <- t(InvertedTermClassMatrix)*(InvertedClassMatrix)
#Method 2:
re <- t(InvertedTermClassMatrix)
re <- re[-nrow(re),]
#*****************************************
# 4.Build Class Term Frequency Matrix
#*****************************************
ClassFreqDF<-aggregate(freq~path+keyterm,data=clsTree,sum)
ClassFreq<-dcast(ClassFreqDF,path~keyterm,value.var="freq",fill=0)
rownames(ClassFreq)<-ClassFreq[,1]
ClassFreq <- ClassFreq[,-1]
# Convert back to matrix for computation
ClassFreqMatrix <- as.matrix(ClassFreq)

#*****************************************
# 5.Final Score for each Term
#*****************************************
library("stringr")
final<-t(ClassFreqMatrix)*(re)
keywords<-unique(rownames(final))
keywords<-str_replace_all(keywords, "[[:punct:]]", " ")
rownames(final)<-keywords
#*****************************************
# 6. Project User's Query
#*****************************************
q <- "network transmit signal network"
tokens <- unlist(strsplit(q," "))
m <- matrix(tokens)
#*****************************************
# Method 1. Sum Probability
#*****************************************
# GeT Row index of each term
idx <- apply(m,1,function(x)which(rownames(final)==x,arr.ind = TRUE))
rank<-sort(colSums(final[idx,]),decreasing = TRUE)
rankdf <- data.frame(class=names(rank), score=rank)
ggplot(rankdf,aes(x=class,y=score))+geom_bar(stat="identity")+coord_flip()

#*****************************************
# Method 2. Class Vector Space Model
#*****************************************
freq <- count(tokens)$freq
qlen <- sqrt(sum(freq**2))
# Calculate Class Length double each element in cell
doub<-apply(final,2,function(x)x**2)
# Calculate Class Length
clsLen <- sqrt(colSums(doub))
# Calculate denominator
denominator <- clsLen*qlen
# Calculate nominator
freqMatrix <- as.matrix(freq)
nominator<-t(final[idx,])%*%freqMatrix
vsm <- data.frame(class=names(rank), score=nominator/denominator)
ggplot(vsm,aes(x=class,y=score))+geom_bar(stat="identity")+coord_flip()
