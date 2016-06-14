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
TermClassMatrix <- as.matrix(a.dc)

#*****************************************
# 2.Build Document Frequency Class Matrix
#*****************************************
docLength<-length(unique(clsTree$id))
TermDocDF<-clsTree[,c("id","keyterm")]
TermDocDF$docCount<-1
TermDocDF.agg<-aggregate(docCount~keyterm,data = TermDocDF, sum)
rownames(TermDocDF.agg)<-TermDocDF.agg$keyterm
idf<-log(docLength/as.matrix(TermDocDF.agg[,-1]))
#*****************************************
# 3.Build Class Bonus Frequency Matrix
#*****************************************

#Method 2:
final <-t(TermClassMatrix)/idf[,1]
keywords<-unique(rownames(final))
keywords<-str_replace_all(keywords, "[[:punct:]]", " ")
rownames(final)<-keywords
