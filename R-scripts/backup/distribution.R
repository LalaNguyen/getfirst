library(RPostgreSQL)
library(ggplot2)
library(plyr)
library(reshape2)
pw <- {
  "makata0611"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
query <- "data"

# Get list of document with frequency on keyterm
tokens <- unlist(strsplit(query," "))
tokens <- sapply(tokens, function(x) paste0("keyterm = '",x,"'"))
#Append token to query string
parsedString <- paste0(tokens,collapse = " or ")
parsedQuery <- paste0("SELECT * from docs where ", parsedString)
clsTree <- dbGetQuery(con, parsedQuery)
# Dump to file

# Cast data to width
wtextsearch <- dcast(clsTree,id+path+date~keyterm,fill = 0,value.var = c("freq"))
# Remove document with 0 frequency on both term and only one frequency on each term
wts0<-wtextsearch[rowSums(wtextsearch[4:ncol(wtextsearch)])>0,]

# Split result into two data frames for comparison
colMul<- function(df){
  re <-rep(1,nrow(df))
  from <- 4
  to <- ncol(df)
  for (i in from:to){
    re<-re*df[,i]
  }
  return (re)
}

wts1<-wts0[colMul(wts0)!=0,]
wts2<-wts0[colMul(wts0)==0,]
# wts1 always has higher priority as both terms tend to appear in document
wts1.total <- data.frame(patid=wts1$id,path=wts1$path,date=wts1$date,freq=rowSums(wts1[4:ncol(wts1)]))
wts2.total <- data.frame(patid=wts2$id,path=wts2$path,freq=rowSums(wts2[4:ncol(wts2)])) 
ggplot(data=wts1.total,aes(x=freq,fill=path))+geom_histogram(binwidth=0.5)+facet_wrap(~path,scales="free_x",ncol=5) + labs(title = paste0("Frequency Distribution for '",query,"'"))+ scale_y_sqrt()
ggplot(data=wts1.total,aes(x=freq,fill=path))+geom_density(binwidth=0.5)+facet_wrap(~path,scales="free_x",ncol=5) + labs(title = paste0("Frequency Distribution for '",query,"'"))+ scale_y_sqrt()
# Calculate term trend
wts1.trend <- aggregate(freq~date+path,wts1.total,sum)
ggplot(data=wts1.trend,aes(x=date,y=log(freq),group=factor(path),colour=path))+geom_line()+facet_wrap(~path,scales="free_x",ncol=3)

ggplot(data=wts2.total,aes(x=freq,fill=path))+geom_histogram(binwidth=0.5)+facet_wrap(~path,scales="free_x",ncol=3) + labs(title = "Frequency Distribution for 'Transmit Signal'")+ scale_y_sqrt()

textSearch <- wts1.total[order(wts1.total$freq,decreasing = TRUE),]
ggplot(data=wts1.total[1:50,],aes(reorder(patid,freq),y=freq,fill=path))+geom_bar(stat='identity')+coord_flip()+labs("Full-text Search for 'Transmit Signal'")

# We fit a normal distribution and determine which one is off
# Using AIC we can tell which class is nearly normal and
# should be considered first
library("fitdistrplus")
clsRank <-data.frame(clsId="",score=0)
for (path in unique(as.character(wts1.total$path))){
  # Attempt to fit the normal distribution
  subdat<-wts1.total[wts1.total$path==path,c("freq")]
  if (length(unique(subdat))!=1){
    models <- fitdist(subdat,"weibull")
    clsRank <- rbind(clsRank,data.frame(clsId=path,score=models$aic))
  }
}
clsRank
parsedQuery <- "SELECT * FROM DOCS WHERE PATH='706 12'"
clsTree <- dbGetQuery(con, parsedQuery)
isDuplicate <- duplicated(clsTree$keyterm)
subtree <- clsTree[isDuplicate,]
subtree$docCount<-1
c<-aggregate(docCount~keyterm+freq,data=subtree,sum)
b<-c[,]
b_new<-b[order(b$keyterm),]
ggplot(data=b_new,aes(x=freq,y=docCount))+geom_bar(stat="identity")+facet_wrap(~keyterm,scales=c("free_x"),ncol=5) + labs(title = paste0("Frequency Distribution for '",query,"'"))+ scale_y_sqrt()
