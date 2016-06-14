library(reshape2)
library(stringr)
library(plyr)

require("RPostgreSQL")

# Return frequency of keyword for a given class
getDocWordFreq<-function(df,path,keyword){
    # Make absolute path begin with root
    path <- paste0("^",path)
    keyword <- paste0(keyword,"$")
    matched <- intersect(grep(path,clsTree$path),grep(keyword,clsTree$keyTerm))
    subTree <- df[matched,]
    return (subTree)
}
getClassWordPercent<-function(df,keyword){
  # Make absolute path begin with root
  df <- getWordFreqs(df,keyword)
  percent <- data.frame()
  # If word occupies in less than 1 class
  if (length(df$path)<2){
    return(data.frame())
  } else{
    for (i in df$path){
      tmp <- unlist(strsplit(i,"/"))
      # Extend the file list
      #mydat <- data.table(clsId=unlist(strsplit(tmp,"/")))
      #l <- list(percent,mydat)
      #percent <- rbindlist(l,use.names=TRUE,fill=TRUE)
      #percent[selected_row,score:= selected_score]
      #percent[selected_row,path:=selected_path]
      
      percent <- rbind.fill(percent,data.frame(clsId=unlist(strsplit(tmp,"/"))))
      selected_row <- (nrow(percent)-length(tmp)+1):nrow(percent)
      selected_score <- df[df$path==i,c("freqs")]
      selected_path <- as.character(df[df$path==i,c("path")])
      
      percent$score[selected_row] = selected_score
      percent$path[selected_row] = selected_path

    }
    # Make the cls unique
    percent <- aggregate(score~clsId,data=percent,sum)
    percent$term <- keyword
    return (percent)
  }
  #percent <- data.table()
}

getWordFreqs<-function(df,keyword){
  keyword <- paste0(keyword,"$")
  df$term <- sapply(strsplit(as.character(df$pathString),"/"),function(x) x[length(x)])
  # Remove the last occurance of /
  df$path <- unlist(lapply(df$pathString,function(x) sub("/[^/]*$","",x)))
  mdata <- data.frame(path=df$path,term = df$term,freqs=df$freqs)
  # Filter class with the required keyword
  mdata <- mdata[grep(keyword,mdata$term),c("path","freqs")]
  agg.data <- aggregate(freqs~path,mdata, sum)
  return(agg.data)
}

getClassWordsFreqs<-function(df){
  df$term <- sapply(strsplit(as.character(df$pathString),"/"),function(x) x[length(x)])
  # Remove the last occurance of /
  df$path <- unlist(lapply(df$pathString,function(x) sub("/[^/]*$","",x)))
  mdata <- data.frame(path=df$path,term = df$term,freqs=df$freqs)
  agg.data <- aggregate(freqs~path,mdata, sum)
  return(agg.data)
}

getWordsPercents_B<-function(df){
  # Extract  term form the path
  b<-unlist(lapply(lapply(df$pathString,function(x) unlist(strsplit(x,"/"))), function(x) x[length(x)]))
  c<- data.frame(term=b,freqs=df$freqs)
  keyterms <- aggregate(freqs~term,data = c,sum)
  uni_key<- unique(keyterms[keyterms$freqs>=2,c("term")])
  uni_key<-as.character(uni_key)
  count<-0
  for (i in uni_key){
    count<-count+1
    print(paste0("Processing key:",i,". ",count,"/",length(uni_key)))
    dat<-getClassWordPercent(df,i)
    percent <- rbind.fill(percent,dat)
  }
  return(percent)
}
getSubTree<-function(df,path){
  # Make absolute path begin with root
  path <- paste0("^",path)
  matched <- grep(path,clsTree$pathString)
  subTree <- df[matched,]
  return (subTree)
}
getClassWordMatrix<-function(df){
  df$term <- sapply(strsplit(df$pathString,"/"),function(x) x[length(x)])
  df$path <- unlist(lapply(df$pathString,function(x) sub("/[^/]*$","",x)))
  mdata <- data.frame(term=df$term,path=df$path,freqs=df$freqs)
  clswordMatrix <- dcast(mdata,term~path)
  # Replace NA with 0
  clswordMatrix[is.na(clswordMatrix)] <- 0
  return(clswordMatrix)
}
getTopWords<-function(df,path,limit=10){
  path <- paste0("^",path)
  matched <- grep(path,clsTree$pathString)
  #Split the term at the end of the path
  if(length(matched)==0) 
    {
      print("No path matched")
      return (0)
  } else {
  # Collect matched data
  df <- df[matched,]
  df$term <- sapply(strsplit(df$pathString,"/"),function(x) x[length(x)])
  agg.data<- aggregate(freqs~term,data = df[,c("term","freqs")],sum)
  return (head(agg.data[with(agg.data,order(-freqs)),],limit))
  }}

######## Our main here

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
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
clsTree <- dbGetQuery(con, "SELECT * from docs")

word.freqs.in.class<-getDocWordFreq(clsTree,"370328/370329","wlan")
#subTree<-getSubTree(clsTree,"370328/370329")
#top.words <- getTopWords(clsTree,"370328/370329/370331",20)
#word.freqs <- getWordFreqs(clsTree,keyword = "access")
#word.percent <- getClassWordPercent(clsTree,keyword = "wireless_communication_system")
# *********************************************** 
# Testing functin getClassWordMatrix for heatmap 
# *********************************************** 

#clsWord<- getClassWordMatrix(clsTree)
#rownames(clsWord)<-clsWord$term
# Remove row which contains any 0
#df<-data.matrix(clsWord,rownames.force = clsWord[,1])
#clean.df<-df[ !rowSums(df[,colnames(df)[(2:ncol(df))]]==0)>=1,-1]
# Draw heatmap
#d3heatmap(clean.df, scale = "column", dendrogram = "none")
# *********************************************** 
# Testing functin getWordsPercents for Tree 
# *********************************************** 
#time_a<-system.time(a <- getWordsPercents_A(clsTree))
time_b<-system.time(b<-getWordsPercents_B(clsTree))
print(time_b)
#write.csv(b[b$score>100,],file="C:/Users/Administrator/OPAIRS/utility/foo.csv")
#url <- "C:/Users/Administrator/OPAIRS/R-scripts/web/train/tree.json"
#classNet <- jsonlite::fromJSON(url, simplifyDataFrame = FALSE)
#radialNetwork(List = classNet, fontSize = 10, opacity = 0.9)
