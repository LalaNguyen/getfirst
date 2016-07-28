source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadSettings.R")
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/helper.R")
library(reshape2)
library(plyr)
library("RNeo4j")
## User confidential for neo4j
uname <- "neo4j"
pass <- "makata0611"
## Start Neo4j
graph = startGraph("http://localhost:7474/db/data/", username = uname, password =pass)
getPath <- function(classId){
  p<-cypherToList(graph,"Match p = (n)-[:IS_CHILD_OF*0..5]->(m) where n.name = {name} RETURN p, length(p) as len order by len DESC LIMIT 1",name=classId)
  # If no path is found, return the class as it has no children
  if (length(p)==0){
    return (classId)
  }
  else{
    # Cast path to path string
    pathString <- paste0(rev(unlist(nodes(p[[1]]$p))),collapse = "/")
    return (pathString)
  }
  }
setwd("C:/Users/Administrator/OPAIRS/R-scripts/classes")
folders <- list.dirs(path=".", recursive = TRUE)
classIds <- lapply(folders,function(x) unlist(strsplit(x,split="/"))[2])
classIds <- na.omit(unlist(classIds))

globalDoc <- data.frame(patid="", date="",keyterm="", freq=0, path="")
for (classId in classIds){
  # Paste all the patents in the class
  docs <- VCorpus(DirSource(paste0(classId, collapse = "/"),pattern = "\\.txt$"), readerControl = list(language ="en"))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  # Remove punctuation except '-','_'
  docs <- tm_map(docs, f)
  docs <- tm_map(docs, removeWords, stopwordnew)
  docs <- tm_map(docs, stripWhitespace)
  # Inspect doc
  #lapply(docs[1:2], as.character)
  # Creating a term-document matrix
  dtm <- DocumentTermMatrix(docs)
  tmp <- inspect(dtm)
  # Reshape data 
  tmp<-as.data.frame(tmp)
  # Remove txt from patent ID
  tmp$patId<-unlist(lapply(rownames(tmp), function(x) unlist(strsplit(x,split=".",fixed=TRUE))[1]))
  rownames(tmp)<-NULL
  # Convert data set from width to long
  mdata <- melt(tmp, id.vars = c("patId"))
  # We also attach date 
  dateFile <- paste0("C:/Users/Administrator/OPAIRS/R-scripts/classes/",classId,"/",classId,".csv") 
  clsDate <- read.csv(dateFile, sep=",")
  clsDate$id <- sapply(clsDate$id, function(x) {if(substring(x, 1, 1)!='0') {paste0("0",x)} else{paste0(x)}})
  names(clsDate)<-c("patId","date")
  mdata<-merge(clsDate,mdata,by="patId")
  # Get Path in Neo4j
  p<-getPath(classId)
  mdata$path<-p
  names(mdata)<-c("id","date","keyterm","freq","path")
  globalDoc<-rbind.fill(globalDoc, mdata)
}
# Remove dummy row
globalDoc<- globalDoc[-1,]
# Remove patid
globalDoc<- globalDoc[,-1]
# Remove data with freq = 0
globalDoc<-globalDoc[globalDoc$freq!=0,]

# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pass)

dbWriteTable(con, "docs", value = globalDoc, append = TRUE, row.names = FALSE)

