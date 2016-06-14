
f <- content_transformer(function(x){
  gsub("[^[:alnum:][:space:]_]", " ", x)
}
)
stopwordnew<-c(stopwords("english"),"includ","includes","instead","latter","least","smaller","smallest","specif","almost","along","specifi","less","like","may","moreov","now","onc","onto","per","particular","regard","herein","first","second","path","parent","name","level","count","counts","yes","yet","zero","within","without","also","actual","away","also","said","via","uppon","still","thereaft","therebetween","therefor","therebi","third","three","thus","twice","two","toward","upon","well","wherein","wherea","wherebi","abov","befor","beyond","whether","whose","will","back","can","certain","either","becaus","eight","afterward","furthermor","give","given","greater","hereinaft","howev","much","must","none","often","overal","includ","enough","etc")


CleanWord<-function(x,pattern){
  x<-Trim(str_replace_all(x,pattern, " "))
  x<-sapply(x,function(x)paste0(wordStem(unlist(strsplit(x,split = " "))),collapse = " "))
  x<-sapply(x,function(x) Trim(clean(x)))
  return(x)
}
fetchData<-function(){
  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "postgres",
                   host = "localhost", port = 5432,
                   user = sql_user, password = sql_pass)
  parsedQuery <- paste0("SELECT * from docs")
  clsTree <- dbGetQuery(con, parsedQuery)
  dbDisconnect(con) 
  return(clsTree)
}
getClassFrequencyMatrix<-function(x){
  x$keyterm<-CleanWord(x$keyterm,"[[:punct:]]")
  ClassFreqDF<-aggregate(freq~path+keyterm,data=x,sum)
  ClassFreqDF<- ClassFreqDF[ClassFreqDF$keyterm!="",]
  # Convert to wide format
  ClassFreqDF.long<-dcast(ClassFreqDF,path~keyterm,value.var="freq",fill=0)
  # Move class ID to rownames
  rownames(ClassFreqDF.long)<-ClassFreqDF.long[,1]
  ClassFreqDF.long <- ClassFreqDF.long[,-1]
  # Convert to a Class Frequency Matrix
  ClassFreqMatrix <- as.matrix(ClassFreqDF.long)
  return(ClassFreqMatrix)
}
getDocumentsperTerm<-function(x){
  x$keyterm<-CleanWord(x$keyterm,"[[:punct:]]")
  subTree <- x[,c("id","path","keyterm")]
  # For each keyterm, we find number of documents that has the term
  # within each class
  subTree$docCount <-1
  # because some terms may be repeated after cleanning process,
  # We first aggregate the term that have similar (1) id, (2) path, (3) keyterm
  subTree.agg<-aggregate(docCount~id+path+keyterm,data = subTree, sum)
  subTree.agg<-subTree.agg[subTree.agg$keyterm!="",]
  subTree.agg$docCount <-1
  # For each class, we aggregate the number of doc base on class and term
  subTree.agg<-aggregate(docCount~path+keyterm,data = subTree.agg, sum)
  subTree.wide <- dcast(subTree.agg, path~keyterm,value.var = "docCount",fill=0)
  rownames(subTree.wide)<-subTree.wide[,1]
  subTree.wide<-subTree.wide[,-1]
  return(subTree.wide)
}
getDocumentsperClass<-function(x){
  x$keyterm<-CleanWord(x$keyterm,"[[:punct:]]")
  DocumentClassDF<- unique(x[,c("id","path")])
  # Get total number of documents for each Class
  ClassDocTabl<-table(DocumentClassDF$path)
  return(ClassDocTabl)
}

getUnstemWords<-function(x,dict){
  unstemmed_words<-sapply(x,function(x) paste0(stemCompletion(unlist(strsplit(x,split=" ")),dict),collapse = " "))
  return(unstemmed_words)
}

getThesaurusSynonyms<-function(x){
  re<-getURL(paste0(APILink,APITheSaurus,as.character(x),APIFormat,collapse = ""))
  return(re)
}

getSimilarCompound1<-function(t1){
  q <- paste0("Match p=((n:Concept{word:'",t1,"'})-[r:IS_ASSOCIATED_WITH]->(m:Concept)) 
            Return EXTRACT(n in nodes(p)|n.class) as classID,
            EXTRACT(n in nodes(p)|n.word) as term,
            r.weighted")
  re<-unlist(cypherToList(graph,query = q))
  classID1 <- re[names(re)=="classID1"]
  classID2 <- re[names(re)=="classID2"]
  term1 <- re[names(re)=="term1"]
  term2 <- re[names(re)=="term2"]
  weighted <- re[names(re)=="r.weighted"]
  df <- data.frame(clsID=classID1,term1=term1,term2=term2,weighted=weighted)
  return(df)
}
getSimilarCompound2<-function(t1){
  q <- paste0("Match p=((n:Concept)-[r:IS_ASSOCIATED_WITH]->(m:Concept{word:'",t1,"'})) 
            Return EXTRACT(n in nodes(p)|n.class) as classID,
            EXTRACT(n in nodes(p)|n.word) as term,
            r.weighted")
  re<-unlist(cypherToList(graph,query = q))
  classID1 <- re[names(re)=="classID1"]
  classID2 <- re[names(re)=="classID2"]
  term1 <- re[names(re)=="term1"]
  term2 <- re[names(re)=="term2"]
  weighted <- re[names(re)=="r.weighted"]
  df <- data.frame(clsID=classID1,term1=term1,term2=term2,weighted=weighted)
  return(df)
}

getCandidatedClasses<-function(x){
  # Convert to table for counting
  TablNeighbours <- table(x$clsID)
  maxConcepts <- max(TablNeighbours)
  selected_classes <- TablNeighbours[TablNeighbours==maxConcepts]
}
getRelatedConcepts<-function(x){
  str <- unlist(strsplit(x,' ',fixed=TRUE))
  cp1 <- getSimilarCompound1(str[1])
  cp2 <- getSimilarCompound2(str[2])
  df <- list(cp1,cp2)
  return(df)
}
getSynonymsCP<-function(df, map_table){
  re<- data.frame(clsID="",term1="",term2="",weighted=0)
  for (i in seq(1:nrow(map_table))){
    clsIdx<-as.character(map_table$clsID)[i]
    subdat<-ldply(df[[i]],data.frame)
    subdat$clsID<-as.character(subdat$clsID)
    filtered_class<-subdat[subdat$clsID==clsIdx,]
    selected_term<-head(filtered_class[order(filtered_class$weighted,decreasing = TRUE),],1)
    re <-rbind(re,selected_term)
  }
  re<-re[-1,]
  return(re)
}