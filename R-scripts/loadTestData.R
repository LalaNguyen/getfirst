#testLabel<- tail(unlist(strsplit(outDir,as.factor("/"))),1)
docs <- VCorpus(DirSource(outDir,pattern = "\\.txt$"), readerControl = list(language ="en"))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
# Remove punctuation except '-','_'
docs <- tm_map(docs, f)
docs <- tm_map(docs, removeWords, stopwordnew)
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
tmp <- inspect(dtm)
tmp<-as.data.frame(tmp)
# Remove txt from patent ID
tmp$patId<-unlist(lapply(rownames(tmp), function(x) unlist(strsplit(x,split=".",fixed=TRUE))[1]))
mdata <- melt(tmp,id.vars="patId")
mdata$variable<- Trim(str_replace_all(mdata$variable,"[[:punct:]]", " "))
mdata$variable<-sapply(mdata$variable,function(x)paste0(wordStem(unlist(strsplit(x,split = " "))),collapse = " "))
# replace double space
mdata$variable<-sapply(mdata$variable,function(x) Trim(clean(x)))
# aggregate the common terms
mdata <- aggregate(value~patId+variable,data=mdata,sum)
tmp <- dcast(mdata,patId~variable,value.var = "value")
rownames(tmp)<-tmp[,1]
tmp<-tmp[,-1]
