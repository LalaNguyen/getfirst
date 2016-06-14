#***************************************
# Finding Class Frequency
# 1.Retrieve documents term from database
#***********s****************************
library(reshape2)
library("RNeo4j")
## User confidential for neo4j
uname <- "neo4j"
pass <- "makata0611"
## Start Neo4j
graph = startGraph("http://localhost:7474/db/data/", username = uname, password =pass)

library(plyr)
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadSettings.R")
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/helper.R")
# Choose method for similarity
enableSemantic=FALSE

#method ="TFIDFICF" 
method ="TFICF" 
#method ="TFIDF" 
#method = "TF" 
# Choose to enable semantic enhancement

clsTree <- fetchData()
#head(clsTree)
#date                                    keyterm freq   path       id
#1 20110104 substantially_parallel_electrodes_arranged    1 345173 07864160
#2 20110104            capacitive_touch_sensing_system    1 345173 07864160
#3 20110104                              electrode_set    2 345173 07864160
#4 20110104   primary_electrode_electrically_connected    1 345173 07864160
#5 20110104                          primary_electrode    1 345173 07864160
#6 20110104                             electrode_sets    1 345173 07864160
#*****************************************
# 2 .Build Class Frequency Matrix        #
#*****************************************
# Create a data frame with 3 variables | classId | keyword | class frequency |
# We want to observe the class frequency of keywords
# We first clean the term in the clsTree
ClassFreqMatrix<-getClassFrequencyMatrix(clsTree)
# ClassFreqMatrix[1:5,1:5]
#              a expand progress outward a form aaa aab aac format
#345173                                0      0   0   0          0
#358  115                              0      0   0   0          0
#370252                                0      0   0   0          0
#370328                                0      0   0   0          0
#370328/370329                         0      0   1   0          0
#***************************************
# 3.Build Documents per Term Matrix
#***************************************
DocumentperTermDF<-getDocumentsperTerm(clsTree)
# DocumentperTermDF[1:5,1:5]
#              a expand progress outward a form aaa aab aac format
#345173                                0      0   0   0          0
#358  115                              0      0   0   0          0
#370252                                0      0   0   0          0
#370328                                0      0   0   0          0
#370328/370329                         0      0   1   0          0
#*****************************************
# 4.Build Documents per Class Matrix     #
#*****************************************
# Get Unique Document ID with Class
DocumentperClass<-getDocumentsperClass(clsTree)
# head(DocumentperClass)
#345173             358  115               370252               370328 
#576                  271                  446                  456 
#370328/370329 370328/370329/370331 
#663                  280 
#*****************************************
# 5.Build Class Bonus Frequency Matrix
#*****************************************
# We count each class the term has frequency == 0
# Larger sums means the word is spreading through many class
# and it is not important, we can try to negate the score
# score = log(No.Classes/Number Of Classes contains w)
DocumentperTermMat <- as.matrix(DocumentperTermDF)
# Convert mdata to matrix
InvertedTermClassMatrix <- log(as.numeric(DocumentperClass)/DocumentperTermMat)
# For term with 0 frequency, Division will cause Inf
InvertedTermClassMatrix[InvertedTermClassMatrix==Inf]<-0
InvertedClassMatrix <- log(nrow(ClassFreqMatrix)/colSums(ClassFreqMatrix>0))
# Inverted Class matrix is a vector of keywords with its
# classbonus value
if(method=="TFIDFICF"){
  adj <- t(InvertedTermClassMatrix)*(InvertedClassMatrix)
} else if(method=="TFIDF"){
  adj <- t(InvertedTermClassMatrix)
} else if(method=="TF"){
  adj <- 1
} else if (method=="TFICF"){
  adj <- InvertedClassMatrix
} else {
  print("Unsupport method")
}
final<-t(ClassFreqMatrix)*adj

final <-t(t(final)*as.numeric(1/DocumentperClass))
#*****************************************
# 4.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadTestData.R")
train_keywords<-unique(rownames(final))
if(enableSemantic==TRUE){
  test_keywords<-colnames(tmp)
  #*****************************************
  # 5.Load up test data for evaluation
  #*****************************************
  # In this step, we extract the single term and replace them with
  # their synonyms that are available within the train keywords
  kw_intersect<- intersect(test_keywords,train_keywords)
  kw_diff <- setdiff(test_keywords,train_keywords)
  # Get Index of single term for train set
  #isSingleTerm1<-unlist(lapply(train_keywords,function(x) length(unlist(strsplit(x,split=" ")))==1))
  # Get Index of single term that are different from train set
  #isSingleTerm2<-unlist(lapply(kw_diff,function(x) length(unlist(strsplit(x,split=" ")))==1))
  # Retrieve single term from both train set and test set
  isTwoTerm2<-unlist(lapply(kw_diff,function(x) length(unlist(strsplit(x,split=" ")))==2))
  #isTwoTerm1<-unlist(lapply(train_keywords,function(x) length(unlist(strsplit(x,split=" ")))==2))
  
  #train_single_terms<-train_keywords[isSingleTerm1]
  #diff_single_terms <- kw_diff[isSingleTerm2]
  #train_single_terms<-train_keywords[isTwoTerm1]
  diff_terms <- kw_diff[isTwoTerm2]
  # Use google search to retrieve original words
  # This process is used as a reverse of stemming since most of public ontology
  # would not understand the stemmed word.
  # unstemmed_words<-getUnstemWords(diff_terms,unstemDict)
  # We perform stemming again our new original words to see if the stemmed version
  # is the same. For words that are not similar after stemming, we discard it
  #validatedTerms<-intersect(wordStem(unstemmed_words),diff_terms)
  # Capture only the valid original version from the stem version
  #unstemmed_words.validated<-unstemmed_words[which(wordStem(unstemmed_words) %in% validatedTerms)]
  # Capture only the stem version
  #stemmed_words.validated<-wordStem(unstemmed_words)[which(wordStem(unstemmed_words) %in% validatedTerms)]
  #*****************************************
  # 6. Retrieve synonyms from thesaurus
  #*****************************************
  #resu<-lapply(diff_terms,function(x) lapply(lapply(unlist(strsplit(x,' ',fixed=TRUE)),function(y) getSimilarCompound(y)),function(z) getCandidatedClasses(z)))
  
  resu <- lapply(diff_terms, function(x) lapply(getRelatedConcepts(x),function(y) getCandidatedClasses(y)))
  
  isDup<-unlist(lapply(resu,function(x)any(duplicated(names(unlist(x))))))
  candidatesTerm<-diff_terms[isDup]
  
  candidatesClass<-unlist(lapply(resu[isDup],function(x)names(unlist(x)[duplicated(names(unlist(x)))])[1]))
  df<-data.frame(term = candidatesTerm,clsID=candidatesClass)
  df$clsID<-as.character(df$clsID)
  df$term<-as.character(df$term)
  relatedConcepts<-lapply(diff_terms,function(x) getRelatedConcepts(x))
  relatedConcepts<-relatedConcepts[isDup]
  cpList<-getSynonymsCP(relatedConcepts,df)
  cpList$clsID<-as.character(cpList$clsID)
  cpList$term1<-as.character(cpList$term1)
  cpList$term2<-as.character(cpList$term2)
  mixTerms<-apply(cpList[2:3],1, paste0,collapse=" ")
  transTabl<-as.data.frame(cbind(df$clsID,df$term,mixTerms,cpList$weighted))
  names(transTabl)<-c("clsID","old_term","new_term","weighted")
  transTabl$weighted<-as.numeric(as.character(transTabl$weighted))
  transTabl$old_term<-as.character(transTabl$old_term)
  transTabl$new_term<-as.character(transTabl$new_term)
  #aggregate(weighted~clsID,data = transTabl,sum)
  tmp1<-data.frame(tmp)
  tmp1$DOCid<-rownames(tmp1)
  colnames(tmp1)<-lapply(colnames(tmp1),function(x) Trim(clean(str_replace_all(x,"[.]"," "))))
  
  mdata<-melt(tmp1,id.vars = "DOCid")
  mdata$variable <- as.character(mdata$variable)
  df.new<-mdata
  for(wIdx in seq(1:nrow(transTabl))){
    oldDF <- mdata[mdata$variable==transTabl[wIdx,c("old_term")],]
    # Save it to new DF
    newDF<-data.frame(DOCid=oldDF$DOCid, variable=rep(transTabl[wIdx,c("new_term")],length(oldDF$DOCid)),value=oldDF$value)
    # For the test set, we remove the original stemmed version of the word
    df.new<- df.new[df.new$variable!=transTabl[wIdx,c("old_term")],]
    # We append the new synonyms to the data frame
    df.new<-rbind(df.new,newDF)
  }
  df.new.agg<-aggregate(value~DOCid+variable,data=df.new,sum)
  # Convert back to wide format
  df.new.wide<-dcast(DOCid~variable,data=df.new.agg,value.var = "value")
  rownames(df.new.wide)<-df.new.wide[,"DOCid"]
  df.new.wide<-df.new.wide[,-1]
  tmp<-df.new.wide
}
dmelt_class<-melt(final)
names(dmelt_class) <- c("keyterm","Id","freq")
tmp[,"Id"]=rownames(tmp)
dmelt_tmp<- melt(tmp,id.vars = c("Id"))
names(dmelt_tmp) <- c("Id","keyterm","freq")
mdata <- rbind(dmelt_tmp,dmelt_class) 

# Convert back to wide
wide_dat <- dcast(mdata,Id~keyterm,value.var = "freq",fill=0)
# Extract test doc and train doc
test <- wide_dat[1:nrow(tmp),]
train <- wide_dat[(nrow(tmp)+1):nrow(wide_dat),]
rownames(train)<-train[,c("Id")]
train<-train[,-1]  
adj <- t(adj)

train_mat <- as.matrix(train[,rownames(final)])

selected_class<-c()
for(i in seq(1,nrow(test))){
  # For each vector d, we compute the eucline distance with our training set
  print(i)
  # We first need to extract the keyterm vector of the testing doc
  # Each testing doc will have different weighting in different classes
  # Therefore, we have to map weighting correspondingly before
  # we can minus its class centroid
  # New terms will be ignored
  tmat <- as.numeric(test[i,rownames(final)])
  # GeT IDF for the corresponding class
  if(method=="TF"){
    weightedMask<-1
  } else if(method=="TFICF"){
    weightedMask <- adj[1,]
  } else {
    weightedMask <- adj[which(rownames(adj)==testLabel),]
  }
  b<-as.numeric(tmat)*weightedMask
  distance <- t(t(train_mat)-b)  # Take exponential of each column
  distance <- apply(distance,2,function(x)x**2)
  # Get squareroot of sum
  selected_class <- c(selected_class,names(which.min(sqrt(rowSums(distance)))))
  print(tail(selected_class,1))
}
# Rocchio Implementation
