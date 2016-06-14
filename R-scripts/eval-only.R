#***************************************
# Finding Class Frequency
# 1.Retrieve documents term from database
#***********s****************************
library(reshape2)
library("RNeo4j")
library("data.table")
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
enableO4=FALSE
#method ="TFIDFICF" 
method ="TFICF" 
#method ="TFIDF" 
#method = "TF" 
# Choose to enable semantic enhancement
re <-list()
clsTree <- fetchData()
total.docs <- length(unique(clsTree$id))
no.train_docs <- round(total.docs*80/100)
no.test_docs <- round(total.docs*20/100)

for(i in seq(1:54)){
  print(i)
  train.patId <- sample(unique(clsTree$id),no.train_docs, replace=FALSE)
  test_docs <- clsTree[!(clsTree$id %in% train.patId),]
  train_docs <-clsTree[clsTree$id %in% train.patId,]
  test_docs$keyterm<-CleanWord(test_docs$keyterm,"[[:punct:]]")
  test_docs <- test_docs[!(test_docs$freq==0),]
  test_docs <- test_docs[test_docs$keyterm!="",]
  # Sorted Label
  testLabel=unique(test_docs[,c("path","id")])
  testLabel <- testLabel[order(testLabel$id,decreasing = FALSE),]
  #------
  tmp <- dcast(test_docs,id~keyterm,value.var = "freq",fun.aggregate = sum)
  rownames(tmp)<-tmp[,1]
  tmp<-tmp[,-1]
  table(unique(test_docs[,c("path","id")])$path)
  
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
  ClassFreqMatrix<-getClassFrequencyMatrix(train_docs)
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
  DocumentperTermDF<-getDocumentsperTerm(train_docs)
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
  DocumentperClass<-getDocumentsperClass(train_docs)
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
#*****************************************
# 3.Find class with maximum keywords
#*****************************************
mkw <- apply(final, 1, function(x) which.max(x))
# mkw will be a vector of keyterms and the value is 
# the index of the class with the maximum frequency.
# v(keyword:clsIdx)
#*****************************************
# 4.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
#*****************************************
# 5.Extract keywords and find intersection
#*****************************************
# Get vector of keywords from training set
# and class indices
train_keywords<-unique(names(mkw))
if(enableSemantic==TRUE){
  print("Find semantic")
  test_keywords<-colnames(tmp)
  #*****************************************
  # 5.Load up test data for evaluation
  #*****************************************
  # In this step, we extract the single term and replace them with
  # their synonyms that are available within the train keywords
  kw_intersect<- intersect(test_keywords,train_keywords)
  kw_diff <- setdiff(test_keywords,train_keywords)
  # Get Index of single term for train set
  isTwoTerm2<-unlist(lapply(kw_diff,function(x) length(unlist(strsplit(x,split=" ")))==2))
  
  diff_terms <- kw_diff[isTwoTerm2]
  #*****************************************
  # 6. Retrieve synonyms from thesaurus
  #*****************************************
  subdata<-diff_terms
  diff_terms<-sample(subdata,3000,replace=FALSE)
  print("finding Probable concept with sampling of 2000 unknown words")
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
  print("Finish TransTabl")
  print(transTabl)
  aggregate(weighted~clsID,data = transTabl,sum)
  tmp1<-data.frame(tmp)
  tmp1$DOCid<-rownames(tmp1)
  colnames(tmp1)<-lapply(colnames(tmp1),function(x) Trim(clean(str_replace_all(x,"[.]"," "))))
  
  mdata<-melt(tmp1,id.vars = "DOCid")
  mdata$variable <- as.character(mdata$variable)
  mdata<-mdata[mdata$value!=0,]
  df.new<-mdata
  # Remove 0 frequency keyword
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
  print("Prepend keyword to test set")
  df.new.wide<-dcast(DOCid~variable,data=df.new.agg,value.var = "value",fill = 0)
  rownames(df.new.wide)<-df.new.wide[,"DOCid"]
  df.new.wide<-df.new.wide[,-1]
  tmp<-df.new.wide
  
}
test_keywords<-colnames(tmp)
and <- intersect(test_keywords,train_keywords)
test<-tmp[,and]

#*****************************************
# 6.One&Only Algorithm Implementation
#*****************************************
doc_test_pred1 <- list()
#For each document in the test set
  for (k in seq(1:nrow(test))){
    #Filter the keywords of the ith document
    dv<- test[k,]
    #If the keyword frequency is zero, simply remove it
    #because such keyword does not belong to the document
    dvnon_zero<- dv[,dv!=0]
    #Get these keywords'class from mkw vector
    #and calculate the sum of its
    val<-table(mkw[names(dvnon_zero)])
    clsIdx <- as.numeric(names(which.max(val)))
    #selected_class<-c(do,rownames(ClassFreqMatrix)[clsIdx])
    doc_test_pred1[[k]] <- data.frame(id=rownames(dvnon_zero),path=rownames(ClassFreqMatrix)[clsIdx])
    # Append result to the final results
  }
doc_test_pred = Reduce(function(...) merge(..., all=T), doc_test_pred1)
print(doc_test_pred[!(doc_test_pred)%in% unlist(doc_test_pred)])

sorted.pred<-as.character(doc_test_pred[order(doc_test_pred$id,decreasing = FALSE),c("path")])
a<-setdiff(testLabel$id,as.character(doc_test_pred$id))
testLabel<-testLabel[!testLabel$id %in% a,]
testLabel<-as.character(testLabel[order(testLabel$id,decreasing = FALSE),c("path")])

if(enableO4==TRUE){
  DT1 <- data.table(prediction=sorted.pred,truth=testLabel)
  DT1[, test:=grepl(truth,prediction),by=truth]
  sorted.pred[DT1$test==TRUE] <- DT1[test==TRUE,truth]  
  
} else {
}
prediction <- as.factor(sorted.pred)
truth <- as.factor(testLabel)
tax<-as.matrix(table(prediction,truth))
# Correct Prediction with correct label
TP <- diag(tax)
#Prediction with wrong mark on class
FP <- rowSums(tax)-TP
#Number of rejected classes that should be true
FN <- colSums(tax)-TP
#Number of correct rejected class
TN <- c()
for (s in seq(1,nrow(tax))){
  TN <-c(TN,sum(colSums(tax[-s,-s])))
}
names(TN)<-rownames(tax)
Accuracy <- sum(TP)/sum(rowSums(tax))
Recall <- TP/(TP+FN)
Precision <- TP/(TP+FP)
Specificity <- TN/(TN+FP)
F1 = 2*Precision*Recall/(Precision+Recall)
re[[i]] <- data.frame(no.class=nrow(tax),accuracy=Accuracy, total.fp=sum(FP),total.fn=sum(FN), micro.f1 = Accuracy, mac.f1=mean(F1,na.rm = TRUE))
print(re)
}