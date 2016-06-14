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
no.train_docs <- round(length(unique(clsTree$id))*70/100)
no.test_docs <- round(length(unique(clsTree$id))*30/100)
train_ind <- sample(clsTree$id,no.train_docs,replace=FALSE)
df.train <- clsTree[train_ind,]
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
#*****************************************
# 3. Penalize User keywords with frequency we known
#*****************************************
#Get Intersect between test_idficf with our classifier
#*****************************************
# 4.Clean up keywords
#*****************************************
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
aggregate(weighted~clsID,data = transTabl,sum)
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
#str <- unlist(strsplit(test,' ',fixed=TRUE))
#test<-lapply(str,function(x) getSimilarCompound(x))
#candidates<-lapply(test,function(x)getCandidatedClasses(x))
#candidates         #20,1,6,42, 128, 132, 25, 159, 161, 213    
#synsets<-c()
#idxArr<-c()
# for each validated word, we send request to thesaurus for synonym
# if(initDict()){
# for (i in unstemmed_words.validated){
#   re <- getThesaurusSynonyms(i)
#   if(re!=""){
#     html<-htmlTreeParse(re,useInternalNodes = TRUE)
#     new_sets<-xpathSApply(html,"//w",xmlValue)
#     synsets<- c(synsets,new_sets)
#     idxArr<-c(idxArr,rep(i,length(new_sets)))
#   } else{
#     synsets<-c(synsets,i)
#     idxArr<-c(idxArr,i)
#   }
# }
# }
# # Clean and stemming the new set of vocabularies
# synsets<-sapply(synsets,function(x)paste0(wordStem(unlist(strsplit(x,split = " "))),collapse = " "))
# synsets<-sapply(synsets,function(x) Trim(clean(x)))
# semanticDF<-cbind(idxArr,synsets)
# # After finding the synsets, we need to check whether synsets keywords
# # are known to our weighting system
# extends<-intersect(semanticDF[,"synsets"],train_single_terms)
# idx<-match(extends,semanticDF[,"synsets"])
# matched<-semanticDF[idx,]
# 
# # After this step, we began to choose which synonym is best match for replacement
# # We use Levenstein to choose based on operations and string similarity
# 
# tmp1<-data.frame(tmp)
# tmp1$DOCid<-rownames(tmp1)
# mdata<-melt(tmp1,id.vars = "DOCid")
# df.new<- mdata
# # Create a data frame to hold 
# # 1. idxArr: original word
# # 2. synsets: synonym of the orignal word
# df<-data.frame(matched,stringsAsFactors = FALSE)
# selected_synsets<-c()
# # For each unique keyword in idxArr,
# for(w in unique(df$idxArr)){
#   # Get index of stemmed word from the original terms list (unstemmed)
#   unstemmedIdx<-grepl(w,unstemmed_words.validated)
#   # Since stemmed_words and original words share the same idx
#   # We get the stemmed word base on index obtained from original term
#   stemmedVersion<- stemmed_words.validated[unstemmedIdx][1]
#   # Retrieve a vector of synsets for the word 'i'
#   subdf<-df[df$idxArr==w,"synsets"]
#   # Formulate distance matrix between the original version of a word and its synsets
#   dist<-as.numeric(adist(stemmedVersion,subdf, costs = list(insertions=1, deletions=1,substitutions=2)))
#   # Find the most minimum distance for the synTerm 
#   synTerm<-subdf[which.min(dist)]
#   # Add syn Term to our selected synset
#   selected_synsets<-c(selected_synsets,synTerm)
#   # Make a copy of original stemmed word and its frequency
#   oldDF <- mdata[mdata$variable==stemmedVersion,]
#   # Save it to new DF
#   newDF<-data.frame(DOCid=oldDF$DOCid, variable=rep(synTerm,length(oldDF$DOCid)),value=oldDF$value)
#   # For the test set, we remove the original stemmed version of the word
#   df.new<- df.new[df.new$variable!=stemmedVersion,]
#   # We append the new synonyms to the data frame
#   df.new<-rbind(df.new,newDF)
# }
# # Some synonyms might be overlapped with the terms exsited in the test set
# # We aggregate data
# df.new.agg<-aggregate(value~DOCid+variable,data=df.new,sum)
# # Convert back to wide format
# df.new.agg$variable<-Trim(str_replace_all(df.new.agg$variable,"[[:punct:]]", " "))
# df.new.wide<-dcast(DOCid~variable,data=df.new.agg,value.var = "value")
# rownames(df.new.wide)<-df.new.wide[,"DOCid"]
# df.new.wide<-df.new.wide[,-1]
# tmp<-df.new.wide
#*****************************************
# 5. Calculate Cosine Similarity
#*****************************************
# The formular is s = (D.c)/(|docLen|*|clsLen|)
# Calculate cross product of D.c = D1.c1+D2.c2+...+Dn.cn
# If term is not visiable in either D or c, we can safely
# ignore it
}
test_keywords<-colnames(tmp)
and <- intersect(test_keywords,train_keywords)
# Calculate Class Length |c| by first double each element in cell
# Calculating total frequency and taking square root
# final frame has the format:
#      | cls1 | cls2 | cls3 |
# ---------------------------
# w1   | 1    |  2   |  3   |
# w2   | 2    |  4   |  6   |
# After applying the power of 2 function:
#      | cls1 | cls2 | cls3 |
# ---------------------------
# w1   | 1    |  4   |  9   |
# w2   | 4    | 16   | 36   |
# After applying colSums, we have total frequency for each class
# cls1 = 5, cls2 = 20, cls3 = 45
clsLen<-sqrt(colSums(apply(final,2,function(x)x**2)))

# A. Compute the nominator: D.c
# We extract the weighting method for particular class
if(method=="TF"){
  weightedMask<-1
} else if(method=="TFICF"){
  weightedMask <- adj[and]
} else {
  weightedMask <- adj[and,testLabel]
}
qMatrix<-as.matrix(tmp[,and])
# Normalize qMatrix 
qMatrix <- t(t(qMatrix)*weightedMask)
clsMatrix <- final[and,]
# qmatrix shape:
#     | w1 | w2 | w3 |
#---------------------
# d1  | 1  | 2  | 3  |
# d2  | 2  | 3  | 1  |
# clsMatrix Shape:
#      | cls1 | cls2 | cls3 |
# ---------------------------
# w1   | 1    |  4   |  9   |
# w2   | 4    | 16   | 36   |
# After multiplication, each cell hold the dot product of a document with a list of class
nominator <- qMatrix %*% clsMatrix
# Calculate doc length for each document
# docLen shape:
#     | w1 | w2 | w3 |
#---------------------
# d1  | 1  | 2  | 3  |
# d2  | 2  | 3  | 1  |
# After calculating sums:
# d1 = 6, d2 = 6
# Each word is now a unit vector, we can calculate
# total frequency for docLen 

docLen<-sqrt(rowSums(apply(qMatrix,2,function(x)x**2)))
# B. Compute the denominator: | docLen | * | clsLen |
# Because we want each docLen to multiply with each class.
# In order to multiply vector docLen and vector clsLen
# We first fill data with clsLen value . Thus t matrix between
# class and test document:
#         | d1 | d2 | d3 | d4 ...
#---------------------------------
# cls 1   | a  | a  | a  | a
# cls 2   | b  | b  | b  | b

t<-matrix(data = clsLen, nrow = length(clsLen), ncol=length(docLen), byrow=FALSE)
# Create a diagonal of docLen
# docLen_diag is:
#      | d1 | d2 | d3 |...
#-------------------------
# d1   | 1  | 0  |  0 |
# d2   | 0  | 4  |  0 |
# d3   | 0  | 0  |  2 |
# The reason when we create a diag is that when we multiply
# t-matrix to diag, we have the particular clsLen * docLen
# for each cell
denominator <- t %*% diag(docLen)
rownames(denominator)<-names(clsLen)
colnames(denominator)<-names(docLen)
# C. Calculate final score
# Our denominator is now in form of 
#        | d1 | d2 | d3 |...
#-------------------------
# cls1   | 1  | 2  |  1 |
# cls2   | 4  | 4  |  5 |
# cls3   | 3  | 2  |  2 |
# Where each cell store the |docLen| * |clsLen|
# While our nominator is in form of
#      | cls1 | cls2 | cls3 |...
#----------------------------
# d1   | 1    | 2    |  1   |
# d2   | 4    | 4    |  5   |
# d3   | 3    | 2    |  2   |
# Where each cell store the D.c
# We perform bitwise division
s<-nominator/t(denominator)
doc_test_pred<-apply(s,1,function(x) names(which.max(x)))
print(table(unlist(doc_test_pred)))


