#*****************************************
# METHOD 9.One&Only Algorithm Implementation
#*****************************************
# 1.Retrieve documents term from database
#***************************************
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/dbConnect.R")
#*****************************************
# 2.Build Class Frequency Matrix
#*****************************************
# Create a data frame with 3 variables | classId | keyword | class frequency |
ClassFreqDF<-aggregate(freq~path+keyterm,data=clsTree,sum)
# Filter low frequency term if necessary
# ClassFreqDF<- ClassFreqDF[ClassFreqDF$freq>100,]
# Convert to wide format
ClassFreq<-dcast(ClassFreqDF,path~keyterm,value.var="freq",fill=0)
# Move class ID to rownames
rownames(ClassFreq)<-ClassFreq[,1]
ClassFreq <- ClassFreq[,-1]
# Build a Class Frequency Matrix
ClassFreqMatrix <- as.matrix(ClassFreq)
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
InvertedClassMatrix <- log(nrow(ClassFreqMatrix)/colSums(ClassFreqMatrix>0))
# Inverted Class matrix is a vector of keywords with its
# classbonus value22
# Inverted Class matrix is a vector of keywords with its
# classbonus value
final<-t(t(ClassFreqMatrix)*InvertedClassMatrix)
#*****************************************
# 3.Find class with maximum keywords
#*****************************************
mkw <- apply(final, 2, function(x) which.max(x))
# mkw will be a vector of keyterms and the value is 
# the index of the class with the maximum frequency.
# v(keyword:clsIdx)
#*****************************************
# 4.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadTestData.R")
#*****************************************
# 5.Extract keywords and find intersection
#*****************************************
# Get vector of keywords from training set
# and class indices
library("stringr")
keywords<-unique(names(mkw))
# Get vector of keywords from test set 
kw<-colnames(tmp)
# Find intersecting keywords
and <- intersect(keywords,kw)
# Extract keywords in each document that we have information
# on vector(keywords:clsIdx)
test<-tmp[,and]
#*****************************************
# 6.One&Only Algorithm Implementation
#*****************************************
selected_class <- c()
#For each document in the test set
for (i in seq(1:nrow(test))){
  print(i)
  #Filter the keywords of the ith document
  dv<- test[i,]
  #If the keyword frequency is zero, simply remove it
  #because such keyword does not belong to the document
  dvnon_zero<- dv[,dv!=0]
  #Get these keywords'class from mkw vector
  #and calculate the sum of its
  val<-table(mkw[names(dvnon_zero)])
  clsIdx <- as.numeric(names(which.max(val)))
  selected_class<-c(selected_class,rownames(ClassFreqMatrix)[clsIdx])
  
  # Append result to the final results
  print(tail(selected_class,1))
}
