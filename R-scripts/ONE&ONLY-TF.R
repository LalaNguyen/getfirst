# Finding Class Frequency
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
#*****************************************
# 3.Find class with maximum keywords
#*****************************************
mkw <- apply(ClassFreqMatrix, 2, function(x) which.max(x))
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

test<-tmp[,and]
selected_class <- c()
for (i in seq(1:nrow(test))){
  print(i)
  dv<- test[i,]
  dvnon_zero<- dv[,dv!=0]
  val<-table(mkw[names(dvnon_zero)])
  clsIdx <- as.numeric(names(which.max(val)))
  selected_class<-c(selected_class,rownames(ClassFreqMatrix)[clsIdx])
  print(tail(selected_class,1))
}

