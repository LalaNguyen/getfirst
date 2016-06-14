# 1.Retrieve documents term from database
#***************************************
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/dbConnect.R")
#*****************************************
# 2.Build Class Term Frequency Matrix
#*****************************************
ClassFreqDF<-aggregate(freq~path+keyterm,data=clsTree,sum)
ClassFreq<-dcast(ClassFreqDF,path~keyterm,value.var="freq",fill=0)
rownames(ClassFreq)<-ClassFreq[,1]
ClassFreq <- ClassFreq[,-1]
# Convert back to matrix for computation
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
re <- t(InvertedTermClassMatrix)
re <- t(re[-nrow(re),])
#*****************************************
# 3.Build Document Frequency Class Matrix
#*****************************************
# Get Unique Document ID with Class
DocumentClassDF<- unique(clsTree[,c("id","path")])
# Get total number of documents for each Class
ClassDocTabl<-as.numeric(table(DocumentClassDF$path))
final <- t(re*1/ClassDocTabl)
#*****************************************
# 4.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadTestData.R")
#
dmelt_class<-melt(final)
names(dmelt_class) <- c("keyterm","Id","freq")
dmelt_tmp<- melt(tmp,id.vars = c("patId"))
names(dmelt_tmp) <- c("Id","keyterm","freq")
mdata <- rbind(dmelt_tmp,dmelt_class) 

# Convert back to wide
wide_dat <- dcast(mdata,Id~keyterm,value.var = "freq",fill=0)
# Extract test doc and train doc
test <- wide_dat[1:nrow(tmp),]
train <- wide_dat[(nrow(tmp)+1):nrow(wide_dat),]
rownames(train)<-train[,c("Id")]
train<-train[,-1]
train_mat <- as.matrix(train[,rownames(final)])
selected_class<-c()
for(i in seq(1,nrow(test))){
  # For each vector d, we compute the eucline distance with our training set
  print(i)
  tmat <- as.numeric(test[i,rownames(final)])
  test_idf <- re[which(rownames(re)==testLabel),]
  b<-as.numeric(tmat)*1
  distance <- t(t(train_mat)-b)
  # Take exponential of each column
  distance <- apply(distance,2,function(x)x**2)
  # Get squareroot of sum
  selected_class <- c(selected_class,names(which.min(sqrt(rowSums(distance)))))
  print(tail(selected_class,1))
}
# Rocchio Implementation
