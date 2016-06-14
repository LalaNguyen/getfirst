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
# 3.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadTestData.R")
rownames(tmp)<-tmp$patId
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
# classbonus value
re <- t(InvertedTermClassMatrix[,-ncol(InvertedTermClassMatrix)])*(InvertedClassMatrix)
final<-t(ClassFreqMatrix)*re
#*****************************************
# 5. Calculate Eucline Distance
#*****************************************
dmelt_class<-melt(final)
names(dmelt_class) <- c("keyterm","Id","freq")
dmelt_tmp<- melt(tmp,id.vars = c("patId"))
names(dmelt_tmp) <- c("Id","keyterm","freq")
mdata <- rbind(dmelt_tmp,dmelt_class) 

# Convert back to wide
wide_dat <- dcast(mdata, Id ~ keyterm, value.var = "freq", fill=0)
# Extract test doc and train doc
test <- wide_dat[1:nrow(tmp),]
train <- wide_dat[(nrow(tmp)+1):nrow(wide_dat),]
rownames(train)<-train[,c("Id")]
train<-train[,-1]
train_mat <- as.matrix(train)
selected_class<-c()
for(i in seq(1,nrow(test))){
  # For each vector d, we compute the eucline distance with our training set
  print(i)
  tmat <- as.numeric(test[i,-1])
  distance <- t(t(train_mat)-tmat)
  # Take exponential of each column
  distance <- apply(distance,2,function(x)x**2)
  # Get squareroot of sum
  selected_class <- c(selected_class,names(which.min(sqrt(rowSums(distance)))))
  print(tail(selected_class,1))
}

