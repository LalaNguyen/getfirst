# 1.Retrieve documents term from database
#***************************************
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/dbConnect.R")
#*****************************************
# 2.Build Class Term Frequency Matrix
#************ *****************************
ClassFreqDF<-aggregate(freq~path+keyterm,data=clsTree,sum)
ClassFreq<-dcast(ClassFreqDF,path~keyterm,value.var="freq",fill=0)
rownames(ClassFreq)<-ClassFreq[,1]
ClassFreq <- ClassFreq[,-1]
# Convert back to matrix for computation
ClassFreqMatrix <- as.matrix(ClassFreq)
#*****************************************
# 3.Build Document Frequency Class Matrix
#*****************************************
# Get Unique Document ID with Class
DocumentClassDF<- unique(clsTree[,c("id","path")])
# Get total number of documents for each Class
ClassDocTabl<-as.numeric(table(DocumentClassDF$path))
re <- ClassFreqMatrix*1/ClassDocTabl
final<-t(re)
#*****************************************
# 4.Load up test data for evaluation
#*****************************************
# This function will return a matrix of document-term
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadTestData.R")

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
  distance <- t(t(train_mat)-tmat)
  # Take exponential of each column
  distance <- apply(distance,2,function(x)x**2)
  # Get squareroot of sum
  selected_class <- c(selected_class,names(which.min(sqrt(rowSums(distance)))))
  print(tail(selected_class,1))
  }
# Rocchio Implementation

