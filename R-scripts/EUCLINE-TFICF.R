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
final<-t(ClassFreqMatrix)*InvertedClassMatrix
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
train_mat <- as.matrix(train[,rownames(final)])
selected_class<-c()
for(i in seq(1,nrow(test))){
  # For each vector d, we compute the eucline distance with our training set
  print(i)
  tmat <- as.numeric(test[i,rownames(final)])
  b<-as.numeric(tmat)*InvertedClassMatrix
  
  distance <- t(t(train_mat)-b)
  # Take exponential of each column
  distance <- apply(distance,2,function(x)x**2)
  # Get squareroot of sum
  selected_class <- c(selected_class,names(which.min(sqrt(rowSums(distance)))))
  print(tail(selected_class,1))
}

#vc<-ClassFreqMatrix[30,]
#cv<-vc[vc!=0]
#and<-intersect(names(cv),names(icm))
#sum(table(icm[and]))
 # table(icm[and])
  