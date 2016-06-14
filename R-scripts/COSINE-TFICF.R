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
tmp$patId <- NULL
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
# 4.Clean up keywords
#*****************************************
library("stringr")
keywords<-unique(rownames(final))
kw<-colnames(tmp)
#*****************************************
# 5. Calculate Cosine Similarity
#*****************************************
# The formular is s = (D.c)/(|docLen|*|clsLen|)
# Calculate cross product of D.c = D1.c1+D2.c2+...+Dn.cn
# If term is not visiable in either D or c, we can safely
# ignore it
and<- intersect(kw,keywords)
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
test_icf <- InvertedClassMatrix[and]
qMatrix<-as.matrix(tmp[,and])
# Normalize qMatrix 
qMatrix <- t(t(qMatrix)*1)
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
table(unlist(doc_test_pred))
