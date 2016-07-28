library(stringr)
library(qdap)
library(SnowballC)
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/loadSettings.R")
source(file = "C:/Users/Administrator/OPAIRS/R-scripts/helper.R")
printDebug<-function(msg,debug){
  debug<-TRUE
  if(debug==TRUE){
    print(msg)
  }
}
## User confidential for neo4j
uname <- "neo4j"
pass <- "makata0611"
## Start Neo4j
graph = startGraph("http://localhost:7474/db/data/", username = uname, password =pass)

pasteNeo <- function(x){
  # Divide data into managable attributes
  t <- as.character(x["keyterm"])
  clsId <- as.character(x["path"])
  w <- as.numeric(final[t,clsId])
  str <- unlist(strsplit(t,' ',fixed=TRUE))
  # Add validation for two concepts if exist
  # We will check the existing relation and update the weight
  # Else we process with creating new relation
  term1 <- GetOrCreateNode(graph,str[1],clsId)
  term2 <- GetOrCreateNode(graph,str[2],clsId)
  # check if the relation is exist between 2 entities
  q <- paste0("Match (n:Concept{word:'",term1[[1]]$n$word,"',class:'",term1[[1]]$n$class,
              "'})-[r:IS_ASSOCIATED_WITH]-(m:Concept{word:'",term2[[1]]$n$word,"',class:'",term2[[1]]$n$class,"'}) Return r")
  re<-cypherToList(graph,query = q)
  if(length(unlist(re))!=0){
    # Check if the existing length is larger than the weight
    if(unlist(re)["r.weighted"]< w){
      debugMsg<- paste0("Duplicate relationship between '",term1[[1]]$n$word, "' and '",term2[[1]]$n$word,"'")
      printDebug(debugMsg)
      #debugMsg<- paste0("Compare existing weight: '",unlist(re)["r.weighted"], "' and new weight: '",w,"'")
      #printDebug(debugMsg)
      debugMsg<- paste0("Larger value found! Add the relationship")
      printDebug(debugMsg)
      createRel(term2[[1]]$n,"IS_ASSOCIATED_WITH",term1[[1]]$n, weighted=w) 
    } else {
      # Do nothing
      debugMsg<- paste0("Duplicate relationship between '",term1[[1]]$n$word, "' and '",term2[[1]]$n$word,"'")
      printDebug(debugMsg)
      #debugMsg<- paste0("Compare existing weight: '",unlist(re)["r.weighted"], "' and new weight: '",w,"'")
      #printDebug(debugMsg)
      debugMsg<- paste0("Smaller value found! Keep the relationship")
      printDebug(debugMsg)
    }
    # Update the weight and direction
  } else {
    debugMsg<- paste0("Create relationship between '",term1[[1]]$n$word, "' and '",term2[[1]]$n$word,"'")
    printDebug(debugMsg)
    createRel(term1[[1]]$n,"IS_ASSOCIATED_WITH",term2[[1]]$n, weighted=w) 
  }
}

GetOrCreateNode<-function(graph, key1, key2){
  q <- paste0("Match (n:Concept{word:'",key1,"',class:'",key2,"'}) Return n")
  re<- cypherToList(graph,q)
  # If node does not exist, perform insert
  if(length(re)==0){
    q <- paste0("CREATE (n:Concept{word:'",key1,"',class:'",key2,"'}) Return n")
    re<- cypherToList(graph,q)
    return(re)
  } else {
    return(re)
  }
}
clsTree <- fetchData()
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
method="TFIDFICF"
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
clsTree$keyterm<-CleanWord(clsTree$keyterm,"[[:punct:]]")

isTwoTerm1<-unlist(lapply(clsTree$keyterm,function(x) length(unlist(strsplit(x,split=" ")))==2))
twoterms<-clsTree[isTwoTerm1,]
className<-unique(clsTree$path)
for(clsIdx in className)
{
  debugMsg<- paste0("Parse ontology for class: ", clsIdx)
  printDebug(debugMsg)
  # Replace special characters
  class_specific_keyterms <- twoterms[twoterms$path==clsIdx,]
  # replicate the frequency so we have full association
  re <- apply(class_specific_keyterms,1,function(x)pasteNeo(x))
}
