# Load/install necessary packages
library(devtools)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(plotly)
library(stringr)
library(qdap) # Clean and trim dataset
library("RNeo4j")
library(rowr)
library(SnowballC)
## User confidential for neo4j
uname <- "neo4j"
pass <- "makata0611"
## Start Neo4j
graph = startGraph("http://localhost:7474/db/data/", username = uname, password =pass)
final <- read.csv(file = "C:/Users/Administrator/OPAIRS/R-scripts/web/tm.csv",sep=" ",stringsAsFactors = FALSE,header = TRUE)
final <- as.matrix(final)

shinyServer(function(input, output,session) {

  classes<-reactive({
    if(length(input$selectize)==0){
      return()
    }
    and <-  intersect(input$selectize,rownames(final))
    clsLen<-sqrt(colSums(apply(final,2,function(x)x**2)))
    # A. Compute the nominator: D.c
    #test_idficf <- re[and,testLabel]
    qMatrix<-t(as.matrix(table(input$selectize)))
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

    docLen<-sqrt(sum(apply(qMatrix,2,function(x)x**2)))
    
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
    denominator <- t * docLen
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
    colnames(s)<-Trim(gsub("\\."," ",gsub("X"," ",colnames(s))))

    return(data.frame(s[,s!=0]))
})
  preprocess_data<-reactive({
    q1 <- input$selectize
    if(length(q1)==0){
      return()
    }
    clean_word<-unlist(lapply(q1, function(x)str_replace_all(x, "[[:punct:]]", " ")))
    b<-unlist(lapply(clean_word,function(x)Trim(clean(x))))
    tokens <-unlist(strsplit(b," "))
    tokens <- unlist(lapply(tokens,function(x) wordStem(x,language="porter")))
    return(tokens)
  })
  concepts<-reactive({

    # for each keyword, we generate a set of subconcepts
    tokens<-preprocess_data()
    classArr <- rownames(classes())
    if(length(tokens)==0){
      return()
    }
    associate_set<-data.frame()
    for(clsidx in classArr){
    for (token in unique(tokens)){
      # we take all the document IDs that containing of the keyword
      # For example, the keyword is healthcare (stem:healthcar)
      # Construct Neo4j Query that starting with our keyword
      # Trim trailing and leading white space
      # we then have a list of individual terms
      # for each term, we find its 1st degree associations
      q = paste0("MATCH (n:Concept{word:'",token,"',class:'",clsidx,"'})-[r:IS_ASSOCIATED_WITH]-(m)
                   RETURN n.word,m.word,r.weighted")
        terms<-cypherToList(graph,query = q)
        terms <- lapply(terms,function(x)paste0(unlist(x),collapse = " "))
        ws <- as.numeric(unlist(lapply(terms,function(x)tail(unlist(strsplit(x,split=" ")),1))))
        cns <- unlist(lapply(terms,function(x) paste0(head(unlist(strsplit(x,split=" ")),2),collapse=" ")))
        cls <- rep(clsidx,length(ws))
        df<-data.frame(do.call(cbind, list(cns,ws,cls)))
        names(df)<-c("term","weighted","class")
        associate_set<-rbind(associate_set,df)
    }
    }
    associate_set
      return(associate_set)
  })
  output$text1 <- DT::renderDataTable(
    DT::datatable(classes(), options = list(pageLength = 25))
  )
  output$text2<- DT::renderDataTable({
    DT::datatable(concepts(), options = list(pageLength = 25))
  })
  

  updateSelectizeInput(session, 'selectize', choices = unique(rownames(final)), server = TRUE)
})