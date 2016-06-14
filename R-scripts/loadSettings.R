library(tm)
library(qdap)
library(stringr)
library(XML)
library(RCurl)
library(SnowballC)
library(wordnet)
require("RPostgreSQL")
APITheSaurus <-  "d6790623760eae7d2f91757730a177b3/"
APILink <-"https://words.bighugelabs.com/api/2/"
APIFormat <-"/xml"
sql_pass <- "makata0611"
sql_user <- "postgres"
setDict("C:/dict/")
googlesearch<-"https://www.google.com/search?q=define+"
outDir <- "C:/Users/Administrator/OPAIRS/R-scripts/testclass/606200/"
testLabel<-"606200"

