library("RNeo4j")
setwd('C:/Users/Administrator/OPAIRS/R-scripts/classes')
folders <- list.dirs(path=".", recursive = TRUE)
## Filter class name from dir list, Retrieve the last element
## Nested classes do not supported
classId <- lapply(folders,function(x) unlist(strsplit(x,split="/"))[2])
## Remove the root class or unknown class
classId <- na.omit(unlist(classId))

## User confidential for neo4j
uname <- "neo4j"
pass <- "makata0611"
## Start Neo4j
graph = startGraph("http://localhost:7474/db/data/", username = uname, password =pass)
clear(graph)

## Create node for each class
cls345173 <- createNode(graph,'Class',name='345173')
cls370252 <- createNode(graph,'Class',name='370252')
cls370328 <- createNode(graph,'Class',name='370328')
cls370338<- createNode(graph,'Class', name='370338')
cls370329 <- createNode(graph,'Class',name='370329')
cls370330 <- createNode(graph,'Class',name='370330')
cls370331 <- createNode(graph,'Class',name='370331')
cls370332 <- createNode(graph,'Class',name='370332')

cls382100<- createNode(graph,'Class', name='382100')
cls382131<- createNode(graph,'Class', name='382131')
cls382103 <- createNode(graph,'Class',name='382103')
cls382128 <- createNode(graph,'Class',name='382128')

cls358115<- createNode(graph,'Class', name='358 115')

cls37524016<- createNode(graph,'Class', name='37524016')
cls375260<- createNode(graph,'Class', name='375260')
cls375267<- createNode(graph,'Class', name='375267')

cls606200<- createNode(graph,'Class', name='606200')

cls70094<- createNode(graph,'Class', name='700 94')

cls70219<- createNode(graph,'Class', name='702 19')

cls7032<- createNode(graph,'Class', name='703 2')

cls7052<- createNode(graph,'Class', name='705 2')
cls7054<- createNode(graph,'Class', name='705 4')
cls705141<- createNode(graph,'Class', name='705 141')
cls705261<- createNode(graph,'Class', name='705 261')
cls70535<- createNode(graph,'Class', name='705 35')
cls70537<- createNode(graph,'Class', name='705 37')
cls70612<- createNode(graph,'Class', name='706 12')
cls70645<- createNode(graph,'Class', name='706 45')
cls709223<- createNode(graph,'Class', name='709223')
cls709206<- createNode(graph,'Class', name='709206')
cls709224<- createNode(graph,'Class', name='709224')

cls7181<- createNode(graph,'Class', name='718 1')











## Create relation for each class

createRel(cls370329,"IS_CHILD_OF",cls370328)
createRel(cls370338,"IS_CHILD_OF",cls370328)
createRel(cls370330,"IS_CHILD_OF",cls370329)
createRel(cls370331,"IS_CHILD_OF",cls370329)
createRel(cls370332,"IS_CHILD_OF",cls370331)
createRel(cls382103,"IS_CHILD_OF",cls382100)
createRel(cls382128,"IS_CHILD_OF",cls382100)
createRel(cls382131,"IS_CHILD_OF",cls382128)


createRel(cls375267,"IS_CHILD_OF",cls375260)
createRel(cls70537,"IS_CHILD_OF",cls70535)
createRel(cls709224,"IS_CHILD_OF",cls709223)

