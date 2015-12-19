1. Run parser to create json file from large XML
2. Run db-client to insert json to MongoDb via --i (use file name only)
MongoDB :
- database : patent_db
- collection : patents
3. Extract single class using db-client via --e 0
4. Run train.py to move single class with decent number of abstracts from class -> train folder
5. Extract single class with date using db-client via --dump 0
6. Navigate to R-script and run the web server via shiny
