1. Run parser to create json file from large XML
python .\OPAIRS\parser.py --i C:\Users\Administrator\OPAIRS\data\ipg130716\ipg130716.xml --e=json
Run parser to check for json file and create if not exist
python .\OPAIRS\parser.py --i=all --e=json

2. Run db-client to insert json to MongoDb via --i (use file name only)
MongoDB :
- database : patent_db
- collection : patents
3. Extract single class using db-client via --e 0
4. Run train.py to move single class with decent number of abstracts from class -> train folder
5. Extract single class with date using db-client via --dump 0
6. Navigate to R-script and run the importneo to populate the tree structure
7. Then run agen.R to produce document matrix
python .\db-client.py --dump 0
.\pg_dump.exe -U postgres -F c -b -v -f db-new.backup postgres
