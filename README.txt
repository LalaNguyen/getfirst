Instruction for Reproducing this work
Author: Minh Nguyen

1. Run parser to create json file from large XML
python .\OPAIRS\parser.py --i C:\Users\Administrator\OPAIRS\data\ipg130716\ipg130716.xml --e=json
Run parser to check for json file and create if not exist
python .\OPAIRS\parser.py --i=all --e=json

2. Run db-client to insert json to MongoDb via --i (use file name only)
MongoDB :
- database : patent_db
- collection : patents
db-client.py will also invoke label.py to markup data using algorithm supplied by rake.py
3. Extract single class using db-client via --e 0
4. Run train.py to move single class with decent number of abstracts from class -> train folder
5. Copy OPAIRS/train to OPAIRS/R-script/classes 
6. Navigate to R-script and run the importneo to populate the tree structure (Ontology Layer 4)
7. Then run STORE_PATH_TO_POSTGRESQL produce document matrix as well as path information from Neo4j. This script will read information in OPAIRS/R-scripts/classes
8. Run Map_COmpound_Noun to connect term of compound noun (Ontology Layer 1)
9. Run eval.R for Rocchio method and eval-only for Majority Voting

Useful Command: 
python .\db-client.py --dump 0
.\pg_dump.exe -U postgres -F c -b -v -f db-new.backup postgres
