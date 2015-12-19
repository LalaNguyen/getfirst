from pymongo import MongoClient
import os, sys, getopt
from settings import patent_path,class_path,train_path
import json
import pymongo
import os.path
import csv
from label import markup
client = MongoClient()
client = MongoClient('localhost', 27017)
db = client["patent_db"]
patents = db.patents

abs_path= patent_path
results = []
def insert(file_name):
    flag=False
    if file_name == "0":
        for folder in os.listdir(abs_path):
            folder_path = abs_path+"/"+folder
            for file in os.listdir(folder_path):
                if file.endswith(".json"):
                    print("Check file: "+file),
                    with open(folder_path+"/"+file,'r') as data_file:
                        data = json.load(data_file)
                        try:
                            results = patents.insert_many(data,ordered=False)
                            print(len(results.inserted_ids))
                        except pymongo.errors.BulkWriteError as bwe:
                            print("Inserted: %d/%d" %(bwe.details["nInserted"],(len(data))))
    else:
        for folder in os.listdir(abs_path):
            folder_path = os.path.join(abs_path,folder)
            for file in os.listdir(folder_path):
                print("Check file: "+file+", "+str(flag))
                file_path = os.path.join(folder_path,file)
                if file.endswith(".json") and file_name==file:
                    flag=True
                if flag == True:
                    if file.endswith(".json"):
                        with open(file_path,'r') as data_file:
                            data = json.load(data_file)
                            try:
                                results = patents.insert_many(data,ordered=False)
                                print(len(results.inserted_ids))
                            except pymongo.errors.BulkWriteError as bwe:
                                print("Inserted: %d/%d" %(bwe.details["nInserted"],(len(data))))


#bin\mallet train-topics  --input 345173.mallet  --num-topics 20 --optimize-interval 20 --output-state 345173-topic-state.gz  --output-topic-keys 345173_keys.txt --output-doc-topics 345173_composition.txt
def get_single_class_with_date():
    pipeline=[
        {'$match':{'$and':[{'abstract':{'$exists':True}},{'us_classification':{'$size':1}}]}},
        {'$group':{
            "_id":"$us_classification",
            'abstracts':{
            '$push':{
                'patid':'$_id',
                'date':'$date-published'
            }}
        }}]
    for uspc in patents.aggregate(pipeline=pipeline):
        path = train_path+uspc['_id'][0]
        if not os.path.exists(path):
            pass
        else:
            with open(path+"/"+uspc['_id'][0]+".csv","w") as csvfile:
                fieldnames = ['id', 'date']
                writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
                writer.writeheader()
                for patent in uspc['abstracts']:
                    writer.writerow({'id':patent['patid'],'date':patent['date']})


def get_single_class():
    pipeline=[
        {'$match':{'$and':[{'abstract':{'$exists':True}},{'us_classification':{'$size':1}}]}},
        {'$group':{
            "_id":"$us_classification",
            'abstracts':{
            '$push':{
                'abstract':'$abstract',
                'patid':'$_id'
            }}
        }}]
    for uspc in patents.aggregate(pipeline=pipeline):
        path = class_path+uspc['_id'][0]
        if not os.path.exists(path):
            os.makedirs(path)
        for patent in uspc['abstracts']:
            fp = os.path.join(path,patent['patid'])
            with open(fp+".txt","w") as outfile:
                # Before writing to file, markup " " with noun phases
                # using RAKE technique.
                re = markup(patent['abstract'])
                outfile.write(re)

def main(argv):
    # define local variables
    inputfile = 0
    try:
        opts, args = getopt.getopt(argv,"hi:",["input=","extract=","dump=","help"])
        #Empty input, raise error
        if opts == []:
            print('[Usage:] client-db.py --i=<inputfile.json>')
            sys.exit(2)
    except getopt.GetoptError:
        print('[Usage:] client-db.py --i=<inputfile.json>')
        sys.exit(2)
    for opt, arg in opts:
        if opt == '--help':
            print('[Example Usage] ./parse_patent.py \n\
            --input=/path/to/data.json  # Specify path to data for processing\n'
            )
            sys.exit()
        elif opt in ("--input, -i"):
            inputfile = arg
            insert(inputfile)
        elif opt in ("--extract, -e"):
            get_single_class()
        elif opt in ("--dump, -d"):
            get_single_class_with_date()

if __name__ == "__main__":
    main(sys.argv[1:])
