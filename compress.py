import os, sys, getopt
from settings import patent_path
from shutil import copyfileobj
import bz2

for folder in os.listdir(patent_path):
    folder_path = os.path.join(patent_path,folder)
    hasXML = False
    for file_name in os.listdir(folder_path):
        print("Compressing ",file_name)
        if file_name.endswith(".xml"):
            file_path = folder_path+"/"+file_name
            hasXML=True
            # Convert xml file to 7z
            with open(file_path, 'rb') as input:
                with bz2.BZ2File(file_path+'.bz2', 'wb', compresslevel=9) as output:
                    copyfileobj(input, output)
            os.remove(file_path)
