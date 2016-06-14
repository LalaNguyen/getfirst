#!/usr/bin/env python

"""
    parse-patent
    ~~~~~~~~~~~~~~~
    Work with specific xml 4.5 DTD of US patent
"""

try:
    from io import StringIO
except ImportError:
    from cStringIO import StringIO
import sys, getopt,os
import xml.sax
import json
import copy
import re
import collections
from collections import defaultdict
from settings import patent_path,class_path,train_path

#Define settings and global variables
NEO4J_URL = "http://localhost:7474/db/data"
MAX_NUMBER_OF_PATENTS = True
MAX_NUMBER_OF_OUTPUT_FILE = 50
abs_path= patent_path

class PatentHandler(xml.sax.ContentHandler):
    """Patent Handler creates a handler to hold formatted data that are parsed from xml file.
    Current captured XML tags:
    <us-patent-grant>
    <date-publ>
    <file>
    <country>
    <publication-reference>
    <doc-number>
    <kind>
    <inventor>
    <invention-title>
    <number_of_claims>
    <us-references-cited>
    <classification-national>
    """
    def __init__(self):
        self.CurrentData = ""
        self.date_produced = ""
        self.country = ""
        self.class_counter=0
        self.date_publ = ""
        self.invention_title = ""
        self.appl_type=""
        self.application_number = ""
        self.doc_number = ""
        self.us_classification = []
        self.kind = ""
        self.abstract=""
        # Use stack to store opening tag and its value
        self.stack = defaultdict(list)
        self.enable_stack =False
        self.ls = []

    # Call when an element starts
    def startElement(self, tag, attributes):
        self.CurrentData = tag
        if tag == "us-patent-grant":
            self.date_produced = attributes["date-produced"]
            self.date_publ = attributes["date-publ"]
            self.application_number = attributes["file"]
            self.country = attributes["country"]

        if tag == "abstract":
            self.stack.clear()
            self.count=0
            self.stack["abstract"]="abstract"
            self.enable_stack = True

        if tag == "application-reference":
            self.appl_type=attributes["appl-type"]

        if tag == "publication-reference":
            #We found what we want, set our stack ready
            self.stack.clear()
            self.enable_stack = True

        if tag == "us-term-of-grant":
            self.stack.clear()
            self.stack["us-term-of-grant"]=1

        if tag == "classification-national" and "us-term-of-grant" in self.stack:
            self.enable_stack= True


    # Call when a character is read
    def characters(self, content):

        #For tags which occurs many times in the instance or nested,
        #we use stack. Stack only stores opening tag and value,
        #just ignore the closing tag
        if self.enable_stack==True:
            if content != '\n':
                # Remove special characters
                content = re.sub('[^A-Za-z0-9 \-./]+', ' ', content)
                if "abstract" in self.stack:
                    self.ls.append(content)
                else:
                    self.stack[self.CurrentData].append(content)



    # Call when an element ends
    def endElement(self, tag):
        self.CurrentData = tag
        #End of desired tag, let's close our stack.
        if tag == "publication-reference":
            self.enable_stack = False
            # assign doc-number and kind before clearing stack
            self.doc_number = self.stack['doc-number']
            self.kind = self.stack['kind']
            self.stack.clear()

        if tag == "abstract":
            self.abstract=" ".join(self.ls)
            self.enable_stack = False
            self.count=0
            self.stack.clear()

        #Meet end tag of classification-national, clear stack
        if tag == "invention-title":
            self.enable_stack = False
            #print(self.stack)
            # When invention-title is single tag
            if "invention-title" in self.stack:
                self.invention_title = self.stack["invention-title"][0]
            # When invention-title has child tag
            else:
                self.invention_title +=" ".join(item for item in self.stack["i"])
            # if this is D patent, we must be able to place stack in classification-locarno
            if "main-classification" in self.stack:
                self.us_classification.extend(self.stack["main-classification"])
            if "further-classification" in self.stack and "invention-title" in self.stack:
                self.us_classification.extend(self.stack["further-classification"])
            # If this is RE & PP do something else
            self.stack.clear()

    # Reset everything to initial state.
    def reset(self):
        #self.inventor_list[:] = []
        self.us_classification[:] = []
        #self.citation_list[:] = []
        #self.claim_counter = 0
        #self.claims ={}
        self.stack.clear()
        #self.inventor_count = 0
        #self.citation_count = 0
        self.abstract=""
        self.count=0
        self.ls[:]=[]

    # Construct Json to work with various database format
    def serialization(self):
        results = {}
        results["_id"] = self.doc_number[0]
        results["title"] = self.invention_title
        results["date-produced"] = self.date_produced
        results["country"] = self.country
        results["date-published"] = self.date_publ
        results["app-number"] = self.application_number
        #results["number-of-claims"] = self.number_of_claims[0]
        results["kind"] = self.kind[0]
        #results["inventors"] = self.inventor_list
        #results["citations"] = self.citation_list
        results["us_classification"]=self.us_classification
        results["appl-type"]=self.appl_type
        #results["claims"]=self.claims
        if self.appl_type!="design":
            results["abstract"]=self.abstract
        #print(results)
        return results


def xml_documents(file_obj):
    """Split large xml file into separated xml instance.
    Code from http://stackoverflow.com/questions/15645057/parsing-large-combined-xml-document-with-python

    Args:
        :param file_obj(file_object): a handler to large xml file.

    Returns:
        :param document(str): document form of each xml instance.

    """
    document = []
    for line in file_obj:
        if line.strip().startswith('<?xml') and document:
                yield ''.join(document)
                document = []
        document.append(line)
    if document:
        yield ''.join(document)


def parse_xml(file_name, size = 0, method = "json"):
    """Parse single XML file into the separated XML instances.

    Args:
        :param file_name(str): Name of file within same level as root.
        :param size(int): Number of XML instances within XML file to be read.
        :param method(str): Export method(Default:Json).

    Returns:
    int.  The return code::

             0 -- Success!
    """
    # Set initial values
    count = 0
    results = []
    global MAX_NUMBER_OF_PATENTS
    # create an XML Reader
    parser = xml.sax.make_parser()
    # turn off namespaces
    parser.setFeature(xml.sax.handler.feature_namespaces, 0)
    # turn off validation for DTD
    parser.setFeature(xml.sax.handler.feature_external_ges, False)
    # override the default Context Handler
    xml_patent_handler = PatentHandler()
    parser.setContentHandler(xml_patent_handler)
    if(file_name=="all"):
    # First validate if input folder already contain json file
        for folder in os.listdir(abs_path):
            isEmpty=True
            folder_path = abs_path+"/"+folder
            # First brutal check if any json file exist
            for file in os.listdir(folder_path):
                if file.endswith(".json"):
                    isEmpty=False
                    print("This folder"+folder+" is already processed")
                    break
            # If this file contains no json string, we begin to extract
            if isEmpty:
                file_name=folder_path+"/"+folder+'.xml'
                print("Processing file ", file_name)
                try:
                    with open(file_name) as citation:
                        for xml_part in xml_documents(citation):
                            parser.parse(StringIO(xml_part))
                            results.append(copy.deepcopy(xml_patent_handler.serialization()))
                            count = count+1
                            if not MAX_NUMBER_OF_PATENTS:
                                if count == int(size):
                                    break
                            xml_patent_handler.reset()
                    if method == "json":
                        export2json(file_name, results)
                except IOError as e:
                    raise e
    # Specific file name is provided, process them individually
    else:
        try:
            with open(file_name) as citation:
                for xml_part in xml_documents(citation):
                    parser.parse(StringIO(xml_part))
                    results.append(copy.deepcopy(xml_patent_handler.serialization()))
                    count = count+1
                    if not MAX_NUMBER_OF_PATENTS:
                        if count == int(size):
                            break
                    xml_patent_handler.reset()
            if method == "json":
                export2json(file_name, results)
            return 0
        except IOError as e:
            raise e

def export2json(file_name, data):
    """Export to json file

    Args:
        :param data(list): data list to be exported.

    Returns:
    int.  The return code::

             0 -- Success!
    """
    name = file_name.split(".")[0]
    counter = 0
    i = 0
    step = len(data)//MAX_NUMBER_OF_OUTPUT_FILE
    while True:
        old = i
        i += step
        counter += 1
        if i >= len(data):
            with open(name+"_"+str(counter)+'.json', 'w') as outfile:
                json.dump(data[old:len(data)], outfile, indent=4, ensure_ascii=False)
            break
        elif len(data)<MAX_NUMBER_OF_OUTPUT_FILE:
            with open(name+"_"+str(len(data))+'.json', 'w') as outfile:
                json.dump(data[old:len(data)], outfile, indent=4, ensure_ascii=False)
                break
        else:
            with open(name+"_"+str(counter)+'.json', 'w') as outfile:
                json.dump(data[old:i], outfile, indent=4, ensure_ascii=False)
    return 0

def main(argv):
    # define local variables
    inputfile = 0
    size = 0
    method = ""
    global MAX_NUMBER_OF_PATENTS
    try:
        opts, args = getopt.getopt(argv,"hi:",["input=","size=","export=","help"])
        #Empty input, raise error
        if opts == []:
            print('[Usage:] parse_patent.py --i=<inputfile.xml> --s=<number_of_xmls> --e=<export_type>')
            sys.exit(2)
    except getopt.GetoptError:
        print('[Usage:] parse_patent.py --i=<inputfile.xml> --s=<number_of_xmls> --e=<export_type>')
        sys.exit(2)
    for opt, arg in opts:
        if opt == '--help':
            print('[Example Usage] ./parse_patent.py \n\
            --input=/path/to/data.xml  # Specify path to data for processing\n\
            --size=1                   # Number of xml instances to be processed\n\
            --export=json              # Support json\n')
            sys.exit()
        elif opt in ("--input, -i"):
            inputfile = arg
        elif opt in ("--size", "-s"):
            size = arg
            MAX_NUMBER_OF_PATENTS = False
        elif opt in ("--export", "-e"):
            method = arg
    parse_xml(inputfile,size, method)

if __name__ == "__main__":
    main(sys.argv[1:])
