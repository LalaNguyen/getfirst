import json
import csv
# Create class template for input term
cls345173 = { "name" : "345173" , "children" :[]}
cls370252 = { "name" : "370252" , "children" :[]}
cls370328 = { "name" : "370328" , "children" :[]}
cls370329 = { "name" : "370329" , "children" :[]}
cls370330 = { "name" : "370330" , "children" :[]}
cls370331 = { "name" : "370331" , "children" :[]}
cls370332 = { "name" : "370332" , "children" :[]}
root={ "name" : "root" ,  "children" :[]}

# Read data from R script
with open("foo.csv","r") as csvfile:
    clsReader = csv.reader(csvfile, delimiter= ',' , quotechar= '|' )

    for row in clsReader:
        # Convert string to readable format
        print(row[3])
        row[3] = row[3].translate(str.maketrans('"'," ")).lstrip().rstrip()
        row[2] = row[2].translate(str.maketrans('"'," ")).lstrip().rstrip()
        # Convert term to dictionary
        term = { "name" :row[3], "size" :row[2]}
        if row[1] == '"345173"':
            cls345173[ "children" ].append(term)
        elif row[1]== '"370252"':
            cls370252[ "children" ].append(term)
        elif row[1]== '"370328"' :
            cls370328[ "children" ].append(term)
        elif row[1]== '"370329"' :
            cls370329[ "children" ].append(term)
        elif row[1]== '"370330"' :
            cls370330[ "children" ].append(term)
        elif row[1]== '"370331"':
            cls370331[ "children" ].append(term)
        elif row[1]== '"370332"':
            cls370332[ "children" ].append(term)

# Reconstruct the tree in order of inclusion
cls370331[ "children" ].append(cls370332)
cls370329[ "children" ].append(cls370331)
cls370328[ "children" ].append(cls370329)

# Append to root
root[ "children" ].append(cls370328)
root[ "children" ].append(cls370252)
root[ "children" ].append(cls345173)
print(json.dumps(root,indent=2))


with open("C:/Users/Administrator/OPAIRS/R-scripts/web/train/tree.json", "w" ) as outfile:
    json.dump(root, outfile)
