import os, sys, getopt
from settings import patent_path,class_path, train_path
from collections import OrderedDict
import shutil

def get_tree_size(path):
    """Return total size of files in given path and subdirs."""
    total = 0
    for file in os.listdir(path):
        file_path = path+"/"+file
        if not os.path.isdir(file_path):
            total += os.path.getsize(file_path)
    return total

results={}
for folder in os.listdir(class_path):
    folder_path = os.path.join(class_path,folder)
    results[folder]=get_tree_size(folder_path)
re_sorted_by_value = OrderedDict(sorted(results.items(), key=lambda x: x[1],reverse=True)[:30])

# Delete the train folder
for folder in os.listdir(train_path):
    shutil.rmtree(os.path.join(train_path,folder))
# Move new data to train folder
for item,key in re_sorted_by_value.items():
    src = os.path.join(class_path,item)
    dst = os.path.join(train_path,item)
    shutil.copytree(src,dst)
