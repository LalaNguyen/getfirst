import os, sys, getopt
from settings import patent_path,class_path, train_path
from collections import OrderedDict
import shutil

def get_tree_size(path):
    """Return total size of files in given path and subdirs."""
    total = 0
    for entry in os.scandir(path):
        if entry.is_dir(follow_symlinks=False):
            total += get_tree_size(entry.path)
        else:
            total += entry.stat(follow_symlinks=False).st_size
    return total

results={}
for folder in os.listdir(class_path):
    folder_path = os.path.join(class_path,folder)
    results[folder]=get_tree_size(folder_path)
re_sorted_by_value = OrderedDict(sorted(results.items(), key=lambda x: x[1],reverse=True)[:10])

# Delete the train folder
for folder in os.listdir(train_path):
    shutil.rmtree(os.path.join(train_path,folder))
# Move new data to train folder
for item,key in re_sorted_by_value.items():
    src = os.path.join(class_path,item)
    dst = os.path.join(train_path,item)
    shutil.copytree(src,dst)
