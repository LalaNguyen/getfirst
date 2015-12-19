from settings import rake
from nltk.corpus import stopwords

def markup(query):
    kws = rake.run(query)
    for kw in kws:
        if kw[0] in query and len(kw[0])>1:
            query = query.replace(kw[0],"_".join(kw[0].split(" ")))
    return query
