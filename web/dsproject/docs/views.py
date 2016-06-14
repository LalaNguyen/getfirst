from django.template import loader
from django.http import HttpResponse
from .models import Docs
import json
# Create your views here.
def index(request):
    docs = Docs.objects.order_by('-freq')[:10000]
    template = loader.get_template('docs/index.html')
    values = [doc.keyterm for doc in docs]
    context = {
        'docs_json':values,
    }
    return HttpResponse(template.render(context,request))

def search(request):
    if request.method == 'GET':
        data = request.GET['q']
        # Break data into tokens
        tokens = str.split(data,',')
        re = []
        for word in tokens:
            docs = Docs.objects.filter(keyterm__exact=word)
            re = re.append(list(docs.values()))
            print(docs.values())
        response_data={'text':'Hellp'}
        return HttpResponse(
            json.dumps(response_data),
            content_type="application/json"
        )
