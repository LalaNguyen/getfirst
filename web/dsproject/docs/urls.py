from django.conf.urls import url

from . import views

urlpatterns = [
    # ex: /docs/
    url(r'^$', views.index, name='index'),
    url(r'^search', views.search, name='search'),
]
