from django.urls import path
from . import views
from django.conf.urls import url, include

app_name = "main"


urlpatterns = [
    path('', views.index, name = 'index'),
    path('metadata', views.metadata, name = "metadata")


]
