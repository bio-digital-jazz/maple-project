from django.urls import path
from . import views
from django.conf.urls import url, include

app_name = "main"

urlpatterns = [
    path('', views.index, name = 'index'),
    path('metadata', views.metadata, name = "metadata"),
    path('dictionary', views.dictionary, name = "dictionary"),
    path('analytics', views.analytics, name = "analytics"),
    path('resources', views.resources, name = "resources"),
    path('projects', views.projects, name = "projects"),
    path('forms', views.forms, name = "forms")
]
