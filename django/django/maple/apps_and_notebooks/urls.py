from django.urls import path
from rest_framework import routers
from . import views
from django.conf.urls import url, include

app_name = "apps_or_notebooks"

router = routers.DefaultRouter()
router.register('all-apps_or_notebooks', views.AppOrNotebookViewSet)

urlpatterns = [
    path("/", include(router.urls)),
]
