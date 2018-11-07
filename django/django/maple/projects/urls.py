from django.urls import path
from rest_framework import routers
from . import views
from django.conf.urls import url, include

app_name = "data_assets"

router = routers.DefaultRouter()
router.register('all-projects', views.ProjectViewSet)

urlpatterns = [
    path("/", include(router.urls)),
]
