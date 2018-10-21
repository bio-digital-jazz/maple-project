from django.urls import path
from rest_framework import routers
from . import views
from django.conf.urls import url, include

app_name = "data_assets"

router = routers.DefaultRouter()
router.register('breach', views.DataAssetBreachViewSet)
urlpatterns = [
    path("/v1/", include(router.urls)),
  


]
