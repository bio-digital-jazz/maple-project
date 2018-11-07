from django.urls import path
from rest_framework import routers
from . import views
from . import report_views
from django.conf.urls import url, include

app_name = "data_assets"

router = routers.DefaultRouter()
router.register('breach', views.DataAssetBreachViewSet)
urlpatterns = [
    path("/v1/", include(router.urls)),
    path("/breach-form", views.create_data_breach),
    path("/assets-report", report_views.assets_report),
    path("/users-report", report_views.users_report),
    path("/thanks",views.thanks)
]
