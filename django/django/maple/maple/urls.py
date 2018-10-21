from django.contrib import admin
from django.urls import include, path

urlpatterns = [
    path('admin/', admin.site.urls),
    path('maple/', include('main.urls')),
    path('api/data-assets', include('data_assets.urls')),
]
