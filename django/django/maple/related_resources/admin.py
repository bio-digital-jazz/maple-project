from django.contrib import admin
from . import models

# Register your models here.
admin.site.register(models.DataAssetRelatedResource)
admin.site.register(models.TrainingModuleRelatedResource)
