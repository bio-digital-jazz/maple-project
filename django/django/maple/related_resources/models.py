from django.db import models
from datetime import datetime
from data_assets.models import DataAsset
from training_modules.models import TrainingModule


class RelatedResource(models.Model):
    title=models.CharField(max_length=200)
    description=models.TextField()
    date_created = models.DateField(blank=True, null=True)
    version = models.FloatField(blank=True, null=True)
    hprm_reference = models.CharField(max_length=200)
    next_review_date = models.DateField(blank=True, null=True)

    def __str__(self):
        return self.title

    class Meta:
        abstract = True

class DataAssetRelatedResource(RelatedResource):
    data_asset = models.ManyToManyField(DataAsset, related_name = "data_asset_related_resources", blank=True)


class TrainingModuleRelatedResource(RelatedResource):
    training_module = models.ManyToManyField(TrainingModule, blank=True)
