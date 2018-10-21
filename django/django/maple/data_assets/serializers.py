from rest_framework import serializers
from . import models


class DataAssetBreachSerializer(serializers.ModelSerializer):
    class Meta:
        model = models.DataAssetBreach
        fields = (
         'date_of_report',
         'summary',
         

        )
