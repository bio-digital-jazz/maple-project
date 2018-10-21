from django.shortcuts import render
from . import serializers
from . import models
from rest_framework import viewsets


class DataAssetBreachViewSet(viewsets.ModelViewSet):
    queryset = models.DataAssetBreach.objects.all()
    serializer_class = serializers.DataAssetBreachSerializer
