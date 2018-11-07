from django.shortcuts import render
from rest_framework import viewsets
from . import serializers
from . import models

class AppOrNotebookViewSet(viewsets.ModelViewSet):
    queryset = models.AppOrNotebook.objects.all()
    serializer_class = serializers.AppOrNotebookSerializer
