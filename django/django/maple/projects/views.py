from django.shortcuts import render
from rest_framework import viewsets
from . import serializers
from . import models

class ProjectViewSet(viewsets.ModelViewSet):
    queryset = models.Project.objects.all()
    serializer_class = serializers.ProjectSerializer
