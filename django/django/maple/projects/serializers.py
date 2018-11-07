from rest_framework import serializers
from django.contrib.auth.models import User
from . import models

class UserSerializer(serializers.ModelSerializer):
    class Meta:
        model = User
        fields = (
            'first_name',
            'last_name'
        )

class ProjectSerializer(serializers.ModelSerializer):
    class Meta:
        model = models.Project
        fields = (
        'id',
         'requestor',
         'title_or_summary',
         'detail',
         'current_status',
         'date_received',
         'data_supplied'
        )
