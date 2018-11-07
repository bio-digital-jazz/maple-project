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

class AppOrNotebookSerializer(serializers.ModelSerializer):
    class Meta:
        model = models.AppOrNotebook
        fields = (
        'id',
        'title',
        'description',
        'location'
        )
