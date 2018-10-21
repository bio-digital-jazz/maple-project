from django.db import models
from django.contrib.auth.models import User
from datetime import datetime



class AppOrNotebook(models.Model):
    created_by = models.ForeignKey(User, related_name="app_or_notebook", on_delete=models.CASCADE,
                              null=True)
    title = models.CharField(max_length=255)
    date_created = models.DateField(default=datetime.now)
    description = models.TextField()
    location = models.URLField()

    def __str__(self):
        return self.creator


class AppOrNotebookTag(models.Model):
    name = models.CharField(max_length=255)
    app_or_notebook = models.ForeignKey(AppOrNotebook, related_name="tags", on_delete=models.CASCADE)

    def __str__(self):
        return self.name