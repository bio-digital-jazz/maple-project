from django.db import models

# Create your models here.
class TrainingModule(models.Model):
    name = models.CharField(max_length=255)
    description = models.TextField()
    collateral_location = models.URLField(max_length=255)
    version = models.FloatField()

    def __str__(self):
        return self.name
