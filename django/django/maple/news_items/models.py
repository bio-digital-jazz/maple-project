from django.db import models
from datetime import datetime

class NewsItem(models.Model):
    date_added = models.DateTimeField(auto_now = True)
    news_text = models.TextField()
    is_displayed = models.BooleanField(default=True)


    def __str__(self):
        return str(self.date_added)
