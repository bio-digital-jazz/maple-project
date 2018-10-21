from django.db import models
from datetime import datetime
from data_assets.models import DataAsset
from django.contrib.auth.models import User

STATUS_CHOICES = (
        ('Awaiting triage', 'Awaiting triage'),
        ('Awaiting scoping and costing', 'Awaiting scoping and costing'),
        ('Awaiting data retrieval and analysis', 'Awaiting data retrieval and analysis'),
        ('Awaiting approval', 'Awaiting approval'),
        ('Awaiting confirmation of completed work', 'Awaiting confirmation of completed work'),
        ('Closed', 'Closed'),
        ('On hold', 'On hold'),
    )

TASK_CHOICES = (
        ('Scoping and costing', 'Scoping and costing'),
        ('Data retrieval and analysis', 'Data retrieval and analysis'),
        ('Approval', 'Approval'),
    )

class Project(models.Model):
    requestor = models.ForeignKey(User, related_name="project", on_delete=models.CASCADE,
                              null=True)
    title_or_summary = models.CharField(max_length=255)
    detail = models.TextField()
    date_received = models.DateField(default=datetime.now, blank=True)
    current_status = models.CharField(max_length=255, choices=STATUS_CHOICES)
    has_associated_cost = models.BooleanField()
    quoted_cost = models.FloatField()
    date_due = models.DateField(default=datetime.now, blank=True)
    category = models.CharField(max_length=255)
    data_supplied = models.BooleanField(default=True)



class ProjectNote(models.Model):
	created_by = models.ForeignKey(User, related_name="project_note", on_delete=models.CASCADE,
                              null=True)
	project = models.ForeignKey(Project, on_delete=models.CASCADE)
	date_and_time = models.DateTimeField(auto_now_add=True)
	detail = models.TextField()
	title_or_summary = models.CharField(max_length=255)

   

class ProjectTask(models.Model):
	created_by = models.ForeignKey(User, related_name="project_task_creator", on_delete=models.CASCADE,
                              null=True)
	allocated_to = models.ForeignKey(User, related_name="project_task_allocated_to", on_delete=models.CASCADE,
                              null=True)
	date_and_time = models.DateTimeField(default=datetime.now)
	request = models.ForeignKey(Project, default=1, on_delete=models.CASCADE)
	task_type = models.CharField(max_length=255, choices=TASK_CHOICES)
	task_detail = models.TextField()
	days_required = models.IntegerField()
	task_completed = models.BooleanField(default=False)