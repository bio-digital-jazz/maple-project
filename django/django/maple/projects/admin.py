from django.contrib import admin
from . import models


class ProjectNoteInline(admin.StackedInline):
    model = models.ProjectNote
    extra = 0

class ProjectTaskInline(admin.TabularInline):
    model = models.ProjectTask
    extra = 0

class ProjectAdmin(admin.ModelAdmin):
    list_display = ('title_or_summary',
                    'date_received',
                    'current_status',
                    'requestor'
                    )
    # list_filter = ['owning_business_unit', 'type']
    # search_fields = ('name',)
    inlines = [ ProjectTaskInline, ProjectNoteInline]



admin.site.register(models.Project, ProjectAdmin)
admin.site.register(models.ProjectTask)
admin.site.register(models.ProjectNote)
