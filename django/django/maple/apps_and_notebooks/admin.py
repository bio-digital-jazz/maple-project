from django.contrib import admin
from . import models



class AppOrNotebookTagInline(admin.TabularInline):
    model = models.AppOrNotebookTag
    extra = 0


class AppOrNotebookAdmin(admin.ModelAdmin):
    list_display = ('created_by',
                    'title',
                    'date_created'
                    )
    # list_filter = ['owning_business_unit', 'type']
    # search_fields = ('name',)
    inlines = [AppOrNotebookTagInline,]


admin.site.register(models.AppOrNotebook, AppOrNotebookAdmin)
admin.site.register(models.AppOrNotebookTag)
# Register your models here.
