import csv
from django.http import HttpResponse
from . import models


def assets_report(request):

    assets = models.DataAsset.objects.all()
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="assets_report.csv"'
    writer = csv.writer(response)
    writer.writerow(['First row', 'Foo', 'Bar', 'Baz'])

    for obj in assets:
        writer.writerow([obj.name, 'A', 'B', 'C', '"Testing"', "Here's a quote"])

    return response

def users_report(request):
    users = models.DataAssetRole.objects.all()

    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="users_report.csv"'
    writer = csv.writer(response)
    writer.writerow(['First row', 'Foo', 'Bar', 'Baz'])

    for obj in users:
        writer.writerow([obj.user.first_name, 'A', 'B', 'C', '"Testing"', "Here's a quote"])

    return response


def resources_report(request):
    users = models.DataAssetRole.objects.all()

    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="users_report.csv"'
    writer = csv.writer(response)
    writer.writerow(['First row', 'Foo', 'Bar', 'Baz'])

    for obj in users:
        writer.writerow([obj.user.first_name, 'A', 'B', 'C', '"Testing"', "Here's a quote"])

    return response

def projects_report(request):
    users = models.DataAssetRole.objects.all()

    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="users_report.csv"'
    writer = csv.writer(response)
    writer.writerow([str(datetime.datetime.today().strftime('%Y-%m-%d'))])
    writer.writerow(['First row', 'Foo', 'Bar', 'Baz'])

    for obj in users:
        writer.writerow([obj.user.first_name, 'A', 'B', 'C', '"Testing"', "Here's a quote"])
    writer.writerow(datetime.datetime.today().strftime('%Y-%m-%d'))
    return response
