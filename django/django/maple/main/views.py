from django.shortcuts import render
from news_items.models import NewsItem

def index(request):
    news_items = NewsItem.objects.filter(is_displayed = True).order_by('-date_added')

    context = {'news_items': news_items}
    return render(request, 'main/index.html', context)

def metadata(request):
	return render(request, 'main/metadata.html')

def dictionary(request):
	return render(request, 'main/dictionary.html')

def analytics(request):
	return render(request, 'main/analytics.html')

def resources(request):
	return render(request, 'main/resources.html')

def projects(request):
	return render(request, 'main/projects.html')

def forms(request):
	return render(request, 'main/forms.html')
