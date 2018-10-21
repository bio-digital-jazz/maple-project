from django.shortcuts import render


def index(request):
    return render(request, 'main/index.html')


def metadata(request):
	return render(request, 'main/metadata.html')