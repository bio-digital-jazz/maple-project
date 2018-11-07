from django.shortcuts import render
from django.http import HttpResponseRedirect
from rest_framework import viewsets
from . import serializers
from . import models
from . import forms
from django.contrib import messages

class DataAssetBreachViewSet(viewsets.ModelViewSet):
    queryset = models.DataAssetBreach.objects.all()
    serializer_class = serializers.DataAssetBreachSerializer

def thanks(request):
	return render(request, 'data_assets/thanks.html')

def create_data_breach(request):
    print("print view called")
    form = forms.DataAssetBreachForm()

    if request.method == 'POST':
        form = forms.DataAssetBreachForm(request.POST)
        print(form)
        if form.is_valid():
            print("HERE")
            breach = form.save(commit=False)
            breach.save()
            return HttpResponseRedirect('/api/data-assets/thanks')
    return render(request,'data_assets/data_breach_form.html', {'form': form})
