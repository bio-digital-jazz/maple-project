from django import forms

from . import models


class DataAssetBreachForm(forms.ModelForm):
	class Meta:
		model = models.DataAssetBreach
		fields = [
				"date_of_report",
			    "summary",
			    "description"
		]
