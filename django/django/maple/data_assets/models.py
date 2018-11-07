from django.db import models
from datetime import datetime
from django.contrib.auth.models import User


ROLE_CHOICES = (
    ("Data Sponsor", "Data Sponsor"),
    ("Data Custodian", "Data Custodian"),
    ("Data Stewart", "Data Stewart"),
    ("Data User", "Data User"),
    ("Business Owner", "Business Owner"),
    ("System Owner", "System Owner"),
    ("System User", "System User")
)

TYPE_CHOICES = (
    ("System", "System"),
    ("Extract", "Extract"),
    ("Analytics", "Analytics"),
    ("Reporting", "Reporting"),
    ("Auxiliary", "Auxiliary"),
    ("Sample", "Sample"),
)

DATA_LIMITING_MARKER_CHOICES = (
    ("For Official Use Only", "For Official Use Only"),
    ("Sensitive - Personal", "Sensitive - Personal"),
    ("Sensitive - Health Information", "Sensitive - Health Information"),
    ("Senstitive - Legal", "Senstitive - Legal"),
    ("Senstitive - NSW Government", "Senstitive - NSW Government"),
    ("Senstitive - NSW Cabinet", "Senstitive - NSW Cabinet"),
)

ROLE_STATUS_CHOICES = (
    ("Active", "Active"),
    ("Inactive", "Inactive"),
)

UPDATE_FREQUENCEY_CHOICES = (
    ("Daily", "Daily"),
    ("Weekly", "Weekly"),
    ("Monthly", "Monthly"),
    ("Quarterly", "Quarterly"),
    ("Annually", "Annually"),
    ("Ad-hoc", "Ad-hoc")
)

DATA_ELEMENT_CATEGORY_CHOICES = (
    ("Person", "Person"),
    ("Case", "Case"),
    ("Episode", "Episode"),
    ("Health Practitioner", "Health Practitioner"),
    ("Health Organisation", "Health Organisation"),
    ("Grant", "Grant"),
    ("Clinical Trial", "Clinical Trial")
)

OWNING_BUSINESS_UNIT_CHOICES = (
    ("Strategic Research and Investment", "Strategic Research and Investment"),
    ("Cancer Services and Information", "Cancer Services and Information"),
    ("Cancer Screening and Prevention", "Cancer Screening and Prevention"),
    ("CI-IT", "CI-IT"),
    ("CI-Admin", "CI-Admin"),
    ("Other", "Other")
)


DATA_LOCATION_TYPE_CHOICES = (
    ("Web Resource", "Web Resource"),
    ("Internal Server", "Internal Server"),
    ("External Server", "External Server"),
)



class DataAsset(models.Model):
    owning_org = models.CharField(max_length=200)
    owning_business_unit = models.CharField(max_length=200, choices = OWNING_BUSINESS_UNIT_CHOICES)
    name = models.CharField(max_length=200)
    asset_id = models.CharField(max_length=200)
    version = models.DecimalField(max_digits=10, decimal_places=3)
    type = models.CharField(max_length=200, choices = TYPE_CHOICES)
    emergency_shutdown_contact = models.CharField(max_length=200)
    emergency_shutdown_org = models.CharField(max_length=200)
    start_date = models.DateField()
    end_date = models.DateField(null=True, blank=True)
    data_limiting_marker = models.CharField(max_length=200, choices = DATA_LIMITING_MARKER_CHOICES)
    update_frequency = models.CharField(max_length=200, choices = UPDATE_FREQUENCEY_CHOICES)
    related_asset = models.ManyToManyField('self', blank=True)
    currently_active = models.BooleanField(default=False)
    utlizes_external_data = models.BooleanField(default=False)
    governed_as_external_data = models.BooleanField(default=False)
    available_for_analytics = models.BooleanField(default=False)
    under_review = models.BooleanField(default=False)
    used_in_shared_dictionary = models.BooleanField(default = False)
    usage_notes = models.TextField()
    data_location_type = models.CharField(max_length = 200, choices = DATA_LOCATION_TYPE_CHOICES)
    data_location = models.CharField(max_length=200)
    description = models.TextField()


    class Meta:
        verbose_name_plural = "Data assets"
    def __str__(self):
        return self.name


class DataAssetRole(models.Model):
    data_asset = models.ForeignKey(DataAsset, on_delete=models.CASCADE, related_name="data_asset_roles")
    type = models.CharField(max_length = 100, choices = ROLE_CHOICES)
    user = models.ForeignKey(User, related_name="data_asset_roles", on_delete=models.CASCADE,
                              null=True)
    status = models.CharField(max_length=100, choices = ROLE_STATUS_CHOICES)
    start_date = models.DateField(null=True, blank=True)
    end_date = models.DateField(null=True, blank=True)

    def __str__(self):
        return self.user.username

class DataElement(models.Model):
    element_id = models.CharField(max_length=200)
    name = models.CharField(max_length=200)
    name_is_conformed = models.BooleanField(default=False)
    type = models.CharField(max_length=200)
    definition = models.TextField()
    accepted_values = models.TextField()
    legacy_collection = models.CharField(max_length=255, default="unknown")
    category = models.CharField(max_length=255, choices = DATA_ELEMENT_CATEGORY_CHOICES)
    usage_notes = models.TextField()
    under_review = models.BooleanField(default=False)

    def __str__(self):
        return self.name



class DataElementUse(models.Model):
    data_element = models.ManyToManyField(DataElement, related_name="data_elements")
    source_data_asset = models.ForeignKey(DataAsset, on_delete=models.CASCADE, related_name="data_element_source")
    source_element_name = models.CharField(max_length =100, null=True)
    destination_data_asset = models.ForeignKey(DataAsset, on_delete=models.CASCADE, related_name="data_element_destination")
    destination_element_name = models.CharField(max_length = 100, null=True)
    destination_name_is_conformed = models.BooleanField(default=False)
    element_use_id = models.CharField(max_length=50)
    risk_level = models.IntegerField()
    null_value_accepted = models.BooleanField(default=False)
    destination_asset_usage_notes = models.TextField()
    element_transformation_rules = models.TextField()
    under_review = models.BooleanField(default=False)

    def __str__(self):
        return str(self.element_use_id)


class DataAssetBreach(models.Model):
    date_of_report = models.DateField(default=datetime.now)
    summary = models.CharField(max_length=255)
    description = models.TextField()

    class Meta:
        verbose_name_plural = "Data asset breaches"
