# Generated by Django 2.1.2 on 2018-10-17 05:29

from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        ('training_modules', '0001_initial'),
        ('data_assets', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='DataAssetRelatedResource',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('title', models.CharField(max_length=200)),
                ('description', models.TextField()),
                ('date_created', models.DateField(blank=True, null=True)),
                ('version', models.FloatField(blank=True, null=True)),
                ('hprm_reference', models.CharField(max_length=200)),
                ('next_review_date', models.DateField(blank=True, null=True)),
                ('data_asset', models.ManyToManyField(blank=True, related_name='data_asset_related_resources', to='data_assets.DataAsset')),
            ],
            options={
                'abstract': False,
            },
        ),
        migrations.CreateModel(
            name='TrainingModuleRelatedResource',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('title', models.CharField(max_length=200)),
                ('description', models.TextField()),
                ('date_created', models.DateField(blank=True, null=True)),
                ('version', models.FloatField(blank=True, null=True)),
                ('hprm_reference', models.CharField(max_length=200)),
                ('next_review_date', models.DateField(blank=True, null=True)),
                ('training_module', models.ManyToManyField(blank=True, to='training_modules.TrainingModule')),
            ],
            options={
                'abstract': False,
            },
        ),
    ]