# Generated by Django 2.1.3 on 2018-11-07 05:37

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('data_assets', '0001_initial'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='dataassetbreach',
            name='data_asset',
        ),
    ]
