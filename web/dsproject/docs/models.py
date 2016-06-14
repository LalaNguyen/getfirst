from django.db import models

# Create your models here.
class Docs(models.Model):
    patid = models.TextField(blank=True, null=True)
    date = models.TextField(blank=True, null=True)
    keyterm = models.TextField(blank=True, null=True)
    freq = models.FloatField(blank=True, null=True)
    path = models.TextField(blank=True, null=True)
    id = models.IntegerField(primary_key=True)
    class Meta:
        managed = True
        db_table = 'docs'

    def __str__(self):
        return self.keyterm
