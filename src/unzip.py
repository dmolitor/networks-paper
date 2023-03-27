import os
import py7zr
import shutil
import zipfile

if not os.path.exists('scratch/'):
    os.mkdir('scratch/')

with py7zr.SevenZipFile('data/od.7z', mode='r') as z:
    z.extractall('scratch/')

with py7zr.SevenZipFile('data/opp_migration_with_features.7z', mode='r') as z:
    z.extractall('scratch/')

with zipfile.ZipFile('data/od_inc.zip', 'r') as zip_ref:
    zip_ref.extractall('scratch/')

with zipfile.ZipFile('data/od_race.zip', 'r') as zip_ref:
    zip_ref.extractall('scratch/')

shutil.copyfile('data/od_pooled.csv', 'scratch/od_pooled.csv')
