import requests

url = "https://www2.census.gov/programs-surveys/demo/tables/geographic-mobility/2015/county-to-county-migration-2011-2015/county-to-county-migration-flows/county-to-county-2011-2015-ins-outs-nets-gross.xlsx"
rural_url = "https://www.hrsa.gov/sites/default/files/hrsa/rural-health/non-metro-counties-cts.xlsx"
resp = requests.get(url)

with open("scratch/total_migration.xlsx", "wb") as f:
    f.write(resp.content)

resp = requests.get(rural_url)

with open("scratch/rural_urban.xlsx", "wb") as f:
    f.write(resp.content)
