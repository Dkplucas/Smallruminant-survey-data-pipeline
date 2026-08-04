import pandas as pd
import numpy as np
import os
os.chdir(r"C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2")

raw = pd.read_excel('data8.xlsx')
sex_col = "I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin"
commune_col = "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

print('total rows:', len(raw))
print('sex non-null:', raw[sex_col].notna().sum())
print('commune non-null:', raw[commune_col].notna().sum())
print('sex/numeric count 1/2:', raw[sex_col].isin([1,2,'1','2']).sum())

raw['Commune'] = raw[commune_col].astype(str).str.strip()
raw['Commune'] = raw['Commune'].replace({'': np.nan, 'nan': np.nan})
print('commune blank treated as NA:', raw['Commune'].isna().sum())
print('rows with both sex and commune:', ((raw[sex_col].isin([1,2,'1','2'])) & raw['Commune'].notna()).sum())

# normalize function from R script
import re
import unicodedata

def normalize_text(x):
    if pd.isna(x):
        return None
    x = str(x).strip()
    if x == '':
        return None
    x = x.lower()
    x = unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode('ascii')
    x = re.sub(r'[^a-z0-9 ]+', '', x)
    x = re.sub(r'\s+', ' ', x).strip()
    return x

raw['Commune_clean'] = raw['Commune'].map(normalize_text)
print('commune_clean missing:', raw['Commune_clean'].isna().sum())

zones = pd.read_excel('zones.xlsx')
zones = zones.rename(columns={'Vegetation zones':'Vegetation_zone','Phytogeographic zones':'Phytogeographic_zone','District':'District'})
zones['District'] = zones['District'].astype(str).str.strip()
zones = zones[~zones['District'].astype(str).str.match(r'^\s*Total', na=False, case=False)]
zones['District_clean'] = zones['District'].map(normalize_text)
print('zones districts count:', len(zones))
print('zones unique districts:', zones['District_clean'].nunique())

joined = raw.merge(zones[['District_clean','Vegetation_zone','Phytogeographic_zone']], how='left', left_on='Commune_clean', right_on='District_clean')
print('joined total rows:', len(joined))
print('joined missing vegetation:', joined['Vegetation_zone'].isna().sum())
print('joined missing phyto:', joined['Phytogeographic_zone'].isna().sum())

analysis = joined[(joined[sex_col].isin([1,2,'1','2'])) & joined['Commune_clean'].notna() & joined['Vegetation_zone'].notna() & joined['Phytogeographic_zone'].notna()]
print('analysis rows:', len(analysis))

print('rows with sex invalid:', (~raw[sex_col].isin([1,2,'1','2'])).sum())
print('rows with commune NA or blank:', raw['Commune_clean'].isna().sum())

# show problematic rows
print('first 10 rows with missing zone or invalid sex:')
print(raw[~raw.index.isin(analysis.index)][[sex_col, commune_col, 'Commune', 'Commune_clean']].head(20).to_string(index=False))
