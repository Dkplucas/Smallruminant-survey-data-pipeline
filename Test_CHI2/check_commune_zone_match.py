import pandas as pd
import os, re, unicodedata
os.chdir(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2')
raw = pd.read_excel('data8_copy.xlsx')
commune_col = 'II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune'
raw['Commune'] = raw[commune_col].astype(str).str.strip()
raw['Commune'] = raw['Commune'].replace({'': pd.NA, 'nan': pd.NA})
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
print('unique communes count', raw['Commune_clean'].nunique())
print(raw['Commune_clean'].value_counts().head(30))

zones = pd.read_excel('zones.xlsx')
zones = zones.rename(columns={'Vegetation zones':'Vegetation_zone','Phytogeographic zones':'Phytogeographic_zone','District':'District'})
zones['District'] = zones['District'].astype(str).str.strip()
zones = zones[~zones['District'].astype(str).str.match(r'^\s*Total', na=False, case=False)]
zones['District_clean'] = zones['District'].map(normalize_text)
print('unique zone districts count', zones['District_clean'].nunique())
print(zones['District_clean'].value_counts().head(30))

communes = set(raw['Commune_clean'].dropna().unique())
zoned = set(zones['District_clean'].dropna().unique())
print('intersection count', len(communes & zoned))
print('missing communes not in zones:', sorted(communes - zoned)[:50])
print('sample matched communes:', sorted(communes & zoned)[:50])
