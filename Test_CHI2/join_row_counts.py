import pandas as pd, os, re, unicodedata
os.chdir(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2')
raw = pd.read_excel('data8_copy.xlsx')
sex_col = 'I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin'
commune_col = 'II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune'
raw['Commune'] = raw[commune_col].astype(str).str.strip().replace({'': pd.NA, 'nan': pd.NA})
def normalize_text(x):
    if pd.isna(x): return None
    x = str(x).strip().lower()
    x = unicodedata.normalize('NFKD', x).encode('ascii','ignore').decode('ascii')
    x = re.sub(r'[^a-z0-9 ]+', '', x)
    x = re.sub(r'\s+', ' ', x).strip()
    return x
raw['Commune_clean'] = raw['Commune'].map(normalize_text)

zones = pd.read_excel('zones.xlsx')
zones = zones.rename(columns={'Vegetation zones':'Vegetation_zone','Phytogeographic zones':'Phytogeographic_zone','District':'District'})
zones['District'] = zones['District'].astype(str).str.strip().replace({'': pd.NA, 'nan': pd.NA})
zones[['Vegetation_zone','Phytogeographic_zone']] = zones[['Vegetation_zone','Phytogeographic_zone']].astype(str).replace({'nan': pd.NA, '': pd.NA})

def clean_nbsp(x):
    if pd.isna(x): return x
    return x.replace('\xa0', '').strip()
zones['Vegetation_zone'] = zones['Vegetation_zone'].map(clean_nbsp)
zones['Phytogeographic_zone'] = zones['Phytogeographic_zone'].map(clean_nbsp)

zones = zones[~zones['District'].astype(str).str.match(r'^\s*Total', na=False, case=False)]
zones['District_clean'] = zones['District'].map(normalize_text)
zones[['Vegetation_zone','Phytogeographic_zone']] = zones[['Vegetation_zone','Phytogeographic_zone']].ffill()

print('rows where commune_clean in zone districts:', raw['Commune_clean'].isin(zones['District_clean']).sum())
print(raw[~raw['Commune_clean'].isin(zones['District_clean'])][['Commune','Commune_clean']].drop_duplicates().to_string(index=False))
print('counts by commune in raw where commune_clean in zones:')
print(raw[raw['Commune_clean'].isin(zones['District_clean'])]['Commune_clean'].value_counts())
print('counts by commune where missing zone:')
print(raw[~raw['Commune_clean'].isin(zones['District_clean'])]['Commune_clean'].value_counts())
