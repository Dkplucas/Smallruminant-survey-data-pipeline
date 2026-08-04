import pandas as pd
import unicodedata
import re
from pathlib import Path

p = Path(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2\data8.xlsx')
q = Path(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2\zones.xlsx')

df = pd.read_excel(p)
zd = pd.read_excel(q)
zd2 = zd[['Vegetation zones', 'Phytogeographic zones', 'District']].copy()
zd2.columns = ['VegetationZone', 'PhytogeographicZone', 'District']
zd2['VegetationZone'] = zd2['VegetationZone'].ffill()
zd2['PhytogeographicZone'] = zd2['PhytogeographicZone'].ffill()
zd2 = zd2[~zd2['District'].fillna('').astype(str).str.contains('total', case=False, na=False)]

def norm(x):
    if pd.isna(x):
        return ''
    s = str(x)
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(ch for ch in s if not unicodedata.combining(ch))
    s = re.sub(r'[^A-Za-z0-9]+', ' ', s.lower()).strip()
    return s

zd2['DistrictNorm'] = zd2['District'].map(norm)
zd2['DistrictNormBase'] = zd2['DistrictNorm'].str.replace(r'[- ].*$', '', regex=True)

age_col = "I.- IDENTIFICATION DU CHEF DE MENAGE /Catégorie d'âge : 1= > 50ans, 2=20 a 30, 3=30 a 50"
commune_col = "II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune"

survey = df[[age_col, commune_col]].copy()
survey.columns = ['AgeCategory', 'Commune']
survey['AgeCategory'] = survey['AgeCategory'].astype(str).str.strip()
survey['Commune'] = survey['Commune'].astype(str).str.strip()
survey['CommuneNorm'] = survey['Commune'].map(norm)
survey['CommuneNormBase'] = survey['CommuneNorm'].str.replace(r'[- ].*$', '', regex=True)

joined_exact = survey.merge(
    zd2[['DistrictNorm', 'VegetationZone', 'PhytogeographicZone']],
    left_on='CommuneNorm', right_on='DistrictNorm', how='left'
)
fallback = joined_exact[joined_exact['VegetationZone'].isna()].drop(columns=['DistrictNorm', 'VegetationZone', 'PhytogeographicZone']).merge(
    zd2[['DistrictNormBase', 'VegetationZone', 'PhytogeographicZone']],
    left_on='CommuneNormBase', right_on='DistrictNormBase', how='left'
)

joined2 = pd.concat([
    joined_exact[joined_exact['VegetationZone'].notna()][['AgeCategory','Commune','CommuneNorm','CommuneNormBase','VegetationZone','PhytogeographicZone']],
    fallback[fallback['VegetationZone'].notna()][['AgeCategory','Commune','CommuneNorm','CommuneNormBase','VegetationZone','PhytogeographicZone']]
], axis=0, ignore_index=True)

print('joined rows', len(joined2), 'unmatched', len(survey) - len(joined2))
print('age counts:')
print(survey['AgeCategory'].value_counts().sort_index())

for zone_col in ['VegetationZone', 'PhytogeographicZone']:
    print('\n===', zone_col, '===')
    tab = pd.crosstab(joined2['AgeCategory'], joined2[zone_col])
    print('shape', tab.shape)
    print(tab)
    total = tab.values.sum()
    row_marg = tab.sum(axis=1)
    col_marg = tab.sum(axis=0)
    exp = pd.DataFrame(index=tab.index, columns=tab.columns)
    for r in tab.index:
        for c in tab.columns:
            exp.loc[r, c] = row_marg[r] * col_marg[c] / total
    print('min expected', exp.min().min())
    print('count<5', (exp.astype(float) < 5).sum().sum())
