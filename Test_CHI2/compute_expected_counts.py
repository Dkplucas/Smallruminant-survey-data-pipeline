import os
import re
import unicodedata
import pandas as pd
import numpy as np

os.chdir(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2')

raw = pd.read_excel('data8_copy.xlsx')
zones_raw = pd.read_excel('zones.xlsx')

sex_col = 'I.- IDENTIFICATION DU CHEF DE MENAGE /Sexe: 1=Masculin, 2=Feminin'
commune_col = 'II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune'


def normalize_text(x):
    if pd.isna(x):
        return None
    x = str(x).strip().lower()
    x = unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode('ascii')
    x = re.sub(r'[^a-z0-9 ]+', '', x)
    x = re.sub(r'\s+', ' ', x).strip()
    return x

survey = raw.rename(columns={commune_col: 'Commune'})[['Commune', sex_col]].copy()
survey['Commune'] = survey['Commune'].astype(str).str.strip()
survey['Commune_clean'] = survey['Commune'].map(normalize_text)
survey['Commune_clean'] = survey['Commune_clean'].replace({'toribossito': 'tori'})

zones = zones_raw.rename(columns={'Vegetation zones': 'Vegetation_zone', 'Phytogeographic zones': 'Phytogeographic_zone', 'District': 'District'})
zones['District'] = zones['District'].astype(str).str.strip().replace({'': np.nan, 'nan': np.nan})
zones['Vegetation_zone'] = zones['Vegetation_zone'].astype(str).str.strip().replace({'': np.nan, 'nan': np.nan})
zones['Phytogeographic_zone'] = zones['Phytogeographic_zone'].astype(str).str.strip().replace({'': np.nan, 'nan': np.nan})


def clean_nbsp(x):
    if pd.isna(x):
        return x
    return x.replace('\xa0', '').strip()

zones['Vegetation_zone'] = zones['Vegetation_zone'].map(clean_nbsp)
zones['Phytogeographic_zone'] = zones['Phytogeographic_zone'].map(clean_nbsp)
zones = zones[~zones['District'].astype(str).str.match(r'^\s*Total', na=False, case=False)]
zones['District_clean'] = zones['District'].map(normalize_text)
zones[['Vegetation_zone', 'Phytogeographic_zone']] = zones[['Vegetation_zone', 'Phytogeographic_zone']].ffill()

joined = survey.merge(zones[['District_clean', 'Vegetation_zone', 'Phytogeographic_zone']], how='left', left_on='Commune_clean', right_on='District_clean')

for zone_col, zone_name in [('Vegetation_zone', 'Vegetation'), ('Phytogeographic_zone', 'Phytogeographic')]:
    df = joined[[sex_col, zone_col]].copy()
    df = df.dropna()
    df[sex_col] = df[sex_col].astype(str)
    df = df[df[sex_col].isin(['1', '2'])]
    df[zone_col] = df[zone_col].astype(str)
    tbl = pd.crosstab(df[sex_col], df[zone_col])
    row_totals = tbl.sum(axis=1)
    col_totals = tbl.sum(axis=0)
    n = tbl.sum().sum()
    expected = np.outer(row_totals, col_totals) / n
    expected_df = pd.DataFrame(expected, index=tbl.index, columns=tbl.columns)
    print(f'=== {zone_name} ===')
    print('Observed table:')
    print(tbl)
    print('\nExpected counts:')
    print(expected_df)
    print('\nAny expected count < 5?')
    print((expected_df < 5).any().any())
    print('Cells below 5:')
    print(expected_df.where(expected_df < 5).stack().dropna().to_string())
    print('')
