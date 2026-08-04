import pandas as pd
import os
import re
import unicodedata
os.chdir(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2')
raw = pd.read_excel('data8_copy.xlsx')
commune_col = 'II- CARACTERISTIQUES DE L’UNITE D’ELEVAGE (UE) /Commune'
raw['Commune'] = raw[commune_col].astype(str).str.strip().replace({'': pd.NA, 'nan': pd.NA})

def normalize_text(x):
    if pd.isna(x):
        return None
    x = str(x).strip().lower()
    x = unicodedata.normalize('NFKD', x).encode('ascii','ignore').decode('ascii')
    x = re.sub(r'[^a-z0-9 ]+', '', x)
    x = re.sub(r'\s+', ' ', x).strip()
    return x

raw['Commune_clean'] = raw['Commune'].map(normalize_text)
raw['Commune_clean'] = raw['Commune_clean'].replace({'toribossito': 'tori'})
print(raw[['Commune', 'Commune_clean']].loc[raw['Commune_clean'].isin(['tori', 'toribossito'])].drop_duplicates().to_string(index=False))
