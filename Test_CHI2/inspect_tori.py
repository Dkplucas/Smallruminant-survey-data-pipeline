import pandas as pd
zones = pd.read_excel('zones.xlsx')
zones = zones.rename(columns={'Vegetation zones':'Vegetation_zone','Phytogeographic zones':'Phytogeographic_zone','District':'District'})
zones['District'] = zones['District'].astype(str).str.strip().replace({'': pd.NA, 'nan': pd.NA})
for d in zones['District'].dropna().unique():
    if 'tori' in str(d).lower() or 'boss' in str(d).lower():
        print(repr(d))
print('---')
print(zones['District'].dropna().value_counts().head(40).to_string())
