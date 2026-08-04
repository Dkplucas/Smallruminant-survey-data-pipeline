import pandas as pd
import os
os.chdir(r'C:\Users\lucas\OneDrive\Bureau\Data\Test_CHI2')
zone = pd.read_excel('zones.xlsx', header=None)
print(zone.head(30).to_string())
print('--- rows with District values ---')
for i,row in zone.iterrows():
    if str(row[2]).strip() not in ['nan', 'None', '']:
        print(i, row.to_list())
