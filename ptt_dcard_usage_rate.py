import pandas as pd

df_post = pd.read_csv("/Users/lialee/Desktop/ptt_dcard.csv")
print(df_post.shape)

#df_post.head()

df_post = df_post.drop(['Unnamed: 0'], axis=1)

df_post.columns

df_post.head()

import folium
import requests

def get_geojson(geojson_path):
    """
    Importing GeoJSON as as Python dict
    """
    r = requests.get(geojson_path)
    geojson_as_dict = r.json()
    return geojson_as_dict

twCounty = get_geojson("https://s3-ap-northeast-1.amazonaws.com/tw-election-2018/county_moi_1070516.json")

type(twCounty["features"])

len(twCounty["features"])

first_county = twCounty["features"][0]
type(first_county)

first_county.keys()

for i in range(len(twCounty["features"])):
  print(twCounty["features"][i]["properties"])

df_post_percentage = df_post.reset_index(drop=True)
df_post_percentage

df_post_percentage['total_post'].values.sum()

df_post_percentage['log'].values

import math
import numpy as np

empty = []
for i in range(0,21):
    empty = math.log(df_post_percentage['total_post'].values[i])
    empty = np.append(empty)

empty

df_post_percentage['log'].head()

df_post_percentage["posts_percentage"] = df_post_percentage['total_post'] / 161049

df_post_percentage.head()

df_post_percentage["city"] = df_post_percentage["city"].str.replace("台", "臺")

df_post_percentage

import folium

m = folium.Map(location=[24, 121], zoom_start=7, tiles='Mapbox Bright')
twCounty = "https://s3-ap-northeast-1.amazonaws.com/tw-election-2018/county_moi_1070516.json"
folium.Choropleth(
    geo_data=twCounty, #important傳圖資
    name='choropleth',
    data=df_post_percentage, #important傳df
    columns=['city', 'log'],
    key_on='feature.properties.COUNTYNAME', #important圖資要用哪個層級的資料
    fill_color='GnBu', #調色盤
    fill_opacity=1,
    line_opacity=0.7,
    legend_name='posts ratio'
).add_to(m)

folium.LayerControl().add_to(m)

m
