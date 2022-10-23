# -*- coding: utf-8 -*-
"""
Created on Wed Sep 28 09:20:55 2022

@author: Jason
"""
import time
from selenium import webdriver
import pandas as pd
import dirfind

dropbox_dir = dirfind.guess_dropbox_dir()
data_dir = dropbox_dir + "kraus/data/ucsb/"

driver = webdriver.Chrome()
website = "https://rspo.org/palmtrace"

driver.get(website)
time.sleep(2)

# for chart in range(2):
#     for series in range(22):
#         temp = driver.execute_script('return window.Highcharts.charts[{}]'
#                                 '.series[{}].options.data'.format(chart,series))
#         print("Chart", chart, ", Series", series, ":", temp)           

# for chart in range(2,4):
#     for series in range(16):
#         temp = driver.execute_script('return window.Highcharts.charts[{}]'
#                                 '.series[{}].options.data'.format(chart,series))
#         print("Chart", chart, ", Series", series, ":", temp)
         
# Get values and chart name       
my_data = []

for chart in range(2):
    for series in range(22):
        temp = driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.data'.format(chart,series))
        temp.insert(0, driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.name'.format(chart,series)))
        my_data.append(temp)

for chart in range(2,4):
    for series in range(16):
        temp = driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.data'.format(chart,series))
        temp.insert(0, driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.name'.format(chart,series)))
        my_data.append(temp)

df = pd.DataFrame(my_data)
print(df)

# Get chart title
my_data1 = []

for chart in range(2):
    for series in range(22):
        title = driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.name'.format(chart,series))
        my_data1.append(chart)

for chart in range(2,4):
    for series in range(16):
        title = driver.execute_script('return window.Highcharts.charts[{}]'
                                '.series[{}].options.name'.format(chart,series))
        my_data1.append(chart)

df1 = pd.DataFrame(my_data1)
df1.rename(columns={0: 'Chart'}, inplace=True)
print(df1)

chart = [0,1,2,3]
name = ["CSPO On Market Trades", "CSPKO On Market Trades", "IS CPO Credit Sales", "IS CPKO Credit Sales"]

df1['chart_name'] = df1['Chart'].replace(chart,name)
del df1['Chart']


# Merge dataframes and clean up
df_merged = pd.concat([df1,df], axis=1)

df_merged = df_merged.reset_index()
df_merged.rename(columns={
            0: 'type',1: 'January', 2: 'February',3: 'March',
            4: 'April', 5: 'May',6: 'June',
            7: 'July', 8: 'August', 9: 'September',
            10: 'October', 11: 'November', 12: 'December'},
            inplace= True)

df_clean = pd.melt(df_merged, id_vars=['chart_name','type'], value_vars=['January','February','March','April','May','June','July',
                                                     'August','September','October','November','December'])

# clean up values with square brackets
df_clean["value"] = df_clean["value"].astype(str).apply(lambda x: x.replace('[','').replace(']','')).astype(float)
# check column type
df_clean.dtypes

# get year
df_clean['year'] = df_clean['type'].str.split('-').str[1]
df_clean['type'] = df_clean.type.str.replace("(-).*","")
# sort by year and month and export
df_clean = df_clean.sort_values(['chart_name', 'year'])
# rename column to month
df_clean.rename(columns={'variable': 'month'},inplace= True)
# reorder columns
df_clean = df_clean.reindex(['chart_name','type','year','month','value'], axis=1)


# Export to csv
df_clean.to_csv(data_dir + 'rspo_palmtrace.csv',index=False)
