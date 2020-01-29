# -*- coding: utf-8 -*-
"""
Created on Thu Jan 23 09:21:43 2020

@author: Jason
"""
###############################################################################
#### This script generates matrices of mill and mill company names
#### matches for every mill from an external dataset against mills from
#### the aka files
###############################################################################

# #############################
# Imports and path definition
# #############################
import pandas as pd
import dirfind
import numpy as np
from fuzzywuzzy import process,fuzz
from tqdm import tqdm
tqdm.pandas()

# Set working directories
dropbox_dir = dirfind.guess_dropbox_dir()
akas_dir = dropbox_dir + 'Trase/Indonesia/mill_lists/merge/input/'
data_dir = dropbox_dir + 'kraus/data/'

mills_xlsx = data_dir + 'ucsb/mills_20200124.xlsx'
mills_df = pd.read_excel(mills_xlsx)

# Remove whitespaces from data frame
mills_df = mills_df.applymap(lambda x: x.strip() if isinstance(x, str) else x)

# Read mill names aka file
mill_aka_csv = akas_dir + 'aka_mill_names.csv'
mill_aka_df = pd.read_csv(mill_aka_csv,encoding="ISO-8859-1")
mill_aka_df['mill_name'] = mill_aka_df['mill_name'].astype(str)

# Read parent company aka file
pco_aka_csv = akas_dir + 'aka_pco_names.csv'
pco_aka_df = pd.read_csv(pco_aka_csv,encoding="utf-8")
pco_aka_df['parent'] = pco_aka_df['parent'].astype(str)

# Merge aka's with updated mill_list
mill_names = mills_df[['mill_name','trase_code']]
mill_aka_df = pd.concat([mill_aka_df,mill_names],ignore_index=False,sort=True)

pco_names = mills_df[['parent_co','trase_code']]
pco_names = pco_names.rename(columns = {'parent_co':'parent'})
pco_aka_df = pd.concat([pco_aka_df,pco_names],ignore_index=False,sort=True)

## Read data from Valentin's List
gm_xlsx = data_dir + 'ibs/named_unref.xlsx'
gm_df = pd.read_excel(gm_xlsx,encoding="utf-8")
gm_df['mill_name'] = gm_df['matched_resolved_company_name'].astype(str)
gm_df['parent_co'] = gm_df['matched_resolved_company_name'].astype(str)
gm_df['no'] = gm_df['firm_id'].astype(str)

# Rename columns
gm_df= gm_df.rename(columns = {'no':'mill_id'})
gm_df= gm_df.rename(columns = {'parent_co':'parent'})

######################################
# Matching mills based on mill names #
######################################

def get_matches(x):
    matches = process.extract(x['mill_name'], gm_df['mill_name'], scorer=fuzz.ratio,limit=len(gm_df.index))
    return pd.Series({gm_df.iloc[i[2]]['mill_id']: i[1] for i in matches})

mm_df = mill_aka_df.merge(mill_aka_df.sort_values('mill_name').progress_apply(get_matches, axis=1), left_index=True, right_index=True)

# Groupby unique mill code to get highest score only
mm_max_df = mm_df.groupby('trase_code').max().reset_index()


##########################################
# Matching mills based on parent company #
##########################################

def get_matches(x):
    matches = process.extract(x['parent'], gm_df['parent'], scorer=fuzz.ratio,limit=len(gm_df.index))
    return pd.Series({gm_df.iloc[i[2]]['mill_id']: i[1] for i in matches})

mc_df = pco_aka_df.merge(pco_aka_df.sort_values('parent').progress_apply(get_matches, axis=1), left_index=True, right_index=True)

# Groupby unique mill code to get highest score only
mc_max_df = mc_df.groupby('trase_code').max().reset_index()

################################################
# Matching based on names
#################################################

# Match based on name
def replace_score(row,score):
    for col in cols:
        if row[col]  >= score:
            row[col] = 1
        elif row[col] < score:
            row[col] = 0
    return row


def is_num(cols):
    for col in cols:
        try:
            float(col)
            yield col
        except ValueError:
            continue

# Column names based on firm ids      
cols = [col for col in is_num(mm_max_df.columns.values)]
mill_bm_df = mm_max_df.progress_apply(replace_score, score=75, axis=1)

# Replacing 1's with trase codes
mill_bm_df = mill_bm_df.T.replace(1,pd.np.nan).fillna(mill_bm_df['trase_code']).T

# MILL NAMES #
# Subset columns
mbm_df = pd.DataFrame(mill_bm_df, columns=cols)

# Replace 0s' with nan
mbm_df = mbm_df.replace(0, np.nan)

# Transpose and reset index
mbm_tr_df = mbm_df.T.reset_index()
mbm_tr_df.rename(columns={'index':'firm_id'}, inplace=True)

# Get column on matches based on just name
mbm_tr_df['name_matches'] = mbm_tr_df[mbm_tr_df.columns[1:]].apply(lambda x: ','.join(x.dropna().astype(str)),axis=1)
mbm_tr_df = mbm_tr_df[['firm_id','name_matches']]
mbm_tr_df = mbm_tr_df.assign(name_matches=np.core.defchararray.split(mbm_tr_df.name_matches.values.astype(str), ','))

################################################
# Matching based on mill company names
#################################################

# Column names based on firm ids      
cols = [col for col in is_num(mc_max_df.columns.values)]
mco_bm_df = mc_max_df.progress_apply(replace_score, score=75, axis=1)

# Replacing 1's with trase codes
mco_bm_df = mco_bm_df.T.replace(1,pd.np.nan).fillna(mill_bm_df['trase_code']).T

# MILL NAMES #
# Subset columns
mco_df = pd.DataFrame(mco_bm_df, columns=cols)

# Replace 0s' with nan
mco_df = mco_df.replace(0, np.nan)

# Transpose and reset index
mco_tr_df = mco_df.T.reset_index()
mco_tr_df.rename(columns={'index':'firm_id'}, inplace=True)

# Get column on matches based on just name
mco_tr_df['mco_matches'] = mco_tr_df[mco_tr_df.columns[1:]].apply(lambda x: ','.join(x.dropna().astype(str)),axis=1)
mco_tr_df = mco_tr_df[['firm_id','mco_matches']]
mco_tr_df = mco_tr_df.assign(mco_matches=np.core.defchararray.split(mco_tr_df.mco_matches.values.astype(str), ','))

# Join mill and distance match df's
match_df = mbm_tr_df.merge(mco_tr_df, on='firm_id', how='inner', suffixes=('_1', '_2'))

# Clean up matched mill and mill company columns
match_df['comb_match'] = [list(set(a).intersection(set(b))) for a, b in zip(match_df.name_matches, match_df.mco_matches)]
match_df['name_matches'] = match_df.name_matches.apply(lambda x: ', '.join([str(i) for i in x]))
match_df['mco_matches'] = match_df.mco_matches.apply(lambda x: ', '.join([str(i) for i in x]))
match_df['comb_match'] = match_df.comb_match.apply(lambda x: ', '.join([str(i) for i in x]))
match_df = match_df.replace(r'^\s*$', np.nan, regex=True)

# Join matched dataframe and original input dataset
match_df['firm_id'] = match_df['firm_id'].astype(int)
match_add_df = match_df.merge(gm_df[['firm_id','matched_resolved_company_name','district_name','kec_name','village_name','year']], on='firm_id', how='outer')

# Export to excel
match_add_df.to_excel(data_dir + 'ucsb/md_millMatching.xlsx',index=False)