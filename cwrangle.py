# This is a doc to clean and append the data I'll be using

import pandas as pd

usa2017 = {'sexo':'sex', 'edad':'age', 'p2':'read_write', 'p6':'head_house', \
'p11_1d':'home_dept', 'p11_1m':'home_city', 'p14_1':'mex_port', 'p15':'trans_crossm', \
'p17_1c':'coyote_usd', 'p19_1e':'first_state', 'p19_1l':'first_city', 'p19_2e':'second_state', \
'p19_2l':'second_city', 'p20e':'long_state', 'p20l':'long_city', 'p26':'us_entry', \
'p30':'trans_crossu', 'p31':'reason', 'p33':'fellow_trav'}

usa2016 = {'sexo':'sex', 'edad':'age', 'p2':'read_write', 'p6':'head_house', \
'p11_1d':'home_dept', 'p11_1m':'home_city', 'p14_1':'mex_port', 'p15':'trans_crossm', \
'p17_1c':'coyote_usd', 'p19e1':'first_state', 'p19l1':'first_city', 'p19e2':'second_state', \
'p19l2':'second_city', 'p20e':'long_state', 'p20l':'long_city', 'p26':'us_entry', \
'p30':'trans_crossu', 'p31':'reason', 'p33':'fellow_trav'}

usa2015 = {'sexo': 'sex', 'edad':'age', 'p1':'read_write', 'p6':'head_house', \
'p11_1d':'home_dept', 'p11_1m':'home_city', 'p14_1':'mex_port', 'p15':'trans_crossm', \
'p17_1c':'coyote_usd', 'p19e1':'first_state', 'p19l1':'first_city', 'p19e2':'second_state',\
'p19l2': 'second_city', 'p20e':'long_state', 'p20l':'long_city', 'p26':'us_entry', \
'p30':'trans_crossu', 'p31':'reason', 'p33':'fellow_trav'}

usa2014 = {'SEXO': 'sex', 'EDAD':'age', 'p1':'read_write', 'p6':'head_house', \
'p11_1d':'home_dept', 'p11_1m':'home_city', 'p14_1':'mex_port', 'p15':'trans_crossm', \
'p17_1c':'coyote_usd', 'p19e1':'first_state', 'p19c1':'first_city', 'p19e2':'second_state',\
'p19c2': 'second_city', 'p20e':'long_state', 'p20c':'long_city', 'p25':'us_entry',\
'p28_1':'trans_crossu', 'p29':'reason', 'p31':'fellow_trav'}

mex2017 = {'p1':'sex', 'p2':'age', 'p4':'read_write', 'p8':'head_house', \
'p11_1d':'home_dept','p11_1m':'home_city','p15':'mex_port','p19':'trans_crossm',\
'p20_1c':'coyote_usd','p22_1e':'first_state','p22_1l':'first_city', 'p22_2e':'second_state',\
'p22_2l':'second_city','p24e':'long_state','p24l':'long_city',\
'p18_1':'fellow_travs', 'p34e':'state_deported','p34m':'city_deported'}

mex2016 = {'p1':'sex', 'p2':'age', 'p4':'read_write', 'p8':'head_house', \
'p11_1d':'home_dept','p11_1m':'home_city','p15':'mex_port','p19':'trans_crossm',\
'p20_1c':'coyote_usd','p22_1e':'first_state','p22_1l':'first_city', 'p22_2e':'second_state',\
'p22_2l':'second_city','p24e':'long_state','p24l':'long_city',\
'p18_1':'fellow_travs', 'p34e':'state_deported','p34m':'city_deported'}

mex2015 = {'p1':'sex', 'p2':'age', 'p3':'read_write', 'p8':'head_house', \
'p11_1d':'home_dept','p11_1m':'home_city','p15':'mex_port','p19':'trans_crossm',\
'p20_1c':'coyote_usd','p22_1e':'first_state','p22_1l':'first_city', 'p22_2e':'second_state',\
'p22_2l':'second_city','p24e':'long_state','p24l':'long_city',\
'p18_1':'fellow_travs', 'p34e':'state_deported','p34m':'city_deported'}

mex2014 = {'p1':'sex', 'p2':'age', 'p3':'read_write', 'p8':'head_house', \
'p11_1d':'home_dept','p11_1m':'home_city','p14':'mex_port','p18':'trans_crossm',\
'p19_1c':'coyote_usd','p21e1':'first_state','p21c1':'first_city', 'p21e2':'second_state',\
'p21c2':'second_city','p23e':'long_state','p23c':'long_city',\
'p17_1':'fellow_travs', 'p43e':'state_deported','p43m':'city_deported'}




def go(path_data, path_codebook, index, country, year):
    '''
    This function takes in the paths for a dataframe and the codebook
    that accompanies it, index variable, country, and year, and
    returns them as pandas dataframes with appropriate codes.
    '''

    l = []

    if country == 'USA':
        if year == 2017:
            for k in usa2017:
                l.append(k)
                dicr = usa2017
        elif year == 2016:
            for k in usa2016:
                l.append(k)
                dicr = usa2016
        elif year == 2015:
            for k in usa2015:
                l.append(k)
                dicr = usa2015
        else:
            for k in usa2014:
                l.append(k)
                dicr = usa2014

    else:
        if year == 2017:
            for k in mex2017:
                l.append(k)
                dicr = mex2017
        elif year == 2016:
            for k in mex2016:
                l.append(k)
                dicr = mex2016
        elif year == 2015:
            for k in mex2015:
                l.append(k)
                dicr = mex2015
        else:
            for k in mex2014:
                l.append(k)
                dicr = mex2014

    df = read_csv(path_data, path_codebook, index, l)
    renamed = df.rename(columns=dicr)

    return renamed


def read_csv(path_data, codebook, index, cols):
    '''
    Helper function to do all the work.
    '''

    data = pd.read_csv(path_data, usecols=cols)
    codes = pd.read_csv(codebook, encoding = 'latin1')
    codes = codes.loc[codes[index].isin(cols)]

    codedict = dict(codes.set_index(index).groupby(level = 0).apply(lambda x : x.to_dict(orient= 'records')))

    dicts = {}
    for k, v in codedict.items():
        keys = []
        values = []
        for dic in v:
            for j in dic.values():
                if type(j) is int:
                    keys.append(j)
                else:
                    values.append(j)
                d = dict(zip(keys, values))
            dicts[k] = d

    for k, v in dicts.items():
        for col in data:
            if col == k:
                data[col] = data[col].map(v).fillna(data[col])
    return data
