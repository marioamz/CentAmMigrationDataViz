# This is a doc to clean and append the data I'll be using

import pandas as pd

def go(path_data, path_codebook, index):
    '''
    This function takes in the paths for a dataframe and the codebook
    that accompanies it, and returns them as pandas dataframes with
    appropriate codes.
    '''

    data = pd.read_csv(path_data)
    codes = pd.read_csv(path_codebook, encoding = 'latin1')

    codedict = dict(codes.set_index(index).groupby(level = 0).apply(lambda x : x.to_dict(orient= 'records')))

    dic = {}
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
        dic[k] = d

    for k, v in dic.items():
        for col in data:
            if col == k:
                data[col] = data[col].map(v).fillna(data[col])

    return data
