import pandas as pd
import numpy as np
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler

def data_transform(csv_name):
    np.random.seed(1) # for reprodusability of results
    df = pd.read_csv(csv_name)

    #####################
    ### IMPUTING NA'S ###
    #####################

    #Imputing NA's with 0, as 0 represent no street connected to the lot
    df[['LotFrontage']] = df[['LotFrontage']].fillna(0)
    df[['BsmtFullBath']] = df[['BsmtFullBath']].fillna(0)
    df[['BsmtHalfBath']] = df[['BsmtHalfBath']].fillna(0)
    df[['FullBath']] = df[['FullBath']].fillna(0)
    df[['FullBath']] = df[['FullBath']].fillna(0)
    df[['GarageCars']] = df[['GarageCars']].fillna(0)

    #Electrical
    imp = SimpleImputer(missing_values=np.nan, strategy='most_frequent')
    imp.fit(df[['Electrical']])
    df[['Electrical']] = pd.DataFrame(imp.transform(df[['Electrical']]), columns = ['Electrical'])

    #GarageType (impute + convert to binary)
    df[['GarageType']] = df[['GarageType']].fillna('N')
    df['Garage'] = (df['GarageType'] != 'N').astype(int)

    df[['Functional']] = df[['Functional']].fillna('Typ')

    imp.fit(df[['KitchenQual']])
    df[['KitchenQual']] = pd.DataFrame(imp.transform(df[['KitchenQual']]), columns = ['KitchenQual'])

    #Total porch area
    df['Total.Porch']= df['OpenPorchSF']+ df['EnclosedPorch'] + df['ScreenPorch']

    #Total rooms
    df['TotRmsAbvGrd'] = df['TotRmsAbvGrd'] - df['BedroomAbvGr']

    #Total baths
    df['Total.HalfBath'] = df['BsmtFullBath'] + df['FullBath']
    df['Total.FullBath'] = df['BsmtHalfBath'] + df['HalfBath']

    # Subtracting num of bedrooms from total number of rooms as bedrooms are represented in a separate feature
    df['Total.RmsAbvGrd'] = df['TotRmsAbvGrd'] - df['BedroomAbvGr']

    #Rename one 'MSZoning' value for dummification
    df.loc[(df['MSZoning'] == 'C (all)'), 'MSZoning'] = 'C'

    #Converting numerical to categorical
    df = df.replace({'MSSubClass': {20: 'SC20', 30: 'SC30', 40: 'SC40', 45: 'SC45', 50: 'SC50', 60: 'SC60', 70: 'SC70', 
                                    75: 'SC75', 80: 'SC80', 85: 'SC85', 90: 'SC90', 120: 'SC120', 150: 'SC150', 160: 'SC160',
                                    180: 'SC180', 190: 'SC190'} })

    #####################################
    ### CONVERTING FEATURES TO BINARY ###
    #####################################

    #convert CentralAir to boolean
    df['CentralAir'] = (df['CentralAir'] != 'N').astype(int)

    #convert Fireplaces to boolean
    df.loc[(df['Fireplaces'] > 0), 'Fireplaces'] = 1

    # Create a binary feature 'Basement' that indicates if the home has a basement
    df['Basement'] = [1 if row > 0 else 0 for row in df['TotalBsmtSF']]

    # Pool Area
    df['PoolArea'] = [1 if row > 0 else 0 for row in df['PoolArea']]

    # YearRemodAdd
    # if the year listed in both columns is the same, there has not been a renovation
    # Turn into binary variable "Reno"
    Year_temp = df.YearRemodAdd - df.YearBuilt
    df['Reno'] = [0 if x == 0 else 1 for x in Year_temp]

    # Conditions 1 + 2
    # Only "Positive Feature" really seems to matter here -> binarize
    PosFeat1 = [1 if 'Pos' in x else 0 for x in df['Condition1']]
    PosFeat1 = pd.Series(PosFeat1)
    PosFeat2 = [1 if 'Pos' in x else 0 for x in df['Condition2']]
    PosFeat2 = pd.Series(PosFeat2)
    temp = pd.DataFrame()
    temp['PosFeat1'] = PosFeat1
    temp['PosFeat2'] = PosFeat2
    temp['PosFeat'] = temp.PosFeat1 + temp.PosFeat2
    # binarize and put into df
    df['PosFeat'] = [1 if x > 0 else 0 for x in temp['PosFeat']]
    # Just in Case:
    # Also binarize "Norm" col
    # if Cond1 = norm, so does Cond2, so we only need to look @ one col
    df['Norm'] = [1 if 'Norm' in x else 0 for x in df['Condition1']]

    df['TwoFloors'] = [1 if "2" in x else 0 for x in df['HouseStyle']]

    df['OneFamHome'] = [1 if row == "1Fam" else 0 for row in df['BldgType']]
    df['WoodDeck'] = [1 if row > 0 else 0 for row in df['WoodDeckSF']]
    df['Porch'] = [1 if row > 0 else 0 for row in df['Total.Porch']]


    ###################################
    ### CONVERTING ORDINAL FEATURES ###
    ###################################
    quality_scale = {'Ex': 5, 'Gd': 4, 'TA': 3, 'Fa': 2, 'Po': 1}

    df = df.replace({'HeatingQC': quality_scale,
                     'KitchenQual': quality_scale,
                     'PavedDrive': {'N': 0, 'Y': 2, 'P': 1},
                     'Functional': {'Sal': 1, 'Sev': 2, 'Maj2': 3, 'Maj1': 4, 
                                    'Mod': 5, 'Min2': 6, 'Min1': 7, 'Typ': 8}
    })

    df['LotFrontage'] = [3 if x > 100 else 2 if x > 60 else 1 if x > 0 else 0 for x in df['LotFrontage']]
    df['LotArea'] = [2 if x > 12000 else 1 if x > 7000 else 0 for x in df['LotArea']]


    #########################
    ### SELECTED FEATURES ###
    #########################
    ordinal_features = ['PavedDrive', 'Functional', 'OverallQual', 'KitchenQual', 'LotFrontage','HeatingQC', 'LotArea']
    nominal_features = ['MSSubClass', 'LandContour', 'Utilities', 'LotConfig', 'LandSlope', 
                        'Neighborhood', 'Electrical', 'SaleCondition', 'MSZoning']
    binary_features = ['CentralAir', 'Basement', 'Fireplaces', 'Garage', 'Reno', 'OneFamHome', 
                       'PosFeat', 'Norm', 'TwoFloors', 'PoolArea', 'WoodDeck', 'Porch']
    contin_features = ['GrLivArea']
    discrete_features = ['Total.HalfBath', 'Total.FullBath', 'BedroomAbvGr', 'TotRmsAbvGrd', 'GarageCars']
    date_features = ['YearBuilt', 'YrSold']

    ##########################
    ### LOG TRANSFORM AREA ###
    ##########################   
    df['GrLivArea'] = df['GrLivArea'].apply(lambda x: np.log(x))

    #####################
    ### DUMMIFICATION ###
    #####################  
    nominal_dummies = df[nominal_features]
    nominal_dummies = pd.get_dummies(nominal_dummies, drop_first=True)  


    ##########################
    ### GETTING FINAL DATA ###
    ##########################  
    selection = ordinal_features+binary_features+contin_features+discrete_features+date_features
    # final_data = df[selection]
    final_data = pd.concat([df[selection], nominal_dummies], axis = 1)

    return(final_data)


