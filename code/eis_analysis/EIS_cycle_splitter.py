#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Subproject: SPOC
#%%
# loads the relevant EIS processing packages
from galvani import BioLogic

# loads warning handler
import warnings

# loads file handling, data processing and visualization tools
import pandas as pd
import numpy as np
import os

# suppress output calculations
pd.options.mode.chained_assignment = None  # default='warn'

def MPR_convert(file_name):
    '''
    Converts the EIS .mpr files (raw files) into a dataframe,
    then performs data cleaning of the column names.

    Input:
        file_name = EIS file with the format '.mpr'

    Output:
        converted and formated dataframe
    '''
    # handles warnings by removing them from output
    warnings.simplefilter(
        action='ignore', 
        category=FutureWarning)
    # using BioLogic to load .mpr files
    fileConversion = BioLogic.MPRfile(file_name)

    # convert the file to DF
    MPRtoDF = pd.DataFrame(fileConversion.data)

    # removes the column units
    MPRtoDF.columns = MPRtoDF.columns.str.split('/').str[0].str.strip()

    # removes space in names with _
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        ' ', 
        '_')
    
    # removes | in names
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        '|', 
        '')
    
    # removes '(Z)' in names
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        '\s*\(\s*Z\s*\)\s*',
         '')
    
    # removes the negative character
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        '-', 
        '')
    
    # removes the <> characters 
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        '<', 
        '')
    
    # replaces '>' with _meas to avoid duplicate names
    MPRtoDF.columns = MPRtoDF.columns.str.replace(
        '>', 
        '_meas')
    
    # sorts the frequencies by decreasing values
    #MPRtoDF = MPRtoDF.sort_values(by = 'freq', ascending= False)

    # resets the indexing so that it does not follow the time sequence
    MPRtoDF = MPRtoDF.reset_index()
    
    return(MPRtoDF)

def EIS_CycleLabels(EIS_file):
    '''
    Loads an EIS file (.mpr format) then labels each cycle
    based on the return to the initial frequency measurement

    Input = 
        EIS_file - raw EIS files
    
    Output =
        Dataframe from EIS + cycle label
    '''

    # try catch error in EIS dataframe conversion to return nothing
    try:
        # transforms raw EIS file to a dataframe
        if EIS_file.endswith('.mpr'):
            df = MPR_convert(EIS_file)
        elif EIS_file.endswith('.csv'):
            df = pd.read_csv(EIS_file)
    except Exception as e:
        return None

    # finds the max frequency value (can be changed if new files are not finding it)
    max_freq = df['freq'].max()

    # initializes the counter for the cycle count
    counter = 1

    # creates an empty list for the cycle counts
    cycle = []

    # iterates throught the frequencies and labels where the max values are
    for i in df['freq']:
        if i == max_freq:
            cycle.append(counter)
            counter += 1
        else:
            cycle.append(None)

    # creates a new column where the labels are and fills down the cycle number between cycles
    df['cycle'] = cycle
    df['cycle'] = df['cycle'].fillna(method = 'ffill')

    # finds how many cycles and how many points per cycle
    Total_cycles = df['cycle'].value_counts()

    # prints a summary of the total amount of cycles with how many points per cycle
    print('Total cycles | # of points', Total_cycles, EIS_file)

    # outputs a df with cycles added as a column
    return(df)

def EIS_CycleSplitter(EIS_file):
    '''
    Takes labeled dataframes by cycle and saves each cycle
    as a separate csv file for processing

    input = 
        - EIS_file: raw EIS data
    
    output = 
        - converted files saved as csv into destination folder
    '''

    # loads the EIS files and sorts the files by cycle
    df = EIS_CycleLabels(EIS_file)
    
    # sets destination folder for the converted files
    new_path = '~/Git/SPOC_code/data/converted_files'

    # changes the file name and appends the original filename and replicate number
    basename = os.path.basename(EIS_file)
    if basename.endswith('.mpr'):
        filename = basename.replace('.mpr', '_RUN_')
    elif basename.endswith('.csv'):
        filename = basename.replace('.csv', '_RUN_')
    else:
        filename = basename + '_RUN_'                                                   
    filename = rf'{new_path}\{filename}'

    # try catch unsuccessful conversions & repeat files and returns nothing
    try:
        # iterates through the dataframe to save each cycle as a csv
        for i, g in df.groupby('cycle'):
            df = df.sort_values('cycle')
            cycle = df['cycle'][i]

            # checks if file exists so as to not repeat and re-date converted files
            if not os.path.exists(filename):
                g.to_csv(f'{filename}{{:.0f}}.csv'.format(i), header=True, index_label=True)
            else:
                return None
            
    except Exception as e:
        return None
    
def EIS_conversion():
    '''
    Wrapper function that takes the database and splits the EIS files into its constituent replicates

    Inputs:
        none

    Outputs:
        split files into individual csv files
    '''
    # loads the data dictionary from EIS cleaning
    database_csv = '~/Git/SPOC_code/data/database_dictionary/database_dictionary.csv'

    # reads the csv and drops empty values
    df = pd.read_csv(database_csv).dropna()

    # database inspection
    print(df['new_filename'])

    # batch process all of the files in the folder
    list(
        map(lambda x: EIS_CycleSplitter(x),
            df['new_filename']
        )
    )

# Runs the cycle splitter function
EIS_conversion()
#%%
