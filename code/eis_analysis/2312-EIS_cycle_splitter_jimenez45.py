#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
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
import multiprocessing as mp

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
        if ".csv" in EIS_file:
            df = pd.read_csv(EIS_file)
        else:
            df = MPR_convert(EIS_file) 
    except Exception as e:
        return None
    print(EIS_file)
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
    
    if df is None:  # Skip if conversion failed
        print(EIS_file)
        return None
    
    new_path = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_files_converted'
    filename = os.path.basename(EIS_file).replace('.mpr', '_RUN_').replace('.csv', '_RUN_')
    
    try:
        for i, g in df.groupby('cycle'):
            # Check each individual cycle file
            cycle_filename = os.path.join(new_path, f'{filename}{int(i)}.csv')
            
            if not os.path.exists(cycle_filename):
                g.to_csv(cycle_filename, header=True, index_label=True)
                
    except Exception as e:
        print(f"Error processing {EIS_file}: {e}")
        return None

def EIS_conversion():
    '''
    Wrapper function that takes the database and splits the EIS files into its constituent replicates

    Inputs:
        none

    Outputs:
        split files into individual csv files
    '''
    # loads the folder containing the files from EIS cleaning
    database_csv = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_names'
    output_csv = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_new_files_converted'

    # lists all of the finished/converted files
    done_files = os.listdir(output_csv)
    # Remove the _RUN_X.csv pattern from done files
    done_files_cleaned = pd.Series(done_files).str.replace(r'_RUN_\d+\.csv', '', regex=True).unique()

    # reads the csv and drops empty values
    files = [(os.path.join(root, filename), filename) 
        for root, dirs, filenames in os.walk(database_csv) 
        for filename in filenames]

    df = pd.DataFrame(files, columns=['full_path', 'filename'])

    # Clean the filename for matching (remove .mpr extension)
    df['filename_clean'] = df['filename'].str.replace('.mpr', '')

    # Flag files that are done
    df['is_done'] = df['filename_clean'].isin(done_files_cleaned)

    # Get unfinished files
    df_unfinished = df[df['is_done'] == False].reset_index(drop=True)

    # Process only unfinished files
    results = list(
        map(EIS_CycleSplitter, df_unfinished['full_path'])
    )
    
# Runs the cycle splitter function
EIS_conversion()

# %%
