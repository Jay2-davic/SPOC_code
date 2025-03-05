#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
#Subproject: SPOC

#%% Loads relevant Packages

# loads the relevant EIS processing packages
from impedance import preprocessing as proc
from impedance import visualization as viz
from impedance.validation import linKK
from impedance import models
from impedance.models import circuits as mods
from galvani import BioLogic

# loads warning handler
import warnings

# loads file handling, data processing and visualization tools
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import io
import sys
from contextlib import redirect_stdout
import textwrap
from functools import partial

# loads export tools
import csv

# loads math tools
import math

# suppress output calculations
pd.options.mode.chained_assignment = None  # default='warn'


#%% loads the mpr raw file conversion functions
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
    MPRtoDF = MPRtoDF.sort_values(by = 'freq', ascending= False)

    # resets the indexing so that it does not follow the time sequence
    MPRtoDF = MPRtoDF.reset_index()
    
    return(MPRtoDF)
#%% takes the EIS results, cleans the data then returns the formatted Z and freq values
def ApplyFits(file_name):
    '''
    Converts raw data Z values into an array of complex equations and 
    the frequencies as an array and cleans the data.

    Inputs:
        file_name = EIS file with the format '.mpr'
    
    Outputs: 
        converted Z and frequencies
    '''
    # runs the MPR opener function
    MPR_df = MPR_convert(file_name)

    # converts into a np array with the equation Re - Im*j (complex function)
    Z = pd.array(
        MPR_df['Re(Z)'] 
        - MPR_df['Im(Z)']*1j)
    frequencies = pd.array(MPR_df.freq)

    # cleans the data by removing values freq < 0
    frequencies, Z = proc.ignoreBelowX(
        frequencies, 
        Z)

    return(
        Z, 
        frequencies)

def ApplyFits2(csv_files):
    '''
    Converts raw data Z values into an array of complex equations and 
    the frequencies as an array and cleans the data.

    Inputs:
        file_name = EIS file with the format '.mpr'
    
    Outputs: 
        converted Z and frequencies
    '''

    csv_files = pd.read_csv(csv_files)
    csv_files = csv_files.iloc[:, 2:].sort_values(by = 'time', ascending= True).reset_index()

    # converts into a np array with the equation Re - Im*j (complex function)
    Z = pd.array(
        csv_files['Re(Z)'] 
        - csv_files['Im(Z)']*1j)
    frequencies = pd.array(csv_files.freq)

    # cleans the data by removing values freq < 0
    frequencies, Z = proc.ignoreBelowX(
        frequencies, 
        Z)

    return(
        Z, 
        frequencies)

#%% loads the equations for calculations
def calcValues(
        Z_actual, 
        Z_predict):
    '''
    Calculates the statistics of the model fits and the complex components of the Z data

    Input:
        Z_actual = Z of the data
        Z_predict = predicted Z of the data using a circuit
    
    Output:
        rmse_real = real component RMSE
        rmse_imag = imaginary component RMSE
        Z_real = Z of the real component
        Z_imag = Z of the imaginary component
    '''

    # calculates residuals from fitted prediction
    resid = np.array(Z_actual 
                     - Z_predict)
    Z_real = resid.real/np.abs(Z_actual)
    Z_imag = resid.imag/np.abs(Z_actual)

    # calculates the mse and RMSE for the real component
    mse_real = np.square(Z_real).mean()
    rmse_real = math.sqrt(mse_real)

    # calculates the mse and RMSE for the imaginary component
    mse_imag = np.square(Z_imag).mean()
    rmse_imag = math.sqrt(mse_imag)

    return(
        rmse_real, rmse_imag, 
        Z_real, Z_imag)

#%% loads the cleaning functions for the EIS fitted outputs
def printToDF(text_string):
    '''
    Converts the print string output of the predicted values 
    into a dataframe

    Input:
        text_string = output that prints a summary of the fits

    Output:
        A dataframe of the results from the fits
    '''

    # Create a custom output stream using io.StringIO
    output_stream = io.StringIO()
    
    # prints the output of the rmse for both real and imaginary
    with redirect_stdout(output_stream):
        print(text_string)

    # Redirect the standard output to the custom output stream
    original_stdout = sys.stdout
    sys.stdout = output_stream

    # Reset the standard output
    sys.stdout = original_stdout

    # Get the captured print content from the output stream
    captured_content = output_stream.getvalue()

    # Split the captured content into lines
    captured_lines = captured_content.strip().split('\n')

    # Create a DataFrame from the captured lines
    data = {'Printed Content': captured_lines}
    df = pd.DataFrame(data)

    # outputs a dataframe
    return(df)

def EISresult_cleaner(text_string):
    '''
    Cleans the output into a mergeable dataframe

    Input:
        text_string = output that prints a summary of the fits

    Output:
        .csv-ready dataframe of the EIS metadata from fits
    '''
    # converts print output to a dataframe
    df = printToDF(text_string)

    # defines the header information, initial and fitted values
    df_headerInfo = df.loc[0:2]
    df_initial = df.loc[5:9]
    df_fitted = df.loc[12:16]

    # performs the data cleaning and outputs a single row dataframe
    df_initial_ = EIS_InitialCleaner(df_initial)
    df_fitted_ = EIS_FittedCleaner(df_fitted)
    df_headerInfo_ = EIS_HeaderCleaner(df_headerInfo)
        
    # adds prefixes on the values
    df_initial_ = df_initial_.add_prefix('InitialGuess.')
    df_fitted_ = df_fitted_.add_prefix('FittedValue.')
    df_headerInfo_ = df_headerInfo_.add_prefix('EISmeasurement.')

    # concatenates the results
    df_combined = pd.concat(
        [
            df_initial_.reset_index(drop = True), 
            df_fitted_.reset_index(drop = True)
        ], 
        axis = 1)
    
    df_combined = pd.concat(
        [
            df_combined, 
            df_headerInfo_.reset_index(drop = True)
        ], 
        axis = 1)
    
    return(df_combined)

def EIS_InitialCleaner(df):
    '''
    Converts the printed result from the impedancy.py into a dataframe
    of the initial guess inputs from EIS fitting

    Input:
        df = Outputted print result from fitting
    Output:
        single row dataframe
    '''
    # label as column for easy cleaning
    df.columns = ['column']

    # uses ' = ', [, and ] as delimiters for splitting the columns to single row
    df['A'] = df['column'].str.split(
        ' = | \[|\]', 
        expand = False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')
    df = np.transpose(df)

    # uses first row as header row, then removes blanks
    df = df.rename(
        columns = df.iloc[0], 
        inplace = False).drop([
            0,2,3
            ])
    df = df.rename(
        columns=lambda x: x.strip(), 
        inplace=False)

    return(df)

def EIS_HeaderCleaner(df):
    '''
    Converts the printed result from the impedancy.py into a dataframe
    of the circuit model used for EIS fitting

    Input:
        df = Outputted print result from fitting
    Output: 
        single row dataframe
    '''
    # label as column for easy cleaning
    df.columns = ['column']

    # uses ': ' as delimiter for splitting the column to single row
    df['A'] = df['column'].str.split(
        ': ', 
        expand = False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')
    df = np.transpose(df)

    # replaces 'Circuit string' with 'Circuit'
    df[1] = df[1].str.replace(
        ' string',
        '')

    # uses first row as header row, then removes blanks
    df = df.rename(
        columns = df.iloc[0], 
        inplace = False).drop([0])
    df = df.rename(
        columns=lambda x: x.strip(), 
        inplace=False)

    return(df)

def EIS_FittedCleaner(df):
    '''
    Converts the printed result from the impedancy.py into a dataframe
    of the fitted model variables used from EIS fitting

    Input:
        df = Outputted print result from fitting
    Output:
        single row dataframe    
    '''
    # label as column for easy cleaning
    df.columns = ['column']

    # uses ' = ', '[', ']', and '(+' as delimiter for splitting the column to single row
    df['A'] = df['column'].str.split(
        ' = | \[|\]|\(+', 
        expand = False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')

    # removes the parenthesis and +/-
    df[2] = df[2].str.replace(
        '\+/\- |\)', 
        '')
    df[1] = df[1].str.replace(
        ' ', 
        '')
    df[0] = df[0].str.replace(
        ' ', 
        '')

    # uses first row as header row, then removes blanks
    df_act = df[[0,1]]
    df_act = np.transpose(df_act)
    df_act = df_act.rename(
        columns = df_act.iloc[0]).drop([0])
    df_act = df_act.rename(
        columns=lambda x: x.strip(), 
        inplace=False)

    # cleans the st dev/error of the fitted coefficients
    df_err = df[[0,2]]
    df_err = np.transpose(df_err)
    df_err = df_err.rename(
        columns = df_err.iloc[0]).drop([0])
    df_err = df_err.add_suffix('_stDev')

    # concatenates the actual value with its error
    combined = pd.concat([
        df_act.reset_index(drop = True), 
        df_err.reset_index(drop = True)
        ], 
        axis = 1)
    
    return(combined)

# %% loads the fitting functions
def ApplyRandlesFits(
        file_name, R0_guess, 
        R1_guess, Wo1_0_guess, 
        Wo1_1_guess, C1_guess, 
        head_count, save_file,
        global_opt):
    '''
    Performs randles fitting on the data sets

    Inputs: 
        file_name = .mpr file of the EIS
        R0_guess = initial guess for electrolyte resistance
        R1_guess = initial guess for charge transfer resistance
        Wo1_0_guess/Wo1_1_guess = initial guesses for Warburg coefficients
        C1_guess = initial guesses for double layer capacitance
        head_count = parses the data from 1 to specified count 
        save_file = saves the file in the same directory

    Outputs: 
        randles with CPE fitting parameters
        Nyquist plots with fitting
        Bode Plots with fitting
        Residual plots of real and imaginary components of the fits
        Dataframe with inputs and output models
    '''

    # runs the randles circuit model with initial guesses defined by user
    randles = mods.Randles(
        initial_guess = [
            R0_guess, R1_guess, 
            Wo1_0_guess, Wo1_1_guess, 
            C1_guess
            ]
        )

    
    # runs the function ApplyFits to load fitting parameters
    Z, frequencies = ApplyFits2(file_name)

    # parse data based on how many data points as defined by user
    frequencies_fit = frequencies[:head_count]
    Z_fit = Z[:head_count]

    # applies randles fit and prints their fit parameters
    randles_fit = randles.fit(
        frequencies_fit,
        Z_fit, 
        global_opt = global_opt,
        method = 'trf'
        ) #global opt does not work for Randles #*****

    # outputs Nyquist plot displaying the fits
    # nyquist_plot = randles.plot(
    #     f_data = frequencies_fit, 
    #     Z_data = Z_fit, 
    #     kind = 'nyquist')
    
    # outputs a Bode plot displaying the fits
    # bode_plot = randles.plot(
    #     f_data = frequencies_fit, 
    #     Z_data = Z_fit, 
    #     kind = 'bode')
    
    # outputs the curve fitting
    Z_randles = randles.predict(frequencies_fit)
    
    # calculates residuals and complex components from the fitted prediction
    rmse_real, rmse_imag, Z_real, Z_imag = calcValues(
        Z_fit, 
        Z_randles)

    # plots the residuals
    # fig, ax = plt.subplots()
    # residual_plot = viz.plot_residuals(
    #     ax, frequencies_fit, 
    #     Z_real, Z_imag, 
    #     y_limits = (-10,10))
    
    # plots the nyquist plot as a subplot
    fig, ax1 = plt.subplots(2, 1, figsize = (10,10))
    ax1[0].nyquist_plot = randles_fit.plot(
        ax1[0],
        f_data = frequencies_fit, 
        Z_data = Z_fit, 
        kind = 'nyquist')
    ax1[0].legend(['Data', 'Fitted'], loc = 'upper right')
    ax1[0].set_title(f'Nyquist Plot', size = 20)

    # plots the residual plot next to the nyquist plot
    ax1[1].residual_plot = viz.plot_residuals(
        ax1[1], frequencies_fit, 
        Z_real, Z_imag, 
        y_limits = (-20,20))
    ax1[1].set_title('Fit Residuals', size = 20)
    plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}', 80)), size = 15)
    plt.tight_layout()    
    
    # outputs randles fitting parameters
    print('RMSE Real = ', 
          rmse_real)
    print('RMSE Imaginary = ', 
          rmse_imag)
          
    # combines the head count and RMSE values as a dataframe
    stats_df = [[
        file_name, head_count, 
        rmse_real, rmse_imag,
        head_count, #nyquist_plot,
        #bode_plot, #residual_plot
        ]]
    
    # adds column names to the dataframe
    stats_df_names = [
        'EIS_file', 'head_count', 
        'RMSE_real', 'RMSE_imag',
        'head_count', #'nyquist_plot',
        #'bode_plot', #'residual_plot'
        ]
    stats_df = pd.DataFrame(
        stats_df, 
        columns = stats_df_names)

    # saves the outputs as a single line dataframe
    output_df = EISresult_cleaner(randles_fit)
    output_df = pd.concat(
        [stats_df, output_df], 
        axis = 1)
    
    output_file_dir = r'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_results_from_Jay_2'
    basename = os.path.basename(file_name)
    
    # saves as same folder and name as the mpr file
    if save_file == True:
        # takes the file name and cleans the naming scheme then specifies the model
        file_nameCSV = basename.replace(
            ' ', 
            '_').replace(
                '.csv', 
                '__randles.csv')
        output_df.to_csv(
            output_file_dir + r'\\' + file_nameCSV, 
            index = False)
        print(
            'dataframe saved as', 
            f'{file_nameCSV}')
        file_namePlot = basename.replace(
            ' ',
            '_').replace(
                '.csv',
                '_randles.png')
        print(f'{file_name}')
        plt.savefig(output_file_dir + r'\\' + file_namePlot, dpi = 600, format = 'png')
    else:
        print('Output not saved')
        
# %%

# loads directory and filters files that are already done
directory = r'C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\EIS_full_files.csv'
df = pd.read_csv(directory)

# resets indexing so the first file starts at [0]
df = df[(df['file_done'] == True)].reset_index()

#%%
ApplyRandlesFits(file_name = df['filepath'][0],
                 R0_guess = 1e-1,
                 R1_guess = 1e2,
                 Wo1_0_guess = 1e4,
                 Wo1_1_guess = 1e6,
                 C1_guess = 1e-7,
                 head_count = 100,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
#%%
# high impedance

ApplyRandlesFits(file_name = df['filepath'][0],
                 R0_guess = 1e-1,
                 R1_guess = 1e2,
                 Wo1_0_guess = 1e3,
                 Wo1_1_guess = 1e1,
                 C1_guess = 1e-1,
                 head_count = 250,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
# %%

#%% kind of high impedance
ApplyRandlesFits(file_name = df['filepath'][189],
                 R0_guess = 1e-1,
                 R1_guess = 1e2,
                 Wo1_0_guess = 1e3,
                 Wo1_1_guess = 1e3,
                 C1_guess = 1e-5,
                 head_count = 100,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
#%% kind of med impedance
ApplyRandlesFits(file_name = df['filepath'][190],
                 R0_guess = 1e-1,
                 R1_guess = 1e3,
                 Wo1_0_guess = 1e7,
                 Wo1_1_guess = 1e8,
                 C1_guess = 1e-5,
                 head_count = 90,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
# %%

# %%
# coin cells with med-low impedance
ApplyRandlesFits(file_name = df['filepath'][143],
                 R0_guess = 1e-1,
                 R1_guess = 1e5,
                 Wo1_0_guess = 1e9,
                 Wo1_1_guess = 1e7,
                 C1_guess = 1e-4,
                 head_count = 90,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
# %%
# coin cells with med-low impedance with curve
ApplyRandlesFits(file_name = df['filepath'][11],
                 R0_guess = 1e-1,
                 R1_guess = 1e3,
                 Wo1_0_guess = 1e3,
                 Wo1_1_guess = 1e3,
                 C1_guess = 1e-5,
                 head_count = 70,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
# %%
# coin cells with low impedance
ApplyRandlesFits(file_name = df['filepath'][2],
                 R0_guess = 1e-1,
                 R1_guess = 1e2,
                 Wo1_0_guess = 1e1,
                 Wo1_1_guess = 1e2,
                 C1_guess = 1e-5,
                 head_count = 55,
                # save_file = False,
                save_file = True,
                global_opt = False
                # global_opt = True
                )  
# %%

#%% no litfsi
collect_results = list(
        map(
            lambda x: ApplyRandlesFits(x, 
                                       R0_guess = 1e-1,
                                       R1_guess = 1e6, 
                                       Wo1_0_guess = 1e6, 
                                       Wo1_1_guess = 1e6, 
                                       C1_guess = 1e-4,
                                       head_count = 252,
                                       #save_file = False 
                                       save_file = True,
                                       global_opt = False), df3_nosalt['mpr_files']
            )
)

# %%

Z_test, f_test = ApplyFits(df3['mpr_files'][10])

H, w = signal.freqresp(ApplyFits(df3['mpr_files'][10]))

plt.figure()
plt.plot(H.real, -H.imag)
plt.show()

# %%
data = control.ss((Z_test))

# %%
