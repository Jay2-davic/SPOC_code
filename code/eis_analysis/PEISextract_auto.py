#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
#Subproject: SPOC

# loads the relevant EIS processing packages
from impedance import preprocessing as proc
from impedance import visualization as viz
from impedance.validation import linKK
from impedance import models
from impedance.models import circuits as mods
from galvani import BioLogic

# loads warning handler
import warnings
warnings.filterwarnings("ignore")

# loads file handling, data processing and visualization tools
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import io
import sys
from contextlib import redirect_stdout, contextmanager
import textwrap
from functools import partial
from scipy.optimize import minimize

# loads export tools
import csv

# loads math tools
import math

# suppress output calculations
pd.options.mode.chained_assignment = None  # default='warn'
@contextmanager
def suppress_stdout(suppress=True):
    if suppress:
        sys.stdout = open(os.devnull, 'w')
    try:
        yield
    finally:
        sys.stdout = sys.__stdout__

#### CODE ####

def objective_function(initial_guesses, frequencies_fit, Z_fit):
    R0, R1, Wo1_0, Wo1_1, C1 = initial_guesses
    
    # Create a Randles model with the current guesses
    randles = mods.Randles(initial_guess=[R0, R1, Wo1_0, Wo1_1, C1])
    
    # Fit the model to the data
    randles_fit = randles.fit(frequencies_fit, Z_fit, global_opt=False, method='trf')
    
    # Predict the impedance using the fitted model
    Z_randles = randles.predict(frequencies_fit)
    
    # Calculate RMSE for real and imaginary components
    rmse_real, rmse_imag, _, _ = calcValues(Z_fit, Z_randles)
    
    # Return the total RMSE
    return rmse_real + rmse_imag

def optimize_initial_guesses(frequencies_fit, Z_fit, initial_guesses):
    result = minimize(objective_function, initial_guesses, args=(frequencies_fit, Z_fit), method='Nelder-Mead')
    return result.x  # Return optimized guesses


# loads the mpr raw file conversion functions
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

# takes the EIS results, cleans the data then returns the formatted Z and freq values
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
    #print(csv_files)
    # converts into a np array with the equation Re - Im*j (complex function)
    Z = pd.array(
        csv_files['Re'] 
        - csv_files['Im']*1j)
    frequencies = pd.array(csv_files.freq)

    # cleans the data by removing values freq < 0
    frequencies, Z = proc.ignoreBelowX(
        frequencies, 
        Z)

    return(
        Z, 
        frequencies)

# loads the equations for calculations
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
    resid = np.array(np.log10(Z_actual) 
                     - np.log10(Z_predict))
    Z_real = resid.real/np.abs(np.log10(Z_actual))
    Z_imag = resid.imag/np.abs(np.log10(Z_actual))

    # calculates the mse and RMSE for the real component
    mse_real = np.square(Z_real).mean()
    rmse_real = math.sqrt(mse_real)

    # calculates the mse and RMSE for the imaginary component
    mse_imag = np.square(Z_imag).mean()
    rmse_imag = math.sqrt(mse_imag)

    return(
        rmse_real, rmse_imag, 
        Z_real, Z_imag)

# loads the cleaning functions for the EIS fitted outputs
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

# Runs the Lin-KK validation
def EIS_validate(file_name):
    '''
    Performs Lin-KK (Kronig-Kramer) method to validate the circuit

    Input:
        file_name = raw EIS files in .mpr format
    
    Output:
        df2 = dataframe of Lin-KK results
        M = # of equivalent RC circuits
        mu = measurment of over-fitting (closer to 0),
        Z_linKK = Z values of Lin-KK fit
        res_real = residual values of the real component of Z
        res_imag = residual values of the imag component of Z
    '''
    Z, frequencies = ApplyFits2(file_name)
    # convert the complex values from extended array to just numpy
    Z = Z.to_numpy()

    # applying linear Kramers-Kronig validity test to check for data quality
    M, mu, Z_linKK, res_real, res_imag = linKK(frequencies, Z, c=.5, max_M=100, fit_type='complex', add_cap=True)
    mu = round(mu, 2)
    df = pd.DataFrame()
    df['res_real'] = res_real
    df['res_imag'] = res_imag

    # calculates root mean squared error to determine validity for KK fitting
    kk_real = sum((df['res_real']**2)/len(df))**(1/2)
    kk_imag = sum((df['res_imag']**2)/len(df))**(1/2)
    #print(RMSE_real)

    # creates a dataframe to record results
    df2 = pd.DataFrame({'linKK_RMSEreal': [kk_real], 'linKK_RMSEimag': [kk_imag]})
    df2['linKK_passFail'] = np.where((df2['linKK_RMSEreal'] <= 0.2) & (df2['linKK_RMSEimag'] <= 0.2), True, False)
    #print(df2)

    # outputs RMSE from lin KK
    return(df2, M, mu, Z_linKK, res_real, res_imag)

# loads the fitting functions
def ApplyRandlesFits(
        file_name, #OUN,
        R0_guess, R1_guess, 
        Wo1_0_guess, Wo1_1_guess, 
        C1_guess, head_count, 
        save_file, global_opt):
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
    OUN = 'jimenez45'
    basename = os.path.basename(file_name)
    output_file_dir = fr'C:\\Users\\{OUN}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2408-Automate_results'
        
    # loads the Lin-KK method outputs
    with suppress_stdout(suppress=True):
        results_df, M, mu, Z_linKK, res_real, res_imag = EIS_validate(file_name)
        linkk_passfail = results_df['linKK_passFail'][0]
    # runs the function ApplyFits to load fitting parameters
    with suppress_stdout(suppress=True):
        Z, frequencies = ApplyFits2(file_name)

    # parse data based on how many data points as defined by user
    frequencies_fit = frequencies[:head_count]
    Z_fit = Z[:head_count]
    
    # runs the randles circuit model with initial guesses defined by user
    with suppress_stdout(suppress=True):
        initial_guesses = [R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess]
        # Optimize the initial guesses to minimize RMSE
    optimized_guesses = optimize_initial_guesses(frequencies_fit, Z_fit, initial_guesses)
    randles = mods.Randles(initial_guess=optimized_guesses)

    # applies randles fit and prints their fit parameters
    with suppress_stdout(suppress=True):
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
    with suppress_stdout(suppress=True):
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
    
    # plots the nyquist plot as a subplot if the save is true
    # if save_file == True:
    #     fig, ax1 = plt.subplots(2, 1, figsize = (10,10))
    #     ax1[0].nyquist_plot = randles_fit.plot(
    #         ax1[0],
    #         f_data = frequencies_fit, 
    #         Z_data = Z_fit, 
    #         kind = 'nyquist')
    #     ax1[0].legend(['Data', 'Fitted'], loc = 'upper right')
    #     ax1[0].set_title(f'Nyquist Plot', size = 20)

    #     # plots the residual plot next to the nyquist plot
    #     ax1[1].residual_plot = viz.plot_residuals(
    #         ax1[1], frequencies, 
    #         res_real, res_imag, 
    #         y_limits = (-20,20))
    #     ax1[1].set_title(fr'Lin-KK Residuals, $\mu$ = {mu}, # circuits = {M}', size = 20)
    #     plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}', 80)), size = 15)
    #     plt.tight_layout()
    if save_file == True:
        with suppress_stdout(suppress=True):
            # plots the bode plot
            #fig, ax2 = plt.subplots(figsize = (5,5))
            randles_fit.plot(
                #ax2,
                f_data = frequencies_fit, 
                Z_data = Z_fit, 
                kind = 'bode',
                units = '\Omega')
            #ax2.legend(['Data', 'Fitted'], loc = 'upper right')
            #ax2.set_title(f'Bode Plot', size = 20)
            plt.axis('auto')
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}', 80)), size = 10)
            plt.tight_layout()    

            file_namePlot = basename.replace(
                ' ',
                '_').replace(
                    '.csv',
                    '_BodePlot_randles.png')
            plt.savefig(output_file_dir + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close()
        # plots the linkk plot
        with suppress_stdout(suppress=True):
            fig, ax3 = plt.subplots(figsize = (5,5))
            ax3.residual_plot = viz.plot_residuals(
                ax3, frequencies, 
                res_real, res_imag, 
                y_limits = (-5,5))
            ax3.set_title(fr'Lin-KK Residuals, $\mu$ = {mu}, # circuits = {M}', size = 20)
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}', 80)), size = 15)
            plt.tight_layout()

            file_namePlot = basename.replace(
                ' ',
                '_').replace(
                    '.csv',
                    '_LinKKResiduals_randles.png')
            plt.savefig(output_file_dir + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close(fig)

        # plots the nyquist plot
        fig, ax1 = plt.subplots(figsize = (5,5))
        ax1.nyquist_plot = randles_fit.plot(
            ax1,
            f_data = frequencies_fit, 
            Z_data = Z_fit, 
            kind = 'nyquist',
            units = '\Omega')
        ax1.legend(['Data', 'Fitted'], loc = 'upper left')
        ax1.set_aspect('auto')
        #ax1.set_title(f'Nyquist Plot', size = 20)
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}, {rmse_imag}, {rmse_real}', 80)), size = 10)
        plt.tight_layout()

        file_namePlot = basename.replace(
            ' ',
            '_').replace(
                '.csv',
                '_randles.png')
        plt.savefig(output_file_dir + r'\\' + file_namePlot, dpi = 600, format = 'png')        
        
        print('Nyquist, Bode and LinKK plots saved!')
    elif save_file == False:
        # only plot the nyquist for quick visualization
        fig, ax = plt.subplots(figsize = (5,5), squeeze = True)
        ax.nyquist_plot = randles_fit.plot(ax,
                                           f_data = frequencies_fit, 
                                           Z_data = Z_fit, 
                                           kind = 'nyquist',
                                           units = '\Omega')
        ax.legend(['Data', 'Fitted'], loc = 'upper right')
        ax.set_title(f'Nyquist Plot (NOT SAVED)', size = 20)
        ax.set_aspect('auto')
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {file_name}, {rmse_imag}, {rmse_real}', 80)), size = 10)
        plt.tight_layout()
        
    # outputs randles fitting parameters
    print('RMSE Real = ', 
          rmse_real)
    print('RMSE Imaginary = ', 
          rmse_imag)
    print('Mu = ',
          mu)
    print('# circuits = ',
          M)
          
    # combines the head count and RMSE values as a dataframe
    stats_df = [[
        file_name, head_count, 
        res_real, res_imag, 
        mu, M, linkk_passfail,
        rmse_real, rmse_imag
        #bode_plot, #residual_plot
        ]]
    
    # adds column names to the dataframe
    stats_df_names = [
        'EIS_file', 'head_count', 
        'linKK_RMSEreal', 'linKK_RMSEimag',
        'mu', 'Circuits', 'Lin-KK_PassFail',
        'RMSE_real', 'RMSE_imag'
        #'bode_plot', #'residual_plot'
        ]
    stats_df = pd.DataFrame(
        stats_df, 
        columns = stats_df_names)

    cols = ['InitialGuess.R0',
              'InitialGuess.R1',
              'InitialGuess.Wo1_0',
              'InitialGuess.Wo1_1',
              'InitialGuess.C1',
              'FittedValue.R0',
              'FittedValue.R1',
              'FittedValue.Wo1_0',
              'FittedValue.Wo1_1',
              'FittedValue.C1',
              'FittedValue.R0_stDev',
              'FittedValue.R1_stDev',
              'FittedValue.Wo1_0_stDev',
              'FittedValue.Wo1_1_stDev',
              'FittedValue.C1_stDev']
    # saves the outputs as a single line dataframe
    output_df = EISresult_cleaner(randles_fit)
    output_df[cols] = output_df[cols].apply(pd.to_numeric, errors = 'coerce', axis = 1)
    output_df = pd.concat(
        [stats_df, output_df], 
        axis = 1)

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
    else:
        print('Output not saved')
       