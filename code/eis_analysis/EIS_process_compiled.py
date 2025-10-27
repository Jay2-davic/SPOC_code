#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
#Subproject: SPOC

#%%
# loads the relevant EIS processing packages
from impedance import preprocessing as proc
from impedance import visualization as viz
from impedance.validation import linKK
from impedance import models
from impedance.models import circuits as mods
from galvani import BioLogic
from impedance.models.circuits import Randles, CustomCircuit

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

# loads export tools
import csv

# loads math tools
import math

# loads GUI tools
import ipywidgets as widgets
from ipywidgets import interact, Output, VBox, HBox

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

# class of functions for cleaning the raw mpr files
class EIS_cleaning:

    def MPR_convert(EIS_file):
        '''
        Converts the EIS .mpr files (raw files) into a dataframe,
        then performs data cleaning of the column names.

        Input:
            EIS_file = EIS file with the format '.mpr'

        Output:
            converted and formated dataframe
        '''
        
        # handles warnings by removing them from output
        warnings.simplefilter(
            action='ignore', 
            category=FutureWarning)
        # using BioLogic to load .mpr files
        fileConversion = BioLogic.MPRfile(EIS_file)
        print(fileConversion)
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
        MPRtoDF = MPRtoDF.sort_values(by = 'time', ascending= True)

        # resets the indexing so that it does not follow the time sequence
        MPRtoDF = MPRtoDF.reset_index()
        
        return(MPRtoDF)

    def EIS_CycleLabels(EIS_file):
        '''
        Loads an EIS file (.mpr format) then labels each cycle
        based on the return to the initial frequency measurement.

        Input: 
            EIS_file = raw EIS file
        
        Output:
            dataframe with labels for each frequency cycle
        '''

        # try catch error in EIS dataframe conversion to return nothing
        try:
            # transforms raw EIS file to a dataframe
            df = EIS_cleaning.MPR_convert(EIS_file)
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

    def EIS_CycleSplitter(EIS_file, output_path):
        '''
        Saves the data points of each frequency cycle as a separate csv file.

        Input: 
            EIS_file = raw EIS data
            output_path = location for the csv files to be saved to
        
        Output: 
            individual csv files for each cycle of the mpr measurements
        '''
        # loads the EIS files and sorts the files by cycle
        df = EIS_cleaning.EIS_CycleLabels(EIS_file)
        
        # changes the file name and appends the original filename and replicate number 
        filename = os.path.basename(EIS_file).replace('.mpr', f'_RUN_')
        filename = fr'{output_path}\{filename}'

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
        
        return filename
    
        
    def EIS_conversion(data_dictionary, output_path):
        '''
        Wrapper function for EIS_CycleSplitter, uses the data dictionary to split 
        the EIS files within the dictionary into their constituent replicates.

        Inputs:
            data_dictionary = location of the data dictionary csv with the mpr file paths
            output_path = location for the csv files to be saved to

        Outputs:
            individual csv files for each cycle of the mpr measurements
        '''
        # loads the data dictionary from EIS cleaning
        database_csv = fr'{data_dictionary}'
        
        # reads the csv and drops empty values
        df = pd.read_csv(database_csv).dropna(subset = ['filepath'])
        # database inspection
        print(df['filepath'])

        # batch process all of the files in the folder
        list(
            map(lambda x: EIS_cleaning.EIS_CycleSplitter(x, output_path),
                df['filepath']
            )
        )
    
    def ApplyFits(EIS_file):
        '''
        Takes the EIS results, cleans the data then returns the formatted Z and freq values
        converts raw data Z values into an array of complex equations and 
        the frequencies as an array and cleans the data.

        Inputs:
            file_name = EIS file with the format '.mpr' or '.csv'
        
        Outputs: 
            converted Z and frequencies
        '''
        # runs the MPR opener function
        if ".mpr" in EIS_file:
            df = EIS_cleaning.MPR_convert(EIS_file)
        elif ".csv" in EIS_file:
            df = pd.read_csv(EIS_file)
        else:
            print('Error: Files must be .mpr or .csv type')

        # Rename columns to standardized names
        for col in df.columns:
            if 'Re' in col and 'Z' in col:
                df = df.rename(columns={col: 'Re'})
            elif 'Im' in col and 'Z' in col:
                df = df.rename(columns={col: 'Im'})
            elif col == '#NAME?':  # Handle Excel error that represents Im
                df = df.rename(columns={col: 'Im'})

        # Try to get Re column with different possible names
        try:
            re_col = df['Re(Z)']
        except KeyError:
            re_col = df['Re']

        # Try to get Im column with different possible names
        try:
            im_col = df['Im(Z)']
        except KeyError:
            im_col = df['Im']

        # converts into a np array with the equation Re - Im*j (complex function)
        Z = pd.array(re_col - im_col*1j)
        frequencies = pd.array(df.freq)

        # cleans the data by removing values freq < 0
        frequencies, Z = proc.ignoreBelowX(
            frequencies, 
            Z)

        return(
            Z, 
            frequencies)

    def printToDF(text_string):
        '''
        Converts the printed string output of the predicted values 
        into a dataframe.

        Input:
            text_string = output that prints a summary of the fits

        Output:
            dataframe of the results from the fits
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

    def EIS_InitialCleaner(df):
        '''
        Converts the printed result from impedancy.py into a dataframe
        of the initial guess inputs from EIS fitting.

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
        of the circuit model used for EIS fitting.

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

    def EISresult_cleaner(text_string):
        '''
        Cleans the output into a mergeable dataframe.

        Input:
            text_string = output that prints a summary of the fits

        Output:
            .csv-ready dataframe of the EIS metadata from fits
        '''
        # converts print output to a dataframe
        df = EIS_cleaning.printToDF(text_string)

        # defines the header information, initial and fitted values
        df_headerInfo = df.loc[0:2]
        df_initial = df.loc[5:9]
        df_fitted = df.loc[12:16]

        # performs the data cleaning and outputs a single row dataframe
        df_initial_ = EIS_cleaning.EIS_InitialCleaner(df_initial)
        df_fitted_ = EIS_cleaning.EIS_FittedCleaner(df_fitted)
        df_headerInfo_ = EIS_cleaning.EIS_HeaderCleaner(df_headerInfo)
            
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
    

# class of functions that output information extracted from the EIS data
class EIS_analysis:

    def calcValues(
            Z_actual, 
            Z_predict):
        '''
        Calculates the statistics of the model fitting to the impedance data.

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
    
    def EIS_validate(filename):
        '''
        Performs Lin-KK (Kronig-Kramer) method to validate the circuit.

        Input:
            filename = csv file from EIS_CycleSplitter
        
        Output:
            df2 = dataframe of Lin-KK results
            M = # of equivalent RC circuits
            mu = measurment of over-fitting (closer to 0),
            Z_linKK = Z values of Lin-KK fit
            res_real = residual values of the real component of Z
            res_imag = residual values of the imag component of Z
        '''
        # calculate the Z values as a complex equation array and a frequency array
        Z, frequencies = EIS_cleaning.ApplyFits(filename)

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

        # outputs RMSE from lin KK
        return(df2, M, mu, Z_linKK, res_real, res_imag)
    
    def get_headcount(file_name):
        '''
        Get the number of data points in the file
        
        Inputs:
            file_name = CSV file path
            
        Outputs:
            num_points = Number of data points available
        '''
        file = pd.read_csv(file_name)
        
        # Return the actual number of data rows (not including header)
        num_points = int(len(file) * .85)

        # if num_points <= 0:
        #     num_points = 25  # Ensure that it runs with a minimum of 25 points
        return num_points
    
    def load_file(file_name, headcount):
        '''
        Opens the csv file with data from the mpr file and calculates the Z and frequency values.

        Inputs:
            file_name = CSV file path
            headcount = Number of data points to use

        Outputs:
            frequencies = Full frequency array
            Z = Full impedance array
            frequencies_fit = Truncated frequency array for fitting
            Z_fit = Truncated impedance array for fitting
            x = Real component of impedance
            y = Imaginary component of impedance
        '''
        try:
            # Get Z and frequencies using the cleaning function
            Z, frequencies = EIS_cleaning.ApplyFits(file_name)
            
            # Read the CSV file directly for x and y values
            file = pd.read_csv(file_name)
            
            # Convert to numpy arrays if they're pandas arrays
            if hasattr(Z, 'to_numpy'):
                Z = Z.to_numpy()
            if hasattr(frequencies, 'to_numpy'):
                frequencies = frequencies.to_numpy()
            
            # Ensure headcount doesn't exceed available data
            actual_headcount = min(headcount, len(frequencies), len(Z), len(file))
            
            # Create truncated arrays for fitting
            frequencies_fit = frequencies[:actual_headcount]
            Z_fit = Z[:actual_headcount]

            # Try to get Re column with different possible names
            try:
                x = file['Re(Z)'][:actual_headcount]
            except KeyError:
                try:
                    x = file['Re'][:actual_headcount]
                except KeyError:
                    # If neither column exists, try to extract from Z
                    x = np.real(Z[:actual_headcount])
                    print(f"Warning: Could not find Re(Z) or Re column, using np.real(Z)")

            # Try to get Im column with different possible names
            try:
                y = file['Im(Z)'][:actual_headcount]
            except KeyError:
                try:
                    y = file['Im'][:actual_headcount]
                except KeyError:
                    # If neither column exists, try to extract from Z
                    y = np.imag(Z[:actual_headcount])
                    print(f"Warning: Could not find Im(Z) or Im column, using np.imag(Z)")
            
            # Convert to numpy arrays if they're pandas Series
            if hasattr(x, 'to_numpy'):
                x = x.to_numpy()
            if hasattr(y, 'to_numpy'):
                y = y.to_numpy()
            
            # Ensure all arrays have the same length
            min_length = min(len(frequencies), len(Z), len(x), len(y), actual_headcount)
            frequencies_fit = frequencies[:min_length]
            Z_fit = Z[:min_length]
            x = x[:min_length]
            y = y[:min_length]
            
            return frequencies, Z, frequencies_fit, Z_fit, x, y
            
        except Exception as e:
            print(f"Error in load_file: {e}")
            print(f"File name: {file_name}")
            print(f"Headcount: {headcount}")
            
            # Try to provide some debugging information
            try:
                file = pd.read_csv(file_name)
                print(f"Available columns: {file.columns.tolist()}")
                print(f"File shape: {file.shape}")
            except:
                print("Could not read CSV file for debugging")
            
            # Raise the error so we can see the full traceback
            raise

    def derivative_indicator(x, y):
        '''
        Uses the derivative of impedance data to identify the end of the semicircle.

        Inputs:
            x = Real component of impedance
            y = Imaginary component of impedance

        Outputs:
            R1_peak = the point with the rightmost and largest second derivative.
                The point furthest to the right is selected because the second derivative of the 
                linear warburg impedance line stays close to zero. This ensures the end of the 
                semicircle is selected rather than a point in the beginning of the semicircle.
        '''
        # first derivative
        dy_dx = np.gradient(-y, x)  

        # second derivative
        d2y_dx2 = np.gradient(dy_dx, x)

        # choose the 15 largest peaks of the second derivative
        deriv_ind = np.argsort(abs(d2y_dx2))[-15:]

        # choose the rightmost peak to avoid selecting a point in the beginning of the semicircle
        last_peak = np.argsort(deriv_ind)[-1]

        # get the index of the guess
        R1_peak = deriv_ind[last_peak]
    
        return(R1_peak)

    def calculate_guess(x, y, frequencies_fit, R1_peak, headcount):
        '''
        Uses the derivative and frequency values to calculate circuit element values.

        Inputs:
            x = Real component of impedance
            y = Imaginary component of impedance
            frequencies_fit = Frequency array
            R1_peak = Peak index from derivative analysis
            headcount = Number of data points to use

        Outputs:
            R0 = Initial guess for electrolyte resistance
            R1 = Initial guess for charge transfer resistance
            C1 = Initial guess for double layer capacitance
            Wo1_0 = Initial guess for Warburg coefficient 0
            Wo1_1 = Initial guess for Warburg coefficient 1
        '''
        # Calculate initial guesses
        if x[0] > 0:
            R0 = x[0]
        else: 
            R0 = 1e0 # defaulted value if negative

        if x[R1_peak] > 0:
            R1 = x[R1_peak]
        else:
            R1 = 1e3 # defaulted value if negative
        w_freq = frequencies_fit[0]
        C1 = 1/(2*math.pi*w_freq*(R1 - R0))
        
        # Fix: Use the last valid index instead of headcount
        last_index = min(headcount - 1, len(y) - 1, len(frequencies_fit) - 1)
        
        Wo1_0 = y[last_index]
        Wo1_1 = 45/math.sqrt(frequencies_fit[last_index])

        print(f"R0: {R0}\nR1: {R1}\nWo1_0: {Wo1_0}\nWo1_1: {Wo1_1}\nC1: {C1}")
        return(R0, R1, C1, Wo1_0, Wo1_1)

    def get_guesses(file_name, headcount):
        '''
        Wrapper for the circuit element calculation function.

        Inputs:
            file_name = CSV file path
            headcount = Number of data points to use for fitting

        Outputs:
            R0 = Initial guess for electrolyte resistance
            R1 = Initial guess for charge transfer resistance
            C1 = Initial guess for double layer capacitance
            Wo1_0 = Initial guess for Warburg coefficient 0
            Wo1_1 = Initial guess for Warburg coefficient 1
        '''
        try:
            frequencies, Z, frequencies_fit, Z_fit, x, y = EIS_analysis.load_file(file_name, headcount)
            
            R1_peak = EIS_analysis.derivative_indicator(x, y)
            
            # Calculate initial guesses
            R0, R1, C1, Wo1_0, Wo1_1 = EIS_analysis.calculate_guess(x, y, frequencies_fit, R1_peak, headcount)
            
            return R0, R1, C1, Wo1_0, Wo1_1
            
        except Exception as e:
            print(f"Error in get_guesses: {e}")
            print(f"File: {file_name}")
            raise


    def calculate_adaptive_rmse_target(frequencies_fit, Z_fit):
        """
        Calculate appropriate RMSE target based on data characteristics
        """
        # Data quality indicators
        freq_range = np.log10(np.max(frequencies_fit)) - np.log10(np.min(frequencies_fit))
        Z_magnitude_range = np.log10(np.max(np.abs(Z_fit))) - np.log10(np.min(np.abs(Z_fit)))
        data_points = len(Z_fit)
        
        # Signal-to-noise estimation
        Z_magnitude = np.abs(Z_fit)
        Z_phase = np.angle(Z_fit)
        
        # Estimate noise level from high-frequency impedance scatter
        if len(Z_fit) > 10:
            high_freq_indices = np.argsort(frequencies_fit)[-10:]  # Last 10 points
            high_freq_Z_var = np.std(Z_magnitude[high_freq_indices]) / np.mean(Z_magnitude[high_freq_indices])
        else:
            high_freq_Z_var = 0.1  # Default assumption
        
        # Base target RMSE calculation
        base_target = 0.08  # Starting point
        
        # Adjust based on frequency range (more decades = higher target)
        freq_adjustment = np.clip(freq_range / 6.0, 0.5, 2.0)  # 6 decades is typical
        
        # Adjust based on impedance range (larger range = higher target)  
        Z_range_adjustment = np.clip(Z_magnitude_range / 3.0, 0.5, 2.0)  # 3 decades is typical
        
        # Adjust based on estimated noise level
        noise_adjustment = np.clip(high_freq_Z_var * 10, 0.5, 3.0)
        
        # Adjust based on number of data points (more points = lower achievable RMSE)
        points_adjustment = np.clip(50.0 / data_points, 0.3, 2.0)
        
        # Calculate adaptive target
        adaptive_target = base_target * freq_adjustment * Z_range_adjustment * noise_adjustment * points_adjustment
        
        # Reasonable bounds
        adaptive_target = np.clip(adaptive_target, 0.02, 0.30)
        
        return adaptive_target

    # Modified version of calculate_frequency_weighted_rmse with phase emphasis
    def calculate_frequency_weighted_rmse(params, frequencies_fit, Z_fit):
        """
        Calculate frequency-weighted RMSE in log space for Randles circuit parameters
        WITH ENHANCED PHASE WEIGHTING FOR HIGH FREQUENCIES
        """
        R0, R1, Wo1_0, Wo1_1, C1 = params
        
        temp_randles = mods.Randles(initial_guess=params.tolist())
        
        with suppress_stdout(suppress=True):
            try:
                temp_fit = temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                Z_pred = temp_randles.predict(frequencies_fit)
            except:
                return 1e6
        
        # Calculate log-scale residuals (consistent with your calcValues function)
        log_residuals = np.log10(Z_fit) - np.log10(Z_pred)
        residuals_real = np.real(log_residuals) / np.abs(np.log10(Z_fit))
        residuals_imag = np.imag(log_residuals) / np.abs(np.log10(Z_fit))
        
        # Convert to numpy arrays to avoid pandas array issues
        frequencies_array = np.array(frequencies_fit)
        
        # MODIFIED: Enhanced frequency-dependent weighting for phase
        freq_weights = np.ones_like(frequencies_array)
        
        # Find characteristic frequency (at max phase)
        phase_data = np.angle(Z_fit, deg=True)
        max_phase_idx = np.argmin(phase_data)
        f_characteristic = frequencies_array[max_phase_idx]
        
        # Adjust weights based on frequency regions
        low_freq_mask = frequencies_array < 0.1
        mid_freq_mask = (frequencies_array >= 0.1) & (frequencies_array < f_characteristic * 100)
        high_freq_mask = (frequencies_array >= f_characteristic * 100) & (frequencies_array < 1e6)
        very_high_freq_mask = frequencies_array >= 1e6
        
        freq_weights[low_freq_mask] *= 0.5
        freq_weights[mid_freq_mask] *= 2.0
        freq_weights[high_freq_mask] *= 1.0
        freq_weights[very_high_freq_mask] *= 0.1  # De-emphasize very high frequencies
        
        # Calculate phase-specific RMSE
        phase_actual = np.angle(Z_fit, deg=True)
        phase_pred = np.angle(Z_pred, deg=True)
        
        # Handle phase wrapping
        phase_diff = phase_actual - phase_pred
        phase_diff = np.where(phase_diff > 180, phase_diff - 360, phase_diff)
        phase_diff = np.where(phase_diff < -180, phase_diff + 360, phase_diff)
        
        # Apply frequency weighting to phase errors
        weighted_phase_error = np.abs(phase_diff) * freq_weights / 90.0  # Normalize by 90 degrees
        phase_rmse = np.sqrt(np.mean(weighted_phase_error**2))
        
        # Weighted RMSE in log space
        weighted_rmse_real = np.sqrt(np.mean((residuals_real * freq_weights)**2))
        weighted_rmse_imag = np.sqrt(np.mean((residuals_imag * freq_weights)**2))
        
        # MODIFIED: Increase phase weight in combined metric
        return 0.3 * (weighted_rmse_real + weighted_rmse_imag) + 0.7 * phase_rmse


    # Modified version of evaluate_fit_quality with better phase R² calculation
    def evaluate_fit_quality(Z_fit, Z_pred, frequencies_fit=None, fitted_params=None):
        """
        Comprehensive fit quality evaluation with multiple metrics
        MODIFIED FOR BETTER PHASE EVALUATION
        
        Returns dictionary with various fit quality metrics
        """
        results = {}
        
        # 1. R-squared (Coefficient of Determination) - Your preferred option 2.A
        def r2_score(actual, predicted):
            ss_res = np.sum((actual - predicted) ** 2)
            ss_tot = np.sum((actual - np.mean(actual)) ** 2)
            return 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
        
        r2_real = r2_score(np.real(Z_fit), np.real(Z_pred))
        r2_imag = r2_score(np.imag(Z_fit), np.imag(Z_pred))
        r2_magnitude = r2_score(np.abs(Z_fit), np.abs(Z_pred))
        
        # MODIFIED: Enhanced phase R² calculation with frequency weighting
        if frequencies_fit is not None:
            frequencies_array = np.array(frequencies_fit)
            
            # Calculate phase with proper handling
            phase_actual = np.angle(Z_fit, deg=True)
            phase_pred = np.angle(Z_pred, deg=True)
            
            # Find characteristic frequency
            max_phase_idx = np.argmin(phase_actual)
            f_characteristic = frequencies_array[max_phase_idx]
            
            # Create frequency weights for phase R²
            phase_weights = np.ones_like(frequencies_array)
            
            # Weight mid-range frequencies more heavily
            for i, f in enumerate(frequencies_array):
                if f < f_characteristic * 0.01:
                    phase_weights[i] = 0.5
                elif f < f_characteristic * 100:
                    phase_weights[i] = 2.0
                elif f < 1e6:
                    phase_weights[i] = 1.0
                else:
                    phase_weights[i] = 0.1
            
            # Weighted phase R²
            weighted_phase_res = (phase_actual - phase_pred) * np.sqrt(phase_weights)
            weighted_phase_mean = np.average(phase_actual, weights=phase_weights)
            weighted_phase_tot = (phase_actual - weighted_phase_mean) * np.sqrt(phase_weights)
            
            ss_res_phase = np.sum(weighted_phase_res**2)
            ss_tot_phase = np.sum(weighted_phase_tot**2)
            r2_phase = 1 - (ss_res_phase / ss_tot_phase) if ss_tot_phase > 0 else 0
        else:
            r2_phase = r2_score(np.angle(Z_fit), np.angle(Z_pred))
        
        results.update({
            'r2_real': r2_real,
            'r2_imag': r2_imag, 
            'r2_magnitude': r2_magnitude,
            'r2_phase': r2_phase
        })
        
        # 2. Absolute component errors (ohms)
        real_error = np.mean(np.abs(np.real(Z_fit) - np.real(Z_pred)))
        imag_error = np.mean(np.abs(np.imag(Z_fit) - np.imag(Z_pred)))
        max_real_error = np.max(np.abs(np.real(Z_fit) - np.real(Z_pred)))
        max_imag_error = np.max(np.abs(np.imag(Z_fit) - np.imag(Z_pred)))
        
        results.update({
            'mean_real_error_ohm': real_error,
            'mean_imag_error_ohm': imag_error,
            'max_real_error_ohm': max_real_error,
            'max_imag_error_ohm': max_imag_error
        })
        
        # 3. Correlation metrics
        try:
            mag_corr = np.corrcoef(np.abs(Z_fit), np.abs(Z_pred))[0,1]
            phase_corr = np.corrcoef(np.angle(Z_fit), np.angle(Z_pred))[0,1]
            real_corr = np.corrcoef(np.real(Z_fit), np.real(Z_pred))[0,1]
            imag_corr = np.corrcoef(np.imag(Z_fit), np.imag(Z_pred))[0,1]
            
            results.update({
                'magnitude_correlation': mag_corr,
                'phase_correlation': phase_corr,
                'real_correlation': real_corr,
                'imag_correlation': imag_corr
            })
        except:
            results.update({
                'magnitude_correlation': 0,
                'phase_correlation': 0,
                'real_correlation': 0,
                'imag_correlation': 0
            })
        
        # 4. Physical parameter reasonableness (if provided)
        if fitted_params is not None:
            try:
                R0, R1, Wo1_0, Wo1_1, C1 = fitted_params
                
                param_score = 100  # Start with perfect score
                param_issues = []
                
                # Check resistances
                if R0 <= 0 or R1 <= 0:
                    param_issues.append("Negative resistance")
                    param_score -= 50
                
                if R0 > R1:
                    param_issues.append("R0 > R1")
                    param_score -= 20
                    
                if R0 > 1e6 or R1 > 1e9:
                    param_issues.append("Unreasonably high resistance")
                    param_score -= 30
                    
                # Check capacitance
                if C1 <= 0:
                    param_issues.append("Negative capacitance")
                    param_score -= 50
                elif C1 < 1e-12 or C1 > 1e-1:
                    param_issues.append("Unreasonable capacitance")
                    param_score -= 20
                
                # MODIFIED: Add Warburg parameter checks
                if Wo1_1 > 1000:
                    param_issues.append("Warburg time constant too high")
                    param_score -= 20
                
                results.update({
                    'parameter_score': max(0, param_score),
                    'parameter_issues': param_issues
                })
            except:
                results.update({
                    'parameter_score': 0,
                    'parameter_issues': ['Parameter evaluation failed']
                })
        
        # 5. Composite score based on primary metrics
        def calculate_composite_score():
            score = 0
            weights = 0
            
            # MODIFIED: Adjusted weights for phase emphasis
            # R² contribution (60% weight) - increased phase weight
            if 'r2_magnitude' in results and not np.isnan(results['r2_magnitude']):
                score += 20 * max(0, results['r2_magnitude'])
                weights += 20
            
            if 'r2_phase' in results and not np.isnan(results['r2_phase']):
                score += 60 * max(0, results['r2_phase'])
                weights += 60
            
            # Correlation contribution (20% weight)  
            if 'phase_correlation' in results and not np.isnan(results['phase_correlation']):
                score += 10 * max(0, results['phase_correlation'])
                weights += 10
            
            # Parameter reasonableness (20% weight)
            if 'parameter_score' in results:
                score += 10 * (results['parameter_score'] / 100)
                weights += 10
            
            return score / weights if weights > 0 else 0
        
        composite = calculate_composite_score()
        results['composite_score'] = composite
        
        return results
        
    # Modified version of robust_randles_fit with phase-specific improvements
    def robust_randles_fit(randles_model, frequencies_fit, Z_fit, n_attempts=500, metric_type='phase', min_r2_threshold=0.989):
        """
        Monte Carlo sweep with selectable fit quality metrics and R² threshold
        MODIFIED WITH ENHANCED PHASE FITTING
        
        metric_type options:
        - 'phase': Original phase-based RMSE targeting
        - 'r2': R-squared based targeting (your preferred 2.A)
        - 'composite': Composite score targeting
        - 'absolute': Absolute error targeting
        """
        best_fit = None
        best_rmse = np.inf
        best_Z_pred = None
        rmse_history = []
        min_attempts = 5
        convergence_threshold = 0.001
        
        # Track R² values
        r2_history = []
        best_r2 = -np.inf
        
        # Calculate phase-based RMSE target
        def calculate_phase_target():
            Z_phase = np.angle(Z_fit, deg=True)

            # Find maximum phase angle (most negative)
            max_phase = abs(np.min(Z_phase))

            if max_phase < 20:      # Poor phase response
                phase_target = 0.15
            elif max_phase < 40:    # Fair phase response  
                phase_target = 0.12
            elif max_phase < 60:    # Good phase response
                phase_target = 0.08
            elif max_phase < 80:    # Very good phase response
                phase_target = 0.05
            else:                   # Excellent phase response
                phase_target = 0.03
            
            return phase_target
        
        # Get initial parameter guesses
        try:
            initial_params = randles_model.initial_guess
        except:
            initial_params = [10, 100, 0.01, 0.01, 1e-6]
        
        # Convert initial guesses to log10 space
        log_initial = [np.log10(max(p, 1e-12)) for p in initial_params]
        
        def sample_around_initial(iteration_round=1):
            """
            Sample parameters around initial guesses in log10 space
            MODIFIED WITH TIGHTER SAMPLING FOR WARBURG PARAMETERS
            """
            # Adjust sweep ranges based on iteration round
            if iteration_round == 1:
                sweep_ranges = [4, 4, 3, 2, 4]  # Tighter range for Wo1_1
            else:
                sweep_ranges = [2, 2, 2, 1, 2]  # Even tighter for rerun
            
            sampled_log_params = []
            for i, (log_center, sweep_range) in enumerate(zip(log_initial, sweep_ranges)):
                # Sample within ±sweep_range decades of the initial guess
                log_param = np.random.uniform(log_center - sweep_range, 
                                            log_center + sweep_range)
                sampled_log_params.append(log_param)
            
            # Convert back to linear space
            linear_params = [10**log_p for log_p in sampled_log_params]
            
            return linear_params, sampled_log_params
        
        def evaluate_attempt(Z_fit, temp_Z_pred, sampled_params):
            """
            Evaluate fit quality based on selected metric
            """
            # Always calculate R² regardless of metric type
            fit_quality = EIS_analysis.evaluate_fit_quality(Z_fit, temp_Z_pred, frequencies_fit, sampled_params)
            current_r2 = fit_quality['r2_magnitude']
            
            if metric_type == 'phase':
                # Use phase-weighted RMSE
                rmse_real, rmse_imag, _, _ = EIS_analysis.calcValues(Z_fit, temp_Z_pred)
                
                # Calculate phase RMSE with frequency weighting
                frequencies_array = np.array(frequencies_fit)
                phase_actual = np.angle(Z_fit, deg=True)
                phase_pred = np.angle(temp_Z_pred, deg=True)
                
                # Phase difference
                phase_diff = phase_actual - phase_pred
                phase_diff = np.where(phase_diff > 180, phase_diff - 360, phase_diff)
                phase_diff = np.where(phase_diff < -180, phase_diff + 360, phase_diff)
                
                # Weight by frequency
                freq_weights = np.ones_like(frequencies_array)
                high_freq_mask = frequencies_array > 1e5
                freq_weights[high_freq_mask] *= 0.1
                
                weighted_phase_rmse = np.sqrt(np.mean((phase_diff * freq_weights)**2))
                
                return rmse_real + rmse_imag + 0.1 * weighted_phase_rmse, 'lower_better', current_r2
            
            elif metric_type == 'r2':
                # R-squared evaluation with phase consideration
                r2_phase = fit_quality.get('r2_phase', 0)
                combined_r2 = 0.5 * current_r2 + 0.5 * r2_phase
                return combined_r2, 'higher_better', current_r2
            
            elif metric_type == 'composite':
                # Composite score evaluation
                return fit_quality['composite_score'], 'higher_better', current_r2
            
            elif metric_type == 'absolute':
                # Absolute error evaluation
                # Use max error (worst case)
                max_error = max(fit_quality['max_real_error_ohm'], fit_quality['max_imag_error_ohm'])
                return max_error, 'lower_better', current_r2
            
            else:
                # Default to RMSE
                rmse_real, rmse_imag, _, _ = EIS_analysis.calcValues(Z_fit, temp_Z_pred)
                return rmse_real + rmse_imag, 'lower_better', current_r2
        
        # Set targets based on metric type
        if metric_type == 'phase':
            target_value = calculate_phase_target()
            comparison_better = lambda current, target: current <= target
        elif metric_type == 'r2':
            # Update target to user's minimum threshold
            target_value = min_r2_threshold
            comparison_better = lambda current, target: current >= target
        elif metric_type == 'composite':
            target_value = 85  # 85/100 composite score target
            comparison_better = lambda current, target: current >= target
        elif metric_type == 'absolute':
            target_value = 1000  # Max 1000Ω error target
            comparison_better = lambda current, target: current <= target
        
        # Main fitting loop with automatic rerun
        def run_fitting_attempts(round_num=1, max_attempts=None):
            nonlocal best_fit, best_rmse, best_Z_pred, rmse_history, best_r2, r2_history
            
            attempts_to_use = max_attempts if max_attempts else n_attempts
            best_metric_value = np.inf if metric_type in ['phase', 'absolute'] else -np.inf
            
            for attempt in range(attempts_to_use):
                try:
                    if attempt == 0 and round_num == 1:
                        # First attempt of first round: use exact initial guesses
                        sampled_params = initial_params
                    else:
                        # Subsequent attempts: sample around initial guesses
                        sampled_params, log_params = sample_around_initial(round_num)
                    
                    # MODIFIED: Additional parameter constraints
                    R0, R1, Wo1_0, Wo1_1, C1 = sampled_params
                    if R0 > R1 or Wo1_1 > 100:  # Skip unrealistic parameters
                        continue
                    
                    # Create randles model with sampled parameters
                    temp_randles = mods.Randles(initial_guess=sampled_params)
                    
                    with suppress_stdout(suppress=True):
                        temp_fit = temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                        temp_Z_pred = temp_randles.predict(frequencies_fit)
                    
                    # Evaluate based on selected metric
                    metric_value, direction, current_r2 = evaluate_attempt(Z_fit, temp_Z_pred, sampled_params)
                    r2_history.append(current_r2)
                    
                    # Calculate RMSE for tracking (always)
                    rmse_real, rmse_imag, _, _ = EIS_analysis.calcValues(Z_fit, temp_Z_pred)
                    total_rmse = rmse_real + rmse_imag
                    rmse_history.append(total_rmse)
                    
                    # Check if this meets R² threshold AND is better on the selected metric
                    r2_meets_threshold = current_r2 >= min_r2_threshold
                    
                    # Check if this is the best result
                    is_better = False
                    if direction == 'lower_better':
                        if metric_value < best_metric_value and r2_meets_threshold:
                            is_better = True
                            best_metric_value = metric_value
                    else:  # higher_better
                        if metric_value > best_metric_value and r2_meets_threshold:
                            is_better = True
                            best_metric_value = metric_value
                    
                    # Also track best R² regardless of threshold
                    if current_r2 > best_r2:
                        best_r2 = current_r2
                    
                    if is_better:
                        best_rmse = total_rmse
                        best_fit = temp_fit
                        best_Z_pred = temp_Z_pred
                    
                    # Check if we've reached both targets
                    if comparison_better(best_metric_value, target_value) and r2_meets_threshold:
                        return True  # Success
                            
                except Exception as e:
                    rmse_history.append(np.inf)
                    r2_history.append(0)
                    continue
                
                # Early stopping after minimum attempts
                if attempt >= min_attempts - 1 and len(rmse_history) > min_attempts:
                    recent_best = min(rmse_history[-min_attempts:])
                    previous_best = min(rmse_history[:-min_attempts])
                    
                    if previous_best > 0:
                        improvement = (previous_best - recent_best) / previous_best
                        
                        if improvement < convergence_threshold:
                            break
            
            return False  # Did not reach target
        
        # Run first round of fitting
        success = run_fitting_attempts(round_num=1)
        
        # Automatic rerun logic based on metric type and R² threshold
        if not success:
            # Check if we need to rerun based on R² threshold
            if best_r2 < min_r2_threshold:
                success = run_fitting_attempts(round_num=2, max_attempts=50)
                
                # Third round if still below threshold
                if not success and best_r2 < min_r2_threshold:
                    success = run_fitting_attempts(round_num=3, max_attempts=100)
        
        # Check if any successful fit was found
        if best_fit is None or best_Z_pred is None:
            # Return None values to indicate failure
            return None, None, np.inf
        
        return best_fit, best_Z_pred, best_rmse

    def optimize_randles_parameters(frequencies_fit, Z_fit, R0_guess, R1_guess, 
                                Wo1_0_guess, Wo1_1_guess, C1_guess, objective_type='rmse'):
        """
        Optimize Randles circuit parameters with selectable objectives
        
        objective_type options:
        - 'rmse': Original frequency-weighted RMSE (default)
        - 'r2': Target maximum R-squared
        - 'hybrid': Combination of R² and parameter reasonableness
        """
        
        def calculate_r2_objective(log_params):
            """
            Calculate negative R² (since minimize() finds minimum, we want maximum R²)
            """
            try:
                params = 10**log_params
                temp_randles = mods.Randles(initial_guess=params.tolist())
                
                with suppress_stdout(suppress=True):
                    temp_fit = temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                    Z_pred = temp_randles.predict(frequencies_fit)
                
                # Calculate R² for magnitude
                ss_res = np.sum((np.abs(Z_fit) - np.abs(Z_pred)) ** 2)
                ss_tot = np.sum((np.abs(Z_fit) - np.mean(np.abs(Z_fit))) ** 2)
                r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
                
                return -r2  # Negative because we want to maximize R²
                
            except:
                return 1e6  # Large penalty for failed fits
        
        def calculate_hybrid_objective(log_params):
            """
            Combine R² maximization with parameter reasonableness
            """
            try:
                params = 10**log_params
                R0, R1, Wo1_0, Wo1_1, C1 = params
                
                temp_randles = mods.Randles(initial_guess=params.tolist())
                
                with suppress_stdout(suppress=True):
                    temp_fit = temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                    Z_pred = temp_randles.predict(frequencies_fit)
                
                # Calculate R²
                ss_res = np.sum((np.abs(Z_fit) - np.abs(Z_pred)) ** 2)
                ss_tot = np.sum((np.abs(Z_fit) - np.mean(np.abs(Z_fit))) ** 2)
                r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
                
                # Parameter penalty (lighter than grading system)
                param_penalty = 0
                
                # Gentle penalties for unreasonable parameters
                if R0 <= 0 or R1 <= 0: param_penalty += 0.1
                if C1 <= 0: param_penalty += 0.1
                if R0 > R1: param_penalty += 0.05  # Gentle penalty
                
                # Objective: maximize R² while gently penalizing bad parameters
                objective = -r2 + param_penalty
                
                return objective
                
            except:
                return 1e6
        
        # Select objective function
        if objective_type == 'r2':
            objective_function = calculate_r2_objective
        elif objective_type == 'hybrid':
            objective_function = calculate_hybrid_objective
        else:  # default rmse
            objective_function = lambda log_params: EIS_analysis.calculate_frequency_weighted_rmse(10**log_params, frequencies_fit, Z_fit)
        
        # Initial guess in log space
        initial_log_params = np.log10([R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess])
        log_bounds = EIS_analysis.get_parameter_bounds()
        
        # Try multiple optimization methods for R² (it can be more challenging)
        methods = ['L-BFGS-B', 'TNC', 'SLSQP'] if objective_type in ['r2', 'hybrid'] else ['L-BFGS-B']
        
        best_result = None
        best_objective = np.inf
        
        for method in methods:
            try:
                result = minimize(
                    objective_function,
                    initial_log_params,
                    bounds=log_bounds,
                    method=method,
                    options={'maxiter': 1000, 'ftol': 1e-12}
                )
                
                if result.success and result.fun < best_objective:
                    best_result = result
                    best_objective = result.fun
                        
            except Exception as e:
                continue
        
        if best_result is not None and best_result.success:
            optimal_params = 10**best_result.x
            return optimal_params, True
        else:
            return np.array([R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess]), False

    def ApplyRandlesFits_Calculate(
    file_name, R0_guess, R1_guess, 
    Wo1_0_guess, Wo1_1_guess, 
    C1_guess, head_count, global_opt, 
    fit_metric='phase', optimization_target='phase_weighted',
    min_r2_threshold=0.995):
        '''
        Pure calculation function with enhanced phase optimization
        
        fit_metric options:
        - 'phase': Phase-weighted R² targeting (recommended for Bode phase fits)
        - 'r2': R-squared based targeting
        - 'composite': Composite score targeting
        - 'absolute': Absolute error targeting
        
        optimization_target options:
        - 'phase_weighted': Optimize for best phase fit (NEW - recommended)
        - 'rmse': Optimize initial parameters for minimum RMSE
        - 'r2': Optimize initial parameters for maximum R²
        - 'hybrid': Optimize for R² with parameter constraints
        
        min_r2_threshold: Minimum R² value required (default 0.989 = 98.9%)
        '''
        # extracts the basename of the file
        basename = os.path.basename(file_name)
        # loads the Lin-KK method outputs
        with suppress_stdout(suppress=True):
            results_df, M, mu, Z_linKK, res_real, res_imag = EIS_analysis.EIS_validate(file_name)
            linkk_passfail = results_df['linKK_passFail'][0]

        frequencies, Z, frequencies_fit, Z_fit, x, y = EIS_analysis.load_file(file_name, head_count)
        
        # Optimize parameters using selected objective (now with phase_weighted option)
        optimal_params, optimization_success = EIS_analysis.optimize_randles_parameters(
            frequencies_fit, Z_fit, R0_guess, R1_guess, 
            Wo1_0_guess, Wo1_1_guess, C1_guess,
            objective_type=optimization_target
        )
        
        # Create randles model with optimized parameters
        randles = mods.Randles(initial_guess=optimal_params.tolist())
        
        # ROBUST FITTING WITH SELECTED METRIC AND R² THRESHOLD
        randles_fit, Z_randles, robust_rmse = EIS_analysis.robust_randles_fit(
            randles, frequencies_fit, Z_fit, 
            n_attempts=200,  # Increased attempts for better phase fitting
            metric_type=fit_metric,
            min_r2_threshold=min_r2_threshold
        )
        
        # Check if fitting failed completely
        if randles_fit is None or Z_randles is None:
            # EFFICIENT FALLBACK: Try just a few key thresholds
            fallback_thresholds = [0.98, 0.95, 0.90, 0.80, 0.70, 0.50, 0.0]
            
            for threshold in fallback_thresholds:
                if threshold < min_r2_threshold:
                    # Fewer attempts for faster execution
                    n_attempts = 50 if threshold > 0.8 else 30
                    
                    randles_fit, Z_randles, robust_rmse = EIS_analysis.robust_randles_fit(
                        randles, frequencies_fit, Z_fit, 
                        n_attempts=n_attempts,
                        metric_type=fit_metric,
                        min_r2_threshold=threshold
                    )
                    
                    if randles_fit is not None and Z_randles is not None:
                        min_r2_threshold = threshold
                        break
            
            # If still failing, try ONE direct fit attempt
            if randles_fit is None or Z_randles is None:
                
                try:
                    # Use the optimized parameters for direct fit
                    direct_randles = mods.Randles(initial_guess=optimal_params.tolist())
                    randles_fit = direct_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                    Z_randles = direct_randles.predict(frequencies_fit)
                    
                    if randles_fit is not None:
                        # Calculate the actual R² achieved
                        fit_quality = EIS_analysis.evaluate_fit_quality(Z_fit, Z_randles, frequencies_fit, optimal_params)
                        actual_r2 = fit_quality.get('r2_magnitude', 0)
                        min_r2_threshold = actual_r2  # Update to reflect actual performance
                        robust_rmse = np.inf  # Mark as direct fit
                        
                except Exception as e:
                    print(f"Direct fit failed: {e}")
                    
            # If STILL failing, try with simplified generic parameters
            if randles_fit is None or Z_randles is None:
                # Trying with generic parameters
                generic_params = [10, 10000, 0.01, 1.0, 1e-6]
                
                try:
                    generic_randles = mods.Randles(initial_guess=generic_params)
                    
                    # Try direct fit first (faster):
                    randles_fit = generic_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                    Z_randles = generic_randles.predict(frequencies_fit)
                    
                    if randles_fit is not None:
                        print("✓ Generic parameters succeeded")
                        optimal_params = np.array(generic_params)
                        min_r2_threshold = 0.0
                        robust_rmse = np.inf
                        
                except:
                    pass
            
            # Final check
            if randles_fit is None or Z_randles is None:
                # Return a failure result
                return {
                    'basename': basename,
                    'randles_fit': None,
                    'Z_randles': None,
                    'frequencies_fit': frequencies_fit,
                    'Z_fit': Z_fit,
                    'frequencies': frequencies,
                    'res_real': res_real,
                    'res_imag': res_imag,
                    'rmse_real': np.inf,
                    'rmse_imag': np.inf,
                    'mu': mu,
                    'M': M,
                    'linkk_passfail': linkk_passfail,
                    'output_df': None,
                    'file_name': file_name,
                    'head_count': head_count,
                    'optimization_success': False,
                    'robust_rmse': np.inf,
                    'final_quality': {'r2_magnitude': 0, 'r2_phase': 0},
                    'r2_threshold_met': False,
                    'min_r2_threshold': min_r2_threshold,
                    'fitting_failed': True
                }
        
        # Calculate final statistics using the best fit result
        rmse_real, rmse_imag, Z_real, Z_imag = EIS_analysis.calcValues(Z_fit, Z_randles)
        
        # Calculate comprehensive fit quality with phase metrics
        final_quality = EIS_analysis.evaluate_fit_quality(Z_fit, Z_randles, frequencies_fit, optimal_params)
        
        # Check if R² threshold was met (using combined R² for phase-weighted fits)
        if fit_metric == 'phase' or optimization_target == 'phase_weighted':
            r2_to_check = final_quality.get('r2_combined', final_quality['r2_magnitude'])
        else:
            r2_to_check = final_quality['r2_magnitude']
        
        r2_threshold_met = r2_to_check >= min_r2_threshold
        
        # Create output dataframe
        stats_df = [[
            file_name, head_count, 
            res_real, res_imag, 
            mu, M, linkk_passfail,
            rmse_real, rmse_imag
            ]]
        
        stats_df_names = [
            'EIS_file', 'head_count', 
            'linKK_RMSEreal', 'linKK_RMSEimag',
            'mu', 'Circuits', 'Lin-KK_PassFail',
            'RMSE_real', 'RMSE_imag'
            ]
        stats_df = pd.DataFrame(stats_df, columns = stats_df_names)

        cols = ['InitialGuess.R0', 'InitialGuess.R1', 'InitialGuess.Wo1_0',
                'InitialGuess.Wo1_1', 'InitialGuess.C1', 'FittedValue.R0',
                'FittedValue.R1', 'FittedValue.Wo1_0', 'FittedValue.Wo1_1',
                'FittedValue.C1', 'FittedValue.R0_stDev', 'FittedValue.R1_stDev',
                'FittedValue.Wo1_0_stDev', 'FittedValue.Wo1_1_stDev', 'FittedValue.C1_stDev']
        
        output_df = EIS_cleaning.EISresult_cleaner(randles_fit)
        output_df[cols] = output_df[cols].apply(pd.to_numeric, errors = 'coerce', axis = 1)
        output_df = pd.concat([stats_df, output_df], axis = 1)
        
        # Add optimization and quality metrics to output
        output_df['optimization_success'] = optimization_success
        output_df['optimization_target'] = optimization_target
        output_df['fit_metric'] = fit_metric
        output_df['robust_rmse'] = robust_rmse
        output_df['r2_magnitude'] = final_quality['r2_magnitude'] 
        output_df['r2_phase'] = final_quality.get('r2_phase', 0)
        output_df['r2_combined'] = final_quality.get('r2_combined', final_quality['r2_magnitude'])
        output_df['composite_score'] = final_quality['composite_score']
        
        # Track fallback usage
        output_df['actual_r2_threshold'] = min_r2_threshold
        output_df['used_fallback'] = (min_r2_threshold < 0.995) or (robust_rmse == np.inf)
        output_df['fallback_type'] = 'none'
        if min_r2_threshold < 0.995:
            output_df['fallback_type'] = 'reduced_threshold'
        if robust_rmse == np.inf:
            output_df['fallback_type'] = 'direct_fit'
        
        return {
            'basename': basename, 'randles_fit': randles_fit, 'Z_randles': Z_randles,
            'frequencies_fit': frequencies_fit, 'Z_fit': Z_fit, 'frequencies': frequencies,
            'res_real': res_real, 'res_imag': res_imag, 'rmse_real': rmse_real,
            'rmse_imag': rmse_imag, 'mu': mu, 'M': M, 'linkk_passfail': linkk_passfail,
            'output_df': output_df, 'file_name': file_name, 'head_count': head_count,
            'optimization_success': optimization_success, 'robust_rmse': robust_rmse,
            'final_quality': final_quality, 'r2_threshold_met': r2_threshold_met,
            'min_r2_threshold': min_r2_threshold,
            'fitting_failed': False
        }

    def ApplyRandlesFits_Plot(file_name, calc_results, output_path, save_file):
        '''
        Pure plotting/saving function with enhanced phase metrics display
        '''
        
        # Extract all variables from calc_results
        basename = os.path.basename(file_name)
        randles_fit = calc_results['randles_fit']
        frequencies_fit = calc_results['frequencies_fit']
        Z_fit = calc_results['Z_fit']
        frequencies = calc_results['frequencies']
        res_real = calc_results['res_real']
        res_imag = calc_results['res_imag']
        rmse_real = calc_results['rmse_real']
        rmse_imag = calc_results['rmse_imag']
        mu = calc_results['mu']
        M = calc_results['M']
        output_df = calc_results['output_df']
        file_name = calc_results['file_name']
        robust_rmse = calc_results['robust_rmse']
        
        # Get quality metrics including phase R²
        final_quality = calc_results.get('final_quality', {})
        r2_mag = final_quality.get('r2_magnitude', 0)
        r2_phase = final_quality.get('r2_phase', 0)
        r2_combined = final_quality.get('r2_combined', r2_mag)
        r2_threshold_met = calc_results.get('r2_threshold_met', False)
        min_r2_threshold = calc_results.get('min_r2_threshold', 0.989)
        
        # Add threshold status to plots
        threshold_status = "✓ PASS" if r2_threshold_met else "✗ FAIL"
        threshold_text = f"R² Threshold ({min_r2_threshold:.1%}): {threshold_status}"        
        
        if save_file == True:
            # plots the bode plot with RMSE and phase R²
            fig, ax2 = plt.subplots(figsize = (5,5))
            randles_fit.plot(
                f_data = frequencies_fit, 
                Z_data = Z_fit, 
                kind = 'bode',
                units = '\Omega')
            ax2.legend(['Data', 'Fitted'], loc = 'upper right')
            ax2.set_title(f'Bode Plot', size = 11)
            plt.axis('auto')
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size = 8)
            plt.tight_layout()   

            file_namePlot = basename.replace(' ', '_').replace('.csv', '_BodePlot_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close(fig)

            # plots the linkk plot with RMSE and phase R²
        
            fig, ax3 = plt.subplots(figsize = (5,5))
            ax3.residual_plot = viz.plot_residuals(
                ax3, frequencies, 
                res_real, res_imag, 
                y_limits = (-5,5))
            ax3.set_title(fr'Lin-KK Residuals, $\mu$ = {mu}, # circuits = {M}', size = 10)
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size = 8)
            plt.tight_layout()

            file_namePlot = basename.replace(' ', '_').replace('.csv', '_LinKKResiduals_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close(fig)

            # plots the nyquist plot with phase metrics
            fig, ax1 = plt.subplots(figsize = (5,5))
            ax1.nyquist_plot = randles_fit.plot(
                ax1,
                f_data = frequencies_fit, 
                Z_data = Z_fit, 
                kind = 'nyquist',
                units = '\Omega')
            ax1.legend(['Data', 'Fitted'], loc = 'upper left')
            ax1.set_aspect('auto')
            ax1.set_title(f'Nyquist Plot', size = 10)
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size = 8)
            plt.tight_layout()

            file_namePlot = basename.replace(' ', '_').replace('.csv', 'NyquistPlot_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')   
            plt.close(fig)     
            
            # Save CSV
            file_nameCSV = basename.replace(' ', '_').replace('.csv', '__randles.csv')
            output_df.to_csv(output_path + r'\\' + file_nameCSV, index = False)

            if not r2_threshold_met:
                print(f'⚠️ WARNING: R² threshold not met! ({r2_combined:.3f} < {min_r2_threshold:.3f})')
            
        elif save_file == False:
            # only plot the nyquist for quick visualization with phase metrics
            fig, ax = plt.subplots(figsize = (5,5), squeeze = True)
            ax.nyquist_plot = randles_fit.plot(ax,
                                            f_data = frequencies_fit, 
                                            Z_data = Z_fit, 
                                            kind = 'nyquist',
                                            units = '\Omega')
            ax.legend(['Data', 'Fitted'], loc = 'upper right')
            ax.set_title(f'Nyquist Plot (NOT SAVED)\nRMSE: Real={rmse_real:.4f}, Imag={rmse_imag:.4f}\nR²: Mag={r2_mag:.3f}, Phase={r2_phase:.3f}, Combined={r2_combined:.3f}, {threshold_text}', size = 10)
            ax.set_aspect('auto')
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size = 10)
            plt.tight_layout()
            if not r2_threshold_met:
                print(f'⚠️ WARNING: R² threshold not met! ({r2_combined:.3f} < {min_r2_threshold:.3f})')    
        
    def get_parameter_bounds():
        """
        Define log-space parameter bounds for Randles circuit elements
        """
        log_bounds = [
            (-3, 6),    # R0: 1e-3 to 1e6 Ω
            (2, 8),    # R1: 1e2 to 1e8 Ω  
            (-8, 2),    # Wo1_0: 1e-8 to 1e2
            (-8, 2),    # Wo1_1: 1e-8 to 1e2
            (-12, -1)   # C1: 1e-12 to 1e-1 F
        ]
        return log_bounds

    def ApplyRandlesFits(
        file_name, output_path,
        R0_guess, R1_guess, 
        Wo1_0_guess, Wo1_1_guess, 
        C1_guess, head_count, 
        save_file, global_opt):
        '''
        Wrapper function that combines calculation and plotting/saving.
        Now uses phase-optimized fitting by default for better Bode plot phase fits.
        '''
        
        # Run calculations with phase optimization
        calc_results = EIS_analysis.ApplyRandlesFits_Calculate(
            file_name, R0_guess, R1_guess, 
            Wo1_0_guess, Wo1_1_guess, 
            C1_guess, head_count, global_opt, 
            fit_metric='phase',                    # Use phase-weighted R² metric
            optimization_target='phase_weighted',   # Optimize for phase fitting
            min_r2_threshold=0.989)
        
        # Run plotting/saving
        EIS_analysis.ApplyRandlesFits_Plot(file_name, calc_results, output_path, save_file)
        
        # Return the output dataframe (if needed elsewhere)
        return calc_results['output_df']

    def apply_randles_fits_wrapper(file_name, output, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess, head_count, save_file, global_opt):
        '''
        Wraps the function to apply the impedance data to a Randles circuit model with impedance.py.
        '''
        
        head_count = int(head_count)
        EIS_analysis.ApplyRandlesFits(file_name, 
                                    output_path = output, 
                                    R0_guess=R0_guess, 
                                    R1_guess=R1_guess, 
                                    Wo1_0_guess=Wo1_0_guess, 
                                    Wo1_1_guess=Wo1_1_guess, 
                                    C1_guess=C1_guess, 
                                    head_count=head_count, 
                                    save_file=save_file, 
                                    global_opt = global_opt)
    
    def run_EIS_analysis(filename, save_path):
        '''
        Wraps all functions needed to process raw EIS files

        Input:
            filename = path to raw EIS file, can be ".mpr" or ".csv"
            save_path = path to folder for analysis outputs

        Output:
            CSV file with results
            Bode plot fits
            LinKK residual plot fits
            Nyquist plot fits
        '''
        headcount = EIS_analysis.get_headcount(filename) 
        R0, R1, C1, Wo1_0, Wo1_1 = EIS_analysis.get_guesses(filename, headcount)
        EIS_analysis.apply_randles_fits_wrapper(
                filename, save_path,
                R0, R1, 
                Wo1_0, 
                Wo1_1, C1, headcount, 
                True, False)


#%%
dictionary = fr"C:\Users\cooper94\OneDrive - LLNL\High-Throughput Polymer Electrolytes DIW - General 1\Database of polymer electrolytes\08052025_9_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI\Processing\dictionary.csv"

#fr"C:\Users\cooper94\OneDrive - LLNL\High-Throughput Polymer Electrolytes DIW - General\Database of polymer electrolytes\01092024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI\MPR Processing\data_dictionary.csv"
save_path = fr"C:\Users\cooper94\OneDrive - LLNL\High-Throughput Polymer Electrolytes DIW - General 1\Database of polymer electrolytes\08052025_9_PEGMEA_1_PEGDA_0to15_Aerosil380_0_LiTFSI\Processing"
#fr"C:\Users\cooper94\OneDrive - LLNL\High-Throughput Polymer Electrolytes DIW - General\Database of polymer electrolytes\01092024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI\MPR Processing\Results"

paths = pd.read_csv(dictionary)
#path = fr"C:\Users\cooper94\OneDrive - LLNL\High-Throughput Polymer Electrolytes DIW - General\Database of polymer electrolytes\mpr_new_files_converted\01092024_9_PEGMEA_1_PEGDA_0_Aerosil380_10_LiTFSI_A1_RUN_3_C01_RUN_1.csv"
for x in range(0, len(paths) +1):
    EIS_analysis.run_EIS_analysis(paths['filepath'][x], save_path)

# %%
