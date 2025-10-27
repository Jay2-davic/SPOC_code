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

#%%
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

def load_file(filename, headcount):
    '''
    Opens the csv file of raw data, outputs impedance and frequency arrays and the values of the x and y axis.

    Inputs:
        filename = filepath
        headcount = number of data points
    
    Outputs:
        frequencies = the entire array of frequency values
        Z = the entire array of impedance values
        frequencies_fit = the frequency array with only the specified number of data points
        Z_fit = the impedance array with only the specified number of data points
        x = the specified number of data points for the x axis (real impedance)
        y = the specified number of data points for the y axis (imaginary impedance)
    '''
    # get the impedance and frequency arrays
    Z, frequencies = EIS_cleaning.ApplyFits_csv(filename) #data_dictionary['filepath'][index])

    # select the desired number of points
    frequencies_fit = frequencies[:headcount]

    # select the desired number of points
    Z_fit = Z[:headcount]

    # get the values of the file as a dataframe
    file_df = pd.read_csv(filename)

    # x axis
    x = file_df['Re(Z)'][:headcount]
    
    # y axis
    y = file_df['Im(Z)'][:headcount]
    
    return(frequencies, Z, frequencies_fit, Z_fit, x, y)
    

def derivative_indicator(x, y):
    '''
    Uses the derivative of the impedance spectra to identify the end of the semicircle.

    Inputs:
        x = Real component of impedance
        y = Imaginary component of impedance

    Outputs:
        R1_peak = The point with the largest second derivative. The point furthest to the right of the x axis is selected to 
        avoid second derivative peaks at the beginning of the semicircle.
    '''
    # first derivative
    dy_dx = np.gradient(-y, x)  

    # second derivative
    d2y_dx2 = np.gradient(dy_dx, x)

    # array of the indexes of 15 largest second derivative values
    deriv_ind = np.argsort(abs(d2y_dx2))[-15:]

    # choose the rightmost peak since the Warburg diffusion line generally has a second derivative close to zero
    last_peak = np.argsort(deriv_ind)[-1]

    # the index of R1 guess
    R1_peak = deriv_ind[last_peak]

    return(R1_peak)
    
def calculate_guess(x, y, frequencies_fit, headcount):
    '''
    Uses the derivative and frequency values to guess circuit element values.

    Inputs:
        x = real impedance values
        y = imaginary impedance values
        frequencies_fit = array of frequencies
        R1_peak = index of R1 guess
        headcount = number of data points

    Outputs:
        R0 = initial guess for electrolyte resistance
        R1 = initial guess for charge transfer resistance
        C1 = initial guesses for double layer capacitance
        Wo1_0/Wo1_1 = initial guesses for Warburg coefficients
    '''

    # calculates initial guesses and plots the fit line
    # R0 guess, beginning of the semicircle
    R0 = x[0]
    # R0 constraint
    if R0 < 0:
        R0 = 1
    
    # R1 guess, end of the semicircle
    R1_peak = EIS_analysis.derivative_indicator(x, y)
    R1 = x[R1_peak]

    # C1 guess, top of the semicircle
    #w_freq = frequencies_fit[int(R1_peak/2)]
    w_freq = frequencies_fit[0]
    C1 = 1/(2*math.pi*w_freq*(R1 - R0))

    # Wo1_0 guess, the end of the warburg impedance line
    Wo1_0_count = 2
    Wo1_0 = y[headcount - 1]
    while Wo1_0 < 0:
        Wo1_0 = y[headcount - Wo1_0_count]
        Wo1_0_count +=1

    # Wo1_1 guess, the slope of the warburg impedance line, assumes ideal 45 degree angle
    Wo1_1 = 45/math.sqrt(frequencies_fit[headcount - 1])

    return(R0, R1, C1, Wo1_0, Wo1_1)

def calc_conductivity(thickness, area, R0, R1):
    '''
    Calculate the ionic conductivity of the cell.

    Inputs:
        thickness = thickness of the printed part
        area = area of the printed part
        R0 = initial guess for electrolyte resistance
        R1 = initial guess for charge transfer resistance

    Outputs:
        sigma = ionic conductivity
    '''
    resistance = R1 - R0
    sigma = thickness/(resistance * area)
    return(sigma)

def get_guesses(filename, headcount):
    '''
    Wrapper for the circuit element guess functions.

    Inputs:
        filename = filepath
        headcount = number of data points

    Outputs:
        R0 = initial guess for electrolyte resistance
        R1 = initial guess for charge transfer resistance
        Wo1_0/Wo1_1 = initial guesses for Warburg coefficients
        C1 = initial guesses for double layer capacitance
    '''
    # get frequency and impedance arrays, get x and y axis values
    frequencies, Z, frequencies_fit, Z_fit, x, y = EIS_analysis.load_file(filename, headcount) 
    
    # calculates initial guesses
    R0, R1, C1, Wo1_0, Wo1_1 = EIS_analysis.calculate_guess(x, y, frequencies_fit, headcount)
    
    return(R0, R1, C1, Wo1_0, Wo1_1)

def ApplyRandlesFits(
    filename, output_path,
    R0_guess, R1_guess, 
    Wo1_0_guess, Wo1_1_guess, 
    C1_guess, headcount, 
    save_file, global_opt):
    '''
    Fits the impedance model to a Randles circuit. Uses input values as circuit elements to fit the data.

    Inputs: 
        filename = csv file of EIS data
        output_path = location where files are saved
        R0_guess = initial guess for electrolyte resistance
        R1_guess = initial guess for charge transfer resistance
        Wo1_0_guess/Wo1_1_guess = initial guesses for Warburg coefficients
        C1_guess = initial guesses for double layer capacitance
        headcount = parses the data from 1 to specified count 
        save_file = saves the file in the same directory
        global_opt = 

    Outputs: 
        randles with CPE fitting parameters
        Nyquist plots with fitting
        Bode Plots with fitting
        Residual plots of real and imaginary components of the fits
        Dataframe with inputs and output models
    '''
    # extracts the basename of the file
    basename = os.path.basename(filename)
    
    # runs the randles circuit model with initial guesses defined by user
    with suppress_stdout(suppress=True):
        randles = mods.Randles(
            initial_guess = [
                R0_guess, R1_guess, 
                Wo1_0_guess, Wo1_1_guess, 
                C1_guess
                ]
            )
    
    # loads the Lin-KK method outputs
    with suppress_stdout(suppress=True):
        results_df, M, mu, Z_linKK, res_real, res_imag = EIS_analysis.EIS_validate(filename)
        linkk_passfail = results_df['linKK_passFail'][0]

    # runs the function ApplyFits to load fitting parameters
    #with suppress_stdout(suppress=True):
        #Z, frequencies = EIS_cleaning.ApplyFits_csv(filename)

    # parse data based on how many data points as defined by user
    frequencies, Z, frequencies_fit, Z_fit, x, y = EIS_analysis.load_file(filename, headcount)


    # applies randles fit and prints their fit parameters
    with suppress_stdout(suppress=True):
        randles_fit = randles.fit(
            frequencies_fit,
            Z_fit, 
            global_opt = global_opt, 
            #method = 'trf',
            weight_by_modulus = True
            ) #global opt does not work for Randles #*****
    
    # outputs the curve fitting
    with suppress_stdout(suppress=True):
        Z_randles = randles.predict(frequencies_fit)
    
    # calculates residuals and complex components from the fitted prediction
    rmse_real, rmse_imag, Z_real, Z_imag = EIS_analysis.calcValues(
        Z_fit, 
        Z_randles)

    if save_file == True:
        with suppress_stdout(suppress=True):
            # plots the bode plot
            fig, ax2 = plt.subplots(figsize = (5,5))
            ax2.bode_plot = randles_fit.plot(
                #ax2,
                f_data = frequencies_fit, 
                Z_data = Z_fit, 
                kind = 'bode',
                units = '\Omega')
            ax2.legend(['Data', 'Fitted'], loc = 'upper right')
            ax2.set_title(f'Bode Plot', size = 20)
            plt.axis('auto')
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {filename}', 80)), size = 10)
            plt.tight_layout()   

            file_namePlot = basename.replace(
                ' ',
                '_').replace(
                    '.csv',
                    '_BodePlot_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close(fig)

        # plots the linkk plot
        with suppress_stdout(suppress=True):
            fig, ax3 = plt.subplots(figsize = (5,5))
            ax3.residual_plot = viz.plot_residuals(
                ax3, frequencies, 
                res_real, res_imag, 
                y_limits = (-5,5))
            ax3.set_title(fr'Lin-KK Residuals, $\mu$ = {mu}, # circuits = {M}', size = 20)
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {filename}', 80)), size = 15)
            plt.tight_layout()

            file_namePlot = basename.replace(
                ' ',
                '_').replace(
                    '.csv',
                    '_LinKKResiduals_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')
            plt.close(fig)

        # plots the nyquist plot
        with suppress_stdout(suppress=True):
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
            plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {filename}, {rmse_imag}, {rmse_real}', 80)), size = 10)
            plt.tight_layout()
            

            file_namePlot = basename.replace(
                ' ',
                '_').replace(
                    '.csv',
                    '_randles.png')
            plt.savefig(output_path + r'\\' + file_namePlot, dpi = 600, format = 'png')   
            plt.close(fig)     
            
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
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {filename}, {rmse_imag}, {rmse_real}', 80)), size = 10)
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
        filename, headcount, 
        res_real, res_imag, 
        mu, M, linkk_passfail,
        rmse_real, rmse_imag
        #bode_plot, #residual_plot
        ]]
    
    # adds column names to the dataframe
    stats_df_names = [
        'EIS_file', 'headcount', 
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
    output_df = EIS_cleaning.EISresult_cleaner(randles_fit)
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
            output_path + r'\\' + file_nameCSV, 
            index = False)
        print(
            'dataframe saved as', 
            f'{file_nameCSV}')
    else:
        print('Output not saved')  


def apply_randles_fits_wrapper(file, output, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess, headcount, save_file, global_opt):
    '''
    Wraps the function to apply the impedance data to a Randles circuit model with impedance.py.

    Inputs:
        n = sample index
        R0_guess = initial guess for electrolyte resistance
        R1_guess = initial guess for charge transfer resistance 
        Wo1_0_guess = initial guess for 
        Wo1_1_guess =
        C1_guess = initial guess for double layer capacitance
        headcount = number of data points
        save_file = determines if the current guess values are saved as
    
    Outputs:

    '''
    #file_data = filename= file['filepath'][n]
    headcount = int(headcount)
    #print(df['filepath'])
    #n=int(n)
    EIS_analysis.ApplyRandlesFits(file, 
                                    output_path = output, 
                                    R0_guess=R0_guess, 
                                    R1_guess=R1_guess, 
                                    Wo1_0_guess=Wo1_0_guess, 
                                    Wo1_1_guess=Wo1_1_guess, 
                                    C1_guess=C1_guess, 
                                    headcount=headcount, 
                                    save_file=save_file, 
                                    global_opt = global_opt )
    


def initiate_custom(guess_array, circuit_string = ''):
    circuit = CustomCircuit(
        circuit = circuit_string,
        initial_guess = guess_array
    )

def get_elements(circuit_string):
    

