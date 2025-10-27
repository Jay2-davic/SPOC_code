# loads the relevant EIS processing packages
from impedance import preprocessing as proc
from impedance import visualization as viz
from impedance.validation import linKK
from impedance import models
from impedance.models import circuits as mods
from galvani import BioLogic
from impedance.models.circuits import Randles, CustomC

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

#import SPOC Code
from EIS_process_compiled import EIS_analysis 


#import data set
filepath = # enter path to raw data file, either .mpr or .csv format
data_file = fr"{filepath}"

#designate a location to save outputs
savepath = # enter path to folder of results
save_folder = fr"{savepath}"

EIS_analysis.run_EIS_analysis(data_file, save_folder)

