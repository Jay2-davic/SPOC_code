#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
#Subproject: SPOC

#%%
'''
Loads the script from Git and the relevant libraries
'''
# load OUN
OUN = 'jimenez45'

# imports libraries and in-house scripts
import sys
import pandas as pd
sys.path.append(f"C:\\Users\\{OUN}\\Documents\\Git\\23-spoc_code\\EIS_Analysis")
from PEISextract import ApplyRandlesFits, MPR_convert
import ipywidgets as widgets
from ipywidgets import interact, Output, VBox, HBox
import matplotlib.pyplot as plt

#%% 
'''
Loads the csv file that contains the file paths of the .mpr raw files
It then checks for already finished files.
Note that we need to run the cleaning script in R to update the finished files
before it gets marked here.
'''
# loads directory and filters files that are already done
directory = f'C:\\Users\\{OUN}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\EIS_full_files.csv'
df = pd.read_csv(directory)
#df['head_count'] = pd.to_numeric(df['head_count'], errors = 'coerce').astype('Int64')df = df[df['head_count'].notna()]
# df = df[~mask]
df = df[(df['file_done'] == True)].reset_index()
#file = MPR_convert('(C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\20240913_9_PEGMEA_1_PEGDA_0_AlO3_0_LiTFSI\\2024_9_13_12_32_8_B1_RUN4\\2024_9_13_12_32_8_B1_RUN4_C01.mpr')
# %%
def apply_randles_fits_wrapper(n, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess, head_count, save_file):
  head_count = int(head_count)
  n=int(n)
  ApplyRandlesFits(file_name=df['filepath'][n],
                     R0_guess=R0_guess,
                     R1_guess=R1_guess,
                     Wo1_0_guess=Wo1_0_guess,
                     Wo1_1_guess=Wo1_1_guess,
                     C1_guess=C1_guess,
                     head_count=head_count,
                     save_file=save_file,
                     global_opt=False
                     )

# n text input
n_text = widgets.Text(value='0', description='File Index')

# Define sliders for R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess
R0_slider = widgets.FloatLogSlider(value=-2, base=10, min=-2, max=10, step=1, description='R0_guess:')
R1_slider = widgets.FloatLogSlider(value=1, base=10, min=1, max=10, step=1, description='R1_guess:')
Wo1_0_slider = widgets.FloatLogSlider(value=1, base=10, min=1, max=10, step=1, description='Wo1_0_guess:')
Wo1_1_slider = widgets.FloatLogSlider(value=1, base=10, min=1, max=10, step=1, description='Wo1_1_guess:')
C1_slider = widgets.FloatLogSlider(value=-1, min=-10, max=-1, step=1, description='C1_guess:')
head_C_slider = widgets.IntSlider(value=75, min=1, max=255, step=1, description='Head_count:')
save_file_button = widgets.Checkbox(description='Save File')
apply_button = widgets.Button(description='Apply')

# Use interact to create a live interface
i_plot = widgets.interact(apply_randles_fits_wrapper, 
         n=n_text,
         R0_guess=R0_slider, 
         R1_guess=R1_slider, 
         Wo1_0_guess=Wo1_0_slider, 
         Wo1_1_guess=Wo1_1_slider, 
         C1_guess=C1_slider,
         head_count=head_C_slider,
         save_file=save_file_button,
         apply_button=apply_button)

i_plot.height = '1000px'
i_plot



# %%
