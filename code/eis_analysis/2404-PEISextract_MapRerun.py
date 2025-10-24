#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: SI - Digital Twins
#Subproject: SPOC

#%%
import sys
import pandas as pd

# finds the files in the git folder
#%%
'''
Loads the script from Git
'''
#sys.path.append(f"C:\\Users\\{OUN}\\Documents\\Git\\23-spoc_code\\EIS_Analysis")
from PEISextract import ApplyRandlesFits
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
<<<<<<< HEAD
# loads directory and filters files that are already done
directory = f'C:\\Users\\{OUN}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v2,16_FULL.csv'
=======
def load_dir(path):
  '''
  loads directory and filters files that are already done
>>>>>>> main

  Inputs:
    path = file path to 

  Outputs:
    df = 
  '''
  
  directory = f'{path}'
  #f'C:\\Users\\{OUN}\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Database of polymer electrolytes\\2305-jimenez45_PEdatabase_v2,8_FULL.csv'
  df = pd.read_csv(directory)
  df['head_count'] = pd.to_numeric(df['head_count'], errors = 'coerce').astype('Int64')
  df = df.dropna(subset = ['head_count'])

  return(df)
# %% 
'''
This code chunk re-runs the entire dataset using the initial values
saved in the outputs. The purpose of this is to update the files if
new updates to the code like corrections or new ways of representation
is implemented. We have a mode for updating all of the files.
'''

ApplyRandlesFits(file_name = df['mpr_files_cycle'][n],
                 R0_guess = df['InitialGuess.R0'][n],
                 R1_guess = df['InitialGuess.R1'][n],
                 Wo1_0_guess = df['InitialGuess.Wo1_0'][n],
                 Wo1_1_guess = df['InitialGuess.Wo1_1'][n],
                 C1_guess = df['InitialGuess.C1'][n],
                 head_count = int(df['head_count'][n]),
                 save_file = False,
                global_opt = False
                ) 

#%%
'''
This code chunk allows for fast re-run using map.
We can implement this with the futures pacakges and
take advantage of parallel computing for an even
faster run.
'''
def ApplyRandlesFits_error(mpr_files_cycle, R0, R1, Wo1_0, Wo1_1, C1, head_count):
    try:
        return ApplyRandlesFits(mpr_files_cycle, R0, R1, Wo1_0, Wo1_1, C1, head_count, save_file=True, global_opt=False)
    except Exception as e:
        print(f"Error processing file {mpr_files_cycle}: {e}")
        return None

def collect_results():
   list = list(
      map(
        lambda args: ApplyRandlesFits_error(*args), 
          zip(df['mpr_files_cycle'],
              df['InitialGuess.R0'],
              df['InitialGuess.R1'],
              df['InitialGuess.Wo1_0'],
              df['InitialGuess.Wo1_1'],
              df['InitialGuess.C1'],
              df['head_count']),
          )
   )
   return(list)
#%%

testing
'''
Performs circuit fitting using Randles circuit as the equivalent
circuit. The files are directly queried from the csv file and 
initial guesses are inputted by the user.

Input:
  file_name = full file paths of the .mpr files
  R0_guess = initial guess of the series resistance
  R1_guess = initial guess of the bulk resistance
  Wo1_0_guess = initial guess of the real component of the Warburg coefficient
  Wo1_1_guess = initial guess of the imaginary component of the Warburg coefficient
  C1_guess = initial guess of the capacitance
  head_count = parses the points where the specified value is the maximum points from
    beginning
  save_file = specifies if the files are to be saved right after processing
  global_opt = toggles the global opt (using scipy.curve.fit) or (scipy.basinhopping)
  * currently the basinhopping has bugs that prevent us from saving the outputs =(
  
Inputs in the beginning for easy running
'''

n = 2
r1 = 4
Wo0 = 9
Wo1 = 7
C1 = 6
head_C = 80
save_file = False
# save_file = True

ApplyRandlesFits(file_name = df['filepath'][n],
                #  OUN = 'jimenez45',
                 R0_guess = 1e-2,
                 R1_guess = float(f'1e{r1}'),
                 Wo1_0_guess = float(f'1e{Wo0}'),
                 Wo1_1_guess = float(f'1e{Wo1}'),
                 C1_guess = float(f'1e-{C1}'),
                 head_count = head_C,
                 save_file = save_file,
                # save_file = True,
                global_opt = False
                # global_opt = True
                ) 
