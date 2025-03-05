# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte

### saves csv files of converted files
require(tidyverse)
# indicates the file location of converted files
file_location <-
  r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_results_from_Jay_2)'

# saves the files into a tibble
df_eis <-
  tibble::tibble(csv_files =
                   list.files(file_location,
                              pattern = '.csv'))

# saves the dataframe into a csv file
utils::write.csv(
  df_eis,
  r'(C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\EIS_converted.csv)'
)
