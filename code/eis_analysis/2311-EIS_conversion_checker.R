# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Digital Twins (SI)
# Subproject: Polymer Electrolyte

### saves csv files of converted files
# provide location of the converted files and destination for the final csv
conv_checker <- function(input_path, output_path) {
  
  require(tidyverse)
    
  # file location of individual converted files
  file_location <- r{'input_path'}
  # location for the final csv
  output_location  <- r{'output_path'}
  
  # saves the files into a tibble
  df_eis <-
    tibble::tibble(csv_files =
                    list.files(file_location,
                                pattern = '.csv'))

  # saves the dataframe into a csv file
  utils::write.csv(
    df_eis,
    output_location
  )
}


#testing
input <- 'C:\Users\jimenez45\OneDrive - LLNL\General - High-Throughput Polymer Electrolytes DIW\Database of polymer electrolytes\mpr_results_from_Jay_2'
output <- "C:\Users\cooper94\Documents\Git\SPOC_function_testing"
conv_checker(input, output)