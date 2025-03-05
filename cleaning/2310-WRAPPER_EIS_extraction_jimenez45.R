# Property of Lawrence Livermore National Laboratory
# Author: J.C. Jimenez, PhD.
# Group: STE ENG-STE MED-MATERIALS ENGINEERING
# Project: Polyelectrolyte

# loads tidyverse
require(furrr)
require(dplyr)
require(svDialogs)
require(lubridate)

# EIS extraction
execute_function_call <- function(call_str) {
  # Remove parentheses and call the function
  function_name <- substr(call_str, 1, nchar(call_str))
  result <- do.call(function_name, list())
  return(result)
}

# finds the string pattern 'EIS-extraction' in the cleaning script folder
df_cleaning <- tibble::tibble(
  filepaths =
    list.files(
      pattern = "EIS-extraction",
      # goes inside the EIS_Cleaner git folder and finds the cleaning scripts
      path = "~/Git/23-spoc_code/EIS_Cleaner",
      full.names = TRUE
    ))

# goes through and runs all of the cleaning scripts (full-updates)
lapply(df_cleaning$filepaths,
       source)

rm(EIS_finder, df_cleaning, execute_function_call)

print('EIS Files have been updated')
print(lubridate::now())

      