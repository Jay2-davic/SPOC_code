library(tidyverse)

#make data dictionary for raw data
mpr_collect <- function(EIS_rawfiles, mpr_path) {
  # Get the raw mpr files and compile the names to a data frame
    file <- tibble::tibble(
      filepath = list.files(
      {EIS_rawfiles},
      full.names = TRUE,
      pattern = '.csv'
    )
    )
  readr::write_csv(file, mpr_path)
}

# run EIS_Cleaner.CycleSplitter() 

write_dictionary <- function(converted_files, dictionary_path){
  # Get the file paths of the cycle split
  files <- tibble::tibble(
    filepath = list.files(
      {converted_files},
      full.names = TRUE
    )
  )
  # writes the final data dictionary to the specified path
  readr::write_csv(files, dictionary_path)
}


collect_type <- function(string, location, output){
    files = list.files(
      pattern = string,
      path = {location},
      full.names = FALSE
    )
    for (x in files) {
      old_loc <- paste(location, x, sep = "\\")
      new_loc <- paste(output, x, sep = "\\")
      file.rename(old_loc, new_loc)
    }
}






mprraw <- "C:/Users/cooper94/OneDrive - LLNL/High-Throughput Polymer Electrolytes DIW - General/Database of polymer electrolytes/20250724_9_PEGMEA_1_PEGDA_0to15Aero380_10_LiTFSi_1_ppm_Initiator"
output <- "C:/Users/cooper94/OneDrive - LLNL/High-Throughput Polymer Electrolytes DIW - General/Database of polymer electrolytes/20250724_9_PEGMEA_1_PEGDA_0to15Aero380_10_LiTFSi_1_ppm_Initiator/Processing/dictionary.csv"
mpr_collect(mprraw, output)


split_path <- "C:\\Users\\cooper94\\Documents\\newEC-labTests\\output1"
dict_output <- "C:\\Users\\cooper94\\Documents\\newEC-labTests\\data_dictionary.csv"
write_dictionary(split_path, dict_output)


bode_loc <- "C:\\Users\\cooper94\\OneDrive - LLNL\\High-Throughput Polymer Electrolytes DIW - General\\Database of polymer electrolytes\\01092024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI\\MPR Processing\\Results"
output <- "C:\\Users\\cooper94\\OneDrive - LLNL\\High-Throughput Polymer Electrolytes DIW - General\\Database of polymer electrolytes\\01092024_9_PEGMEA_1_PEGDA_X_Aerosil380_10_LiTFSI\\MPR Processing\\Bode Plots"
plots <- collect_type("BodePlot", bode_loc, output)