#Property of Lawrence Livermore National Laboratory
#Author: J.C. Jimenez, PhD.
#Group: STE ENG-STE MED-MATERIALS ENGINEERING
#Project: Digital Twins (SI)
#Subproject: Polyelectrolyte Screening

# process rheology

# chelsea's code
# Load required packages
library(readr)
library(ggplot2)
library(cowplot)
library(glue)

# Define the file location of the .csv file
file_location <- "C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Chelsea\\Rheology\\"


files <- tibble::tibble(filename= 
                          list.files(file_location, 
                                     full.names=TRUE,
                          ))

# Define the folder where you want to save the plot
save_to <- "C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\"

# Read the .xlsx file - pipe symbol to string another command in one script
df <- readxl::read_xlsx(files$filename[1],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - curing Pegda pegmea 30litfsi  15aerosil 380 100ppm  15.03mWcm2 1000um gap.xlsx") # Specify value of columns

df2 <- readxl::read_xlsx(files$filename[2],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - curing Pegda pegmea 30litfsi  15aerosil 380 100ppm nile red 15.03mWcm2 1000um gap.xlsx") # Specify value of columns

df3 <- readxl::read_xlsx(files$filename[3],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - curing Pegda pegmea 30litfsi 100ppm nile red 15aerosil 380 100ppm  15.03mWcm2 1000um gap.xlsx") # Specify value of columns

df4 <- readxl::read_xlsx(files$filename[4],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap-2 rs - Copy.xlsx") # Specify value of columns

df5 <- readxl::read_xlsx(files$filename[5],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap-3 rs - Copy.xlsx") # Specify value of columns

df6 <- readxl::read_xlsx(files$filename[6],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns
df7 <- readxl::read_xlsx(files$filename[7],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns

df8 <- readxl::read_xlsx(files$filename[8],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 15.50 mWcm2 power 750um gap - Copy.xlsx") # Specify value of columns

df9 <- readxl::read_xlsx(files$filename[9],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15wt% aerosil 0.1wt% pi 13.40mWcm2 power 250um gap.xlsx") # Specify value of columns

df10 <- readxl::read_xlsx(files$filename[10],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 20litfsi   0.1wt% pi 15.50 mWcm2 power 300um gap-2.xlsx") # Specify value of columns

df12 <- readxl::read_xlsx(files$filename[12],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 20litfsi   0.1wt% pi 15.50 mWcm2 power 300um gap.xlsx") # Specify value of columns

df13 <- readxl::read_xlsx(files$filename[13],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns

df14 <- readxl::read_xlsx(files$filename[14],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns

df15 <- readxl::read_xlsx(files$filename[15],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns

df16 <- readxl::read_xlsx(files$filename[16],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs - Copy.xlsx") # Specify value of columns

df17 <- readxl::read_xlsx(files$filename[17],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Loss Modulus - Pegda pegmea nile red 100ppm  0.1wt% pi 13.40mWcm2 power 250um gap.xlsx") # Specify value of columns

df18 <- readxl::read_xlsx(files$filename[18],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap-3 rs.xlsx") # Specify value of columns

df19 <- readxl::read_xlsx(files$filename[19],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 13.40mWcm2 power 700um gap rs.xlsx") # Specify value of columns

df20 <- readxl::read_xlsx(files$filename[20],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 15.50 mWcm2 power 750um gap-2.xlsx") # Specify value of columns

df21 <- readxl::read_xlsx(files$filename[21],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 15 aerosil 20litfsi  nile red 0.1wt% pi 15.50 mWcm2 power 750um gap.xlsx") # Specify value of columns

df22 <- readxl::read_xlsx(files$filename[22],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 15wt% aerosil 0.1wt% pi 13.40mWcm2 power 250um gap.xlsx") # Specify value of columns

d23 <- readxl::read_xlsx(files$filename[23],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea 20litfsi   0.1wt% pi 15.50 mWcm2 power 300um gap-2.xlsx") # Specify value of columns

#df24 <- readxl::read_xlsx(files$filename[24],sheet=1) |>
# janitor::clean_names() |>
#dplyr::mutate(group="Storage Modulus - Pegda pegmea 20litfsi  0.1wt% pi 15.50 mWcm2 power 300um gap.xlsx") # Specify value of columns

df25 <- readxl::read_xlsx(files$filename[25],sheet=1) |>
  janitor::clean_names() |>
  dplyr::mutate(group="Storage Modulus - Pegda pegmea nile red 100ppm  0.1wt% pi 13.40mWcm2 power 250um gap.xlsx") # Specify value of columns

# combining data together
dffinal <- dplyr::bind_rows(df,df2,df3,df4,df5,df6,df7,df8,df9,df10,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df25) |>
  dplyr::mutate(type_modulus = stringr::str_extract(group, 'Loss Modulus|Storage Modulus'),
                group = stringr::str_replace(group, '15wt%|15 ', '15'),
                group = stringr::str_replace(group, '15 aerosil ', '15aerosil '),
                group = stringr::str_replace(group, '  ', ' '),
                additive_wt = dplyr::case_when(
                  stringr::str_detect(group, '\\d{2}aerosil') == TRUE ~ stringr::str_extract(group, '\\d{2}aerosil'),
                  TRUE ~ '0aerosil'),
                additive_wt = stringr::str_remove(additive_wt, 'aerosil'),
                salt_wt = dplyr::case_when(
                  stringr::str_detect(group, '\\d{2}litfsi') == TRUE ~ stringr::str_extract(group, '\\d{2}litfsi'),
                  TRUE ~ '0litfsi'),
                salt_wt = stringr::str_remove(salt_wt, 'litfsi'),
                nile_red = dplyr::case_when(
                  stringr::str_detect(group, '100ppm nile red|nile red 0.1wt%|nile red 100ppm') == TRUE ~ 100,
                  TRUE ~ NA
                ))

# Set your variable names
variable_1 <- "Step Time (s)"  # Replace with the actual column name for x-axis
variable_2 <- "G'/G (MPa)"  # Replace with the actual column name for y-axis

# Create the plot and save it as a variable "plot"
plot <- ggplot2::ggplot(data = dffinal, ggplot2::aes(x = x,
                                                     y = y,
                                                     group = type_modulus,
                                                     color = type_modulus)) + 
  cowplot::theme_half_open() +
  ggplot2::geom_point(size = 1.7) +
  #ggplot2::geom_line(size = 0.4) +
  ggplot2::theme(legend.position = "bottom",
                 #strip.text = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size=19)) +
  ggplot2::labs(x = "Shear Rate (1/s)", y = "Viscosity (Pa.s)") +
  ggplot2::scale_y_continuous(trans="log10") +
  ggplot2::facet_wrap(additive_wt ~ salt_wt,
                      scales = 'free')

#limits = c(0,10)) # limits of the graph

print(plot)

# Save the plot with the correct syntax
ggplot2::ggsave(filename = glue::glue("{save_to}/2410-Photorheology_collected.png"), 
                plot = plot, 
                dpi = 600, 
                width = 4, 
                height = 3, 
                bg = "white")  # Correctly formatted

# loads the functions

# dsc plotting



