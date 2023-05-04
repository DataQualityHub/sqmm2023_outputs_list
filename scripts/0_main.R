

#Necessary imports
library(tidyverse)
POLYFUZZ <- reticulate::import("polyfuzz")
#uninstall numpy and install pip install numpy == 1.19.0


#Data location
data_str <- ".//input//outputs_lists.xlsx"
staff_str <- ".//input//12_Staff Counts 31 Mar 2023.xlsx"


#Scripts
source(".//scripts//1_data_cleaning.R")
source(".//scripts//2_matching.R")
source(".//scripts//3_series_exploration.R")

'#Exporting
date <- Sys.Date()
writexl::write_xlsx(final_dataset, paste0(".//output//outputs_linking_", date,".xlsx"))


