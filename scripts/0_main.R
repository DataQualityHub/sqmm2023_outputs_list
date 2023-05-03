
date <- Sys.Date()

#Necessary imports
library(tidyverse)
POLYFUZZ <- reticulate::import("polyfuzz")
#uninstall numpy and install pip install numpy == 1.19.0


#Data location
data_str <- "outputs_lists.xlsx"



#Scripts
source("1_data_cleaning.R")
source("2_matching.R")

writexl::write_xlsx(final_dataset, paste0(".//output//linking_", date,".xlsx"))
