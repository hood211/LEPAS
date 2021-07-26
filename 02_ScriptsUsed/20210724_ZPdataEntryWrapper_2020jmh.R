
# LEPAS Zooplankton Data Entry 2020"
library("here")
  
#### LEPAS data entry ####
# This code enters zooplankton data into the LEPAS database. These front-end functions and the back-end script 
# "Zooplankton_data_entry_functions_2020.R" were modified from Dr. Josh Stone's original versions in January, 2020 
# by Dr. Daniel. R. O'Donnell to be compatible with the overhauled "LEPAS_2020" SQLite database (LEPAS_2020.db), and 
# to accomodate batch submission of datasheets.
# 
# Please follow the steps outlined below:
#   
#   1. Enter the the sampling year (e.g. "2020") and the correct Lake Erie basin (e.g. "WB"). 
# 
# 2. Make sure the file paths below (in the 'FilePath', "FileName', 'Database', and 'source' lines) point to the correct 
#    data entry spreadsheets and folders that you wish to enter into the database. 
# 
# **Only use the correctly formatted data entry sheet that has been provided.**
# 
# 3. Run the code below by pressing the green arrow -> at the right side of the grey code box.   

Year <- "2020"
Basin <- "CB"
# THIS WILL LOAD ONE FILE
# FileNames <- c("20200604_16-970.csv")

# THIS WILL LOAD ALL THE FILES
FileNames <- list.files("06_RawZPdata/2020/CB", pattern = "[.csv]")
# FilePath <- paste0("/06_RawZPdata/2020/WB") #OLD VERSION
FilePath <- file.path(here::here("06_RawZPdata/2020/CB/"))
# Database <- paste0("/Users/hood.211/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/FADX09/01_LEPASgitHub/LEPAS_20210724.db")
# outside of github so can't use here
Database <- file.path("/Users/hood.211/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/FADX09/01_LEPASgitHub/LEPAS_20210725.db")
source(here::here("02_ScriptsUsed/20210724_ZPdataEntryFunctions_2020jmh.R"))

# JUST FOR TESTING 
# Files <- c("20200618_8-994.csv")

step1(FileNames)



#### Data Check 1 ####
# 4. Check that the data have been entered properly, and that 'Number Measured OK' is displayed above for each datasheet.
#    
# 5. Type "OK" (including quotes) into the parentheses after 'input1' in the line below and run 
#    the code by pressing the green arrow.

input1("OK")

# 6. If data have been successfully entered, run the step2 code below by pressing the green arrow.


step2(X)



#### Data Check 2 ####
# 7. Check that the data have been entered properly. Look at table titled 'Zoop_final'.
# 8. Type "OK" into the parentheses after 'input2' in the line below and run the code by pressing the green arrow.
# 
# **This code will also write the datasheet to a .csv and send it to the "Entered" folder**

input2("OK")


