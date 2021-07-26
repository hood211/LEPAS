#########################################################################################################

# These functions are called by script Zooplankton_data_entry_2020.Rmd. They import the raw datasheets, 
# parse and organize the data into the relevant tables in LEPAS_2020.db. These functions were originally
# written by Dr. Josh Stone, and were modified in January, 2020 by Dr. Daniel R. O'Donnell to be compatible 
# with the overhauled "LEPAS_2020" database.

#########################################################################################################


## Require the necessary R packages.
library(tidyverse)
library(RSQLite)
library(readxl)

## This is needed later for batch submission.
table_list <- list()

## This function loads the SQLite database file into the object "zoop" (creates a temporary SQLite .bd file).
step0.5 <- function(Files){
  
  ## Imports the raw datasheet. Allows for loading datasheets in .csv, .xls and .xlsx formats.
  ifelse(grepl(".xls",Files),
         data <- read_excel(paste0(FilePath,Files), col_names=T, na=c("","NA"), col_types="text"),
         data <- read.csv(paste0(FilePath,Files), header=T, na.strings=c("","NA"), colClasses="character"))
  
  ## zoop is a temporary SQLite object that is created (but invisible) in LEPAS_2020.db. It can call data from
  ## any table in LEPAS_2020.db, as well as create new, temporary data tables, which can later be written to 
  ## the corresponding permanent tables in LEPAS_2020.db.
  ## "Database" is the /path/to/file.db in Zooplankton_data_entry.
  zoop <<- dbConnect(SQLite(), dbname=Database)

  ## Counts number of species detected in a sample.
  species_num <- sum(!is.na(data$Number_counted)) 
  
  ## Creates data frame of Taxa and counts number of length measurements per species.
  measured_num <- data %>% 
    select(Genus_sp_lifestage) %>% 
    mutate(Datasheet_measured_number = as.numeric(rowSums(!is.na(data[,grepl("L", names(data))]))))
  
  ## Creates data frame of sample information 'SI'.
  # Grabs "Information" and "Value" columns from data, puts Information in wide format.
  # Converts dates into POSIXct.
  SI <<- data %>% 
    select(Information, Value) %>% 
    drop_na(Information) %>% 
    spread(Information, Value) %>% 
    select(Sample_ID,Sample_date,Sample_time,Sample_site,Water_body,Project,Latitude,
           Longitude,Depth_m,Surface_temp,Bottom_temp,Ave_temp,Surface_DO, Bottom_DO,
           Net_diameter_m,Meter_1,Meter_2,Revolutions,Secchi_depth_m,Date_counted,Counter_name,
           Microscope_number,mm_per_ocular_unit,Subsampled_vol_mL,Diluted_vol_mL,Comments) %>%
    mutate(Sample_date = as.POSIXct(Sample_date, format="%Y%m%d"),
           Sample_date = format(Sample_date, format="%Y-%m-%d"),
           Date_counted = as.POSIXct(Date_counted, format="%Y%m%d"),
           Date_counted <- format(Date_counted, format="%Y-%m-%d")) %>% 
    mutate_at(.vars=vars(Latitude:Secchi_depth_m,mm_per_ocular_unit:Diluted_vol_mL),
              .funs=list(~as.numeric(as.character(.))))
  
  ## Notes on units (by JS).
  # Units for table 'SI': mm_per_ocular_unit - ratio, Subsample_Vol_mL - milliliters, Diluted_Vol_mL - milliliters, 
  # Meter_1 - Flow meter rotations,
  # Meter_2 - Flow meter rotations, Depth_m - meters, Temp_C - degrees C
  
  ## Calculate volume of water sampled. 
  net_area <- (pi * SI$Net_diameter_m^2) / 4 # Using (pi*d^2)/4, where d is the net diameter
  
  ## Flow meter rotor constant
  flow_constant <- 26873 
  
  ## Caclulate volume sampled from flow meter readings, net diameter, etc. 
  # Units for net_area in m^2, vol_sampled in m^3.
  vol_sampled <- net_area * (((SI$Meter_2 - SI$Meter_1) * flow_constant)/999999)
  
  ## Creates dataframe of Sample_ID, Genus_sp_lifestage, Number_counted.
  counts <- data %>% 
    select(Sample_ID, Genus_sp_lifestage, Number_counted) %>% 
    ## Add Life_stage column. "Other" will become "Older" and "Younger" later, based on lengths.
    mutate(Life_stage = ifelse(grepl("_eggs",Genus_sp_lifestage) & ! grepl("_w",Genus_sp_lifestage), "Egg",
                               ifelse(grepl("nauplii",Genus_sp_lifestage), "Nauplii",
                                      ifelse(grepl("veliger",Genus_sp_lifestage), "Veliger", "Other")))) %>% 
    # Replace NAs in count column with zeros.
    # NOTE: Zoop_density used to be calculated here, but now it's calculated farther down, after Younger and Older
       # have been parsed.
    mutate(Number_counted = as.numeric(replace(Number_counted, is.na(Number_counted), 0)))
  
  ## Put individual length measurements in long (tidy) format.
  # Units for lengths" table: Length_mm - millimeters.
  lengths0 <- data %>% 
    select(Sample_ID, Genus_sp_lifestage, L1:L20) %>% 
    mutate(Life_stage = ifelse(grepl("_eggs",Genus_sp_lifestage) & ! grepl("_w",Genus_sp_lifestage), "Egg",
                               ifelse(grepl("nauplii",Genus_sp_lifestage), "Nauplii",
                                      ifelse(grepl("veliger",Genus_sp_lifestage), "Veliger", "Other")))) %>% 
    gather(Measured_number, Length_OU, L1:L20) %>% 
    mutate(Length_OU = as.numeric(Length_OU)) %>% 
    mutate(Length_mm = Length_OU * SI$mm_per_ocular_unit)
  
  lengths <- lengths0 %>% 
    # Remove rows with NA and convert to mm from optical units (OU).
    drop_na(Length_OU)
  
  ## This .csv contains the names and length cutoffs of those taxa to be parsed into "Older" and "Younger" 
  ## life stages, as per the 2019 overhaul. Only those taxa that have "Younger" life stages are included 
  ## here, though this table does not contain any "X_immature" taxa. These are designated "Younger" below.
  Zlg_db <- read.csv("Z:\\Projects\\FADX09_LEPAS\\15-LEPAS Datasheets\\Entered Spreadsheets\\3-Zooplankton\\DO_NOT_DELETE_Copepod_length_cutoffs_for_entry_20200117.csv") %>% 
    mutate_if(is.factor, as.character) 
  
  ## Calculate proportions of "Older" and "Younger" individuals for allocation of unmeasured 
  ## individuals age classes. 
  Prop_ls <- lengths0 %>% 
    filter(Life_stage == "Other") %>% 
    mutate_if(is.factor, as.character) %>% 
    ## left_join Zlg_db to set length cutoffs for Younger/Older divide.
    left_join(Zlg_db, by="Genus_sp_lifestage") %>% 
    mutate(Length_cutoff = replace(Length_cutoff, is.na(Length_mm), NA),
           Length_cutoff = replace(Length_cutoff, !is.na(Length_mm) & is.na(Length_cutoff), 0))  %>% 
    group_by(Genus_sp_lifestage) %>% 
    summarize(Prop_O = sum(Length_mm >= Length_cutoff & !is.na(Length_mm)) / sum(!is.na(Length_mm)),
              Prop_Y = ifelse(!is.na(Prop_O), 1 - Prop_O, NA)) %>% 
    mutate(Prop_O = replace(Prop_O, is.nan(Prop_O), NA),
           Prop_O = replace(Prop_O, grepl("immature", Genus_sp_lifestage) & !is.na(Prop_O), 0),
           Prop_Y = replace(Prop_Y, grepl("immature", Genus_sp_lifestage) & !is.na(Prop_Y), 1))
  
  ## Add updated life stages to lengths table.
  lengths2 <- lengths %>% 
    mutate_if(is.factor, as.character) %>% 
    left_join(Zlg_db, by="Genus_sp_lifestage") %>% 
    mutate(Life_stage = replace(Life_stage, !is.na(Length_cutoff) & Length_mm < Length_cutoff, "Younger"),
           Life_stage = replace(Life_stage, !is.na(Length_cutoff) & Length_mm >= Length_cutoff, "Older"), 
           Life_stage = replace(Life_stage, is.na(Length_cutoff) & Life_stage=="Other", "Older"),
           Life_stage = replace(Life_stage, grepl("immature",Genus_sp_lifestage), "Younger")) %>% 
    select(-Length_cutoff) 

  ## Create temporary lengths table in SQLite object 'zoop'.
  ## dbBegin() ties the following RSQLite commands together up to dbCommit().
  dbBegin(zoop)
  
    dbWriteTable(zoop, "Zoop_lengths_temp", lengths2, overwrite=TRUE, temporary=TRUE)
  
    lengths_taxa <- dbGetQuery(zoop, "SELECT 
                                         zl.Sample_ID, 
                                         zl.Genus_sp_lifestage, 
                                         zl.Life_stage, 
                                         zl.Measured_number, 
                                         zl.Length_OU, 
                                         zl.Length_mm
                                       FROM 
                                         Zoop_lengths_temp zl")
    
    ## Not currently used, since Life_stage has been added to lengths (above).
    # LEFT JOIN 
    # Zoop_taxa_groups tg 
    # ON 
    # zl.Genus_sp_lifestage = tg.Genus_sp_lifestage
    
  dbCommit(zoop)
  
  ## Subset out non-eggs and create Zoop_eggs table.
  Zoop_lengths <<- lengths_taxa %>% 
    filter(Life_stage != "Egg")
  Zoop_eggs <<- lengths_taxa %>% 
    filter(Life_stage == "Egg")
  
  counts2 <- counts %>% 
    left_join(Prop_ls, by=c("Genus_sp_lifestage")) %>% 
    mutate(GC4_count = ifelse(!grepl("immature",Genus_sp_lifestage), round(Number_counted*Prop_O), 0),
           LC4_count = ifelse(!grepl("immature",Genus_sp_lifestage), round(Number_counted*Prop_Y), Number_counted),
           GC4_count = replace(GC4_count, is.na(GC4_count) & Life_stage=="Other", 0),
           LC4_count = replace(LC4_count, is.na(LC4_count) & Life_stage=="Other", 0)) %>% 
    select(-Prop_O, -Prop_Y)

  ## Create "Younger" subset 
  Young <- counts2 %>% 
    filter(Genus_sp_lifestage %in% Zlg_db$Genus_sp_lifestage | grepl("immature",Genus_sp_lifestage)) %>% 
    select(-Number_counted, -GC4_count) %>% 
    mutate(Number_counted = LC4_count) %>% 
    select(-LC4_count) %>% 
    mutate(Life_stage = "Younger",
           Number_counted = replace(Number_counted, is.na(Number_counted), 0)) %>% 
    distinct()

  ## Create "Older" subset
  Old <- counts2 %>% 
    filter(Genus_sp_lifestage %in% Zlg_db$Genus_sp_lifestage | grepl("adult",Genus_sp_lifestage)) %>% 
    select(-Number_counted, -LC4_count) %>% 
    mutate(Number_counted = GC4_count) %>% 
    select(-GC4_count) %>% 
    mutate(Life_stage = "Older",
           Number_counted = replace(Number_counted, is.na(Number_counted), 0)) %>% 
    distinct()
  
  ## Create subset that are not Older or Younger
  Neither <- counts2 %>% 
    filter(!Genus_sp_lifestage %in% Young$Genus_sp_lifestage & 
             !Genus_sp_lifestage %in% Old$Genus_sp_lifestage) %>% 
    mutate(Life_stage = replace(Life_stage, Life_stage=="Other", "Older")) %>% 
    select(-(GC4_count:LC4_count)) %>% 
    distinct()

  ## Bind back together as Zoop_counts, calculate densities, and eliminate all "w_eggs" + "Younger" rows, 
  ## as they are all zeros and all meaningless (Youngers don't have eggs).
  Zoop_counts <- bind_rows(Young,Old,Neither) %>% 
    # Calculate zooplankton density from number_counted, subsampled volume, 
    # and volume of water sampled. 
    # Units in individuals per liter.
    mutate(Zoop_density = round((Number_counted * SI$Diluted_vol_mL) / 
                                  (SI$Subsampled_vol_mL * vol_sampled*1000), digits=10)) 
  
  ## Not run. Just for internal checking.
  # nrow(Young) + nrow(Old) + nrow(Neither) - nrow(Old) == nrow(counts2)
  # nrow(Young) + nrow(Old) + nrow(Neither) - nrow(Young) == nrow(counts2)
  # nrow(Young) + nrow(Old) + nrow(Neither) == nrow(Zoop_counts)
  # nrow(Zoop_counts) - nrow(Old) == nrow(counts2)
  # which_sn <- data$Genus_sp_lifestage[!is.na(data$Number_counted)]
  # which_zc <- Zoop_counts$Genus_sp_lifestage[Zoop_counts$Number_counted != 0]

  ## Check if the number of measured individuals to be entered into the database is the same as the number 
  ## in the original input file.
  # Number of length measurements that have been added to Zoop_lengths_temp.
  length_check <- count(lengths2, Genus_sp_lifestage)
  
  # Compare to number of measurements in the raw datasheet. 
  length_check2 <- left_join(measured_num, length_check, by = "Genus_sp_lifestage") %>%
    mutate(Datasheet_measured_number = replace(Datasheet_measured_number, is.na(Datasheet_measured_number), 0),
           n = replace(n, is.na(n), 0),
           diff = Datasheet_measured_number - n) %>%
    rename(Original_datasheet_measured_number = Datasheet_measured_number,
           Data_to_enter_measured_number = n,
           Difference= diff)
  
  ## How many datasheets have been submitted?
  counter <<- counter+1

  ## All temporary tables will be stored in a list, which will be stored as an element in the meta-list "table_list".
  ## Each element of table_list corresponds to a single raw datasheet.
  table_list <<- append(table_list, list(dat=list("SI"=SI,"Zoop_counts"=Zoop_counts,"Zoop_lengths"=Zoop_lengths,
                                                  "Zoop_eggs"=Zoop_eggs)))
  
  ## Checking to make sure all lengths and species have been entered correctly.
  MeasNum_OK <- ifelse(sum(length_check2$Difference) != 0, paste0("Number measured NOT OK, Check datasheet ",counter), 
                       "Number measured OK")
  
  SpecCt_OK <- ifelse(species_num != sum(Zoop_counts$Number_counted != 0), paste0("Species counts NOT OK, Check datasheet ",counter), 
                      "Species counts OK")
  
  ## Complicated ifelse() chain for spitting out happy or sad messages, depending on whether all lengths and species have been
  ## entered correctly.
  ifelse(MeasNum_OK=="Number measured OK" & SpecCt_OK=="Species counts OK", 
         paste0(MeasNum_OK,". ",SpecCt_OK,". Datasheet ",counter," submitted."), 
               ifelse(MeasNum_OK=="Number measured OK" & SpecCt_OK!="Species counts OK", SpecCt_OK,
                      ifelse(MeasNum_OK!="Number measured OK" & SpecCt_OK=="Species counts OK", MeasNum_OK, 
                             ifelse(MeasNum_OK!="Number measured OK" & SpecCt_OK!="Species counts OK",
                                    paste0(MeasNum_OK,". ", SpecCt_OK,"."),
                                    "I'm broken! OMG it HURTS!!! Call Danny O'Donnell, quick!!!"))))
  
  ## Not currently used. This used to make the tables pop up in tabs in RStudio.
  # View(SI)
  # View(Zoop_counts)
  # View(Zoop_lengths)
  # View(Zoop_eggs)
}

## Front-end function run in Zooplankton_data_entry_2020 that will apply step0.5 (above)
## to each raw datasheet in the "NOT_entered" folder.
step1 <- function(x){
  counter <<- 0
  lapply(x, step0.5)
} 

# step1(Files)

## This function inputs all of the sample information, taxonomic info, lengths, counts, etc. into the 
## relevant tables in LEPAS_2020.db, with the exception of Zoop_final, which is done by the next function.
input0.5 <- function(table_list){
  
  ## Pull data tables out of each list element (each list element corresponds to a raw datasheet).
  SI <- table_list$SI
  Zoop_counts <- table_list$Zoop_counts
  Zoop_lengths <- table_list$Zoop_lengths
  Zoop_eggs <- table_list$Zoop_eggs
  
  ## Start SQLite operations on temporary SQLite object zoop, created above.
  dbBegin(zoop)

    ## Create temporary sample info table in temporary SQLite object "zoop".
    dbWriteTable(zoop, "Zoop_si_temp", SI, overwrite=TRUE, temporary=TRUE)

    ## Add the relevant sample info to the permanent Zoop_sample_info table in LEPAS_2020.db.
    dbExecute(zoop, "INSERT INTO
                       Zoop_sample_info (Sample_ID,
                                         Sample_date,
                                         Sample_site,
                                         Water_body,
                                         Project,
                                         Depth_m,
                                         Surface_temp,
                                         Meter_1,
                                         Meter_2,
                                         Counter_name,
                                         Date_counted,
                                         Microscope_number,
                                         mm_per_ocular_unit,
                                         Subsampled_vol_mL,
                                         Diluted_vol_mL)
                     SELECT
                       Sample_ID,
                       Sample_date,
                       Sample_site,
                       Water_body,
                       Project,
                       Depth_m,
                       Surface_temp,
                       Meter_1,
                       Meter_2,
                       Counter_name,
                       Date_counted,
                       Microscope_number,
                       mm_per_ocular_unit,
                       Subsampled_vol_mL,
                       Diluted_vol_mL
                     FROM
                       Zoop_si_temp")
    
    ## Add the relevant sample info to the permanent Sample_inventory table in LEPAS_2020.db.
    dbExecute(zoop, "INSERT INTO
                       Sample_inventory (Sample_ID,
                                         Sample_date,
                                         Sample_time,
                                         Sample_site,
                                         Latitude,
                                         Longitude,
                                         Project,
                                         Net_diameter_m,
                                         Meter_1,
                                         Meter_2,
                                         Revolutions,
                                         Secchi_depth_m,
                                         Depth_m,
                                         Surface_temp,
                                         Bottom_temp,
                                         Ave_temp,
                                         Surface_DO,
                                         Bottom_DO,
                                         Comments)
                     SELECT
                       Sample_ID,
                       Sample_date,
                       Sample_time,
                       Sample_site,
                       Latitude,
                       Longitude,
                       Project,
                       Net_diameter_m,
                       Meter_1,
                       Meter_2,
                       Revolutions,
                       Secchi_depth_m,
                       Depth_m,
                       Surface_temp,
                       Bottom_temp,
                       Ave_temp,
                       Surface_DO,
                       Bottom_DO,
                       Comments
                     FROM
                       Zoop_si_temp")
    
    
    ## Create temporary counts table in SQLite object "zoop".
    dbWriteTable(zoop, "Zoop_counts_temp", Zoop_counts, overwrite = TRUE, temporary=TRUE)
    
    ## Add relevant count data to permanent Zoop_counts table in LEPAS_2020.db.
    dbExecute(zoop, "INSERT INTO
                       Zoop_counts (Sample_ID,
                                    Genus_sp_lifestage,
                                    Life_stage,
                                    Number_counted)
                     SELECT
                       Sample_ID,
                       Genus_sp_lifestage,
                       Life_stage,
                       Number_counted
                     FROM
                       Zoop_counts_temp")
    
    
    ## Create temporary lengths table in SQLite object "zoop".
    dbWriteTable(zoop, "Zoop_lengths_temp", Zoop_lengths, overwrite=TRUE, temporary=TRUE)
    
    ## Add the relevant length data to the permanent Zoop_lengths in LEPAS_2020.db.
    dbExecute(zoop, "INSERT INTO
                       Zoop_lengths (Sample_ID,
                                     Genus_sp_lifestage,
                                     Life_stage,
                                     Measured_number,
                                     Length_mm)
                     SELECT
                       Sample_ID,
                       Genus_sp_lifestage,
                       Life_stage,
                       Measured_number,
                       Length_mm
                     FROM
                       Zoop_lengths_temp")
    
    
    ## Create temporary eggs table in SQLite object "zoop".
    dbWriteTable(zoop, "Zoop_eggs_temp", Zoop_eggs, overwrite=TRUE, temporary=TRUE)
    
    ## Add the relevant egg count data to the permanent Zoop_egg_counts in LEPAS_2020.db.
    # NOTE: number of eggs is entered into the Length_OU column, but is recorded as a count.
    dbExecute(zoop, "INSERT INTO
                       Zoop_egg_counts (Sample_ID,
                                        Genus_sp_lifestage,
                                        Measured_number,
                                        Number_eggs)
                     SELECT
                       Sample_ID,
                       Genus_sp_lifestage,
                       Measured_number,
                       Length_OU
                     FROM
                       Zoop_eggs_temp")
    
  dbCommit(zoop)
  
  ## How many raw datasheets have been entered?
  counter2 <<- counter2+1

  ## Print messages indicating that all of the above tables have been updated in LEPAS_2020.db.
  list(print(paste0("Zoop_sample_info ", counter2, " entered into LEPAS_2020.db.")),
       print(paste0("Sample_inventory ", counter2, " entered into LEPAS_2020.db.")),
       print(paste0("Zoop_counts ", counter2, " entered into LEPAS_2020.db.")),
       print(paste0("Zoop_lengths ", counter2, " entered into LEPAS_2020.db.")),
       print(paste0("Zoop_egg_counts ", counter2, " entered into LEPAS_2020.db.")))

}

## Front-end function that applies input0.5 (above) to each element in table_list (one for each datasheet).
input1 <- function(x){
  counter2 <<- 0
  ifelse(x=="OK", sapply(table_list, input0.5), "Please check data and enter 'OK' into input1.")
} %>% invisible()

# input1("OK")

## This function combines data from SI, Zoop_counts, Zoop_lengths and Zoop_eggs to create the Zoop_final
## summary data.
step1.5 <- function(table_list){
  
  ## How many raw datasheets have been entered?
  counter3 <<- counter3+1
  
  ## Pull tables from table_list (each element in table_list corresponds to a raw datasheet).
  SI <- table_list$SI
  Zoop_counts <- table_list$Zoop_counts
  Zoop_lengths <- table_list$Zoop_lengths
  Zoop_eggs <- table_list$Zoop_eggs
  
  ## Call up Zoop_taxa_groups table from LEPAS_2020.db via the zoop object.
  TG <- suppressWarnings(dbGetQuery(zoop, "SELECT 
                                             Genus_sp_lifestage, 
                                             Life_stage,
                                             Biom_a, 
                                             Biom_b, 
                                             Biom_dens, 
                                             FR1_a, 
                                             FR1_b, 
                                             Pr_alpha, 
                                             Pr_beta, 
                                             Pr_rho 
                                           FROM 
                                             Zoop_taxa_groups"))
  
  ## Join the Genus_sp_lifestage-specific biomass and rate coefficients to table of measured lengths.
  FC <- left_join(Zoop_lengths, TG, by = c("Genus_sp_lifestage","Life_stage"))
  
  ## Call up taxonomic and life stage info from LEPAS_2020.db via zoop.
  LS <- suppressWarnings(dbGetQuery(zoop, "SELECT 
                                             Genus_sp_lifestage, 
                                             Phylum 
                                           FROM 
                                             Zoop_taxa_groups"))
  
  ## Calculate biomass, phosphorous excretion 'PE', nitrogen excretion 'NE', and filtration rate 'FR1' 
  ## for each measured individual. Coefficients are from the LEPAS manual.
  # Units for table 'FC2': Length_mm - millimeters, Zoop_biomass - micrograms, PE - micrograms Phosphorous per day.
  # NE - micrograms Nitrogen per day, FR1 - milliliters filtered per day.
  FC2 <- FC %>% 
    mutate(Ind_biomass = Biom_a * (Length_mm ^ Biom_b), 
           PE = 0.022387 * (Ind_biomass ^ 0.54), 
           NE = 0.041686938 * (Ind_biomass ^ 0.67), 
           FR1 = ifelse(Genus_sp_lifestage == "Dreissena_veligers", -0.0602 + (0.714 * as.numeric(Length_mm)), 
                        as.numeric(FR1_a) * (as.numeric(Length_mm) ^ as.numeric(FR1_b))))
  
  ## Calculate means by site, date, taxon, and life stage.
  by_site <- FC2 %>% 
    group_by(Sample_ID, Genus_sp_lifestage, Life_stage) %>% 
    summarise(mean_mass = mean(Ind_biomass, na.rm=T), 
              mean_PE = mean(PE), 
              mean_NE = mean(NE), 
              mean_FR1 = mean(FR1), 
              mean_length = mean(Length_mm, na.rm=T)) %>% 
    distinct()

  ## Left-join Zoop_counts to taxonomic and life stage data.
  ZCT <- left_join(Zoop_counts, LS, by = "Genus_sp_lifestage") %>% 
    distinct()
  
  ## Left-join counts/taxonomic/life stage data to site-specific mean rates. 
  ZF <- left_join(ZCT, by_site[,-1], by = c("Genus_sp_lifestage","Life_stage")) %>%
    distinct() %>% 
    mutate_at(.vars=vars(Number_counted,Zoop_density,mean_mass,mean_PE,mean_NE,mean_FR1,mean_length),
              .funs=list(~as.numeric(as.character(.)))) %>% 
    mutate(Zoop_biomass_perm3 = ifelse(is.na(mean_mass), 0, mean_mass * Zoop_density), 
           PE_dens = mean_PE * Zoop_density, 
           NE_dens = mean_NE * Zoop_density, 
           FR1_dens = mean_FR1 * Zoop_density) %>%
    ## Lef-join full taxonomic and rate coefficient data onto ZF. 
    left_join(TG, by = c("Genus_sp_lifestage","Life_stage")) %>%
    distinct() %>% 
    ## Calculate volume-specific biomasses, densities, rates, productivities, etc.
    mutate(Zoop_biomass = ifelse(!is.na(Biom_dens), Zoop_density * as.numeric(Biom_dens), Zoop_biomass_perm3),
           Zoop_prod = Pr_alpha * Zoop_biomass ^ Pr_beta * (Pr_rho ^ SI$Surface_temp)) %>% 
    # Change NAs to 0s for appropriate Genus_sp_lifestage/life_stages.
    mutate(Zoop_biomass = replace(Zoop_biomass, is.na(Zoop_biomass), 0),
           Zoop_prod = ifelse(Zoop_biomass==0, 0, Zoop_prod),
           PE_dens = ifelse(Zoop_biomass==0, 0, PE_dens),
           NE_dens = ifelse(Zoop_biomass==0, 0, NE_dens),
           FR1_dens = ifelse(Zoop_biomass==0, 0, FR1_dens),
           Zoop_density = ifelse(Life_stage=="Egg", NA, Zoop_density))

  ## Select only those columns to be added to the permanent Zoop_final table in LEPAS_2020.db.
  Zoop_final <<- ZF %>% 
    select(Sample_ID,Genus_sp_lifestage,Life_stage,Number_counted,Zoop_density,Zoop_biomass,
           PE_dens,NE_dens,FR1_dens,Zoop_prod,mean_length)

  ## Not currently used. Used to pop up Zoop_final as a tab in RStudio.
  # View(Zoop_final)
  
  ## Display message indicating that all of the above calculations succeeded. 
  paste0("step2 success ", counter3)
  
}

## This is only for use in Zoop_data_entry_2020.Rmd to simplify the front-end interface and avoid confusion.
X <- table_list

## Front-end function that applies step1.5 to each element of table_list (one for each raw datasheet).
step2 <- function(X){
  counter3 <<- 0
  lapply(table_list, step1.5)
}

# step2(X)

## This function creates the Zoop_final table within the "zoop" SQLite object and adds the data to the permanent 
## Zoop_final table in LEPAS_2020.db
input1.5 <- function(table_list){
  
  ## How many raw datasheets have been entered?
  counter3 <<- counter3+1
    
  ## Input zooplankton counts, density, biomass, and rates into the Zoop_final database table.
  dbBegin(zoop)
      
    ## Create temporary "final" table within zoop.
    dbWriteTable(zoop, "Zoop_final_temp", Zoop_final, overwrite=TRUE, temporary=TRUE)
    
    ## Add the relevant data to the permanent Zoop_final table in LEPAS_2020 - Copy.db.
    dbExecute(zoop, "INSERT INTO 
                       Zoop_final (Sample_ID, 
                                   Genus_sp_lifestage, 
                                   Life_stage,
                                   Number_counted, 
                                   Zoop_density, 
                                   Zoop_biomass, 
                                   Zoop_P_exc, 
                                   Zoop_N_exc, 
                                   Zoop_FR1, 
                                   Zoop_prod, 
                                   Zoop_len_avg)
                     SELECT 
                       Sample_ID, 
                       Genus_sp_lifestage, 
                       Life_stage,
                       Number_counted, 
                       Zoop_density, 
                       Zoop_biomass, 
                       PE_dens, 
                       NE_dens, 
                       FR1_dens, 
                       Zoop_prod, 
                       mean_length 
                     FROM 
                       Zoop_final_temp")
    
  dbCommit(zoop)
  
  ## Write a permanent copy of the raw datasheet to the "Entered" folder. 
  write.csv(data, paste0("Z:\\Projects\\FADX09_LEPAS\\15-LEPAS Datasheets\\Entered Spreadsheets\\3-Zooplankton\\LEPAS_", Year,"\\",Year,"-",Basin,"\\Entered\\",Files), na="")
  
  ## Not currently used. This would delete datasheets from the "NOT_entered" folder once they've been entered.
  # shell("Z:\\Projects\\FADX09_LEPAS\\15-LEPAS Datasheets\\Entered Spreadsheets\\3-Zooplankton\\
  #       LEPAS_2020\\2020-WB\\NOT_entered>del /f ",Files)
    
  ## Display message indicating that data from each raw datasheet have been entered into Zoop_final.
  print(paste0("Zoop_final ", counter3, " entered into LEPAS_2020.db."))
  
}

## Front-end function that applies input1.5 to each element of table_list (one for each raw datasheet)
input2 <- function(x){
  counter3 <<- 0
  ifelse(x=="OK", lapply(table_list, input1.5), "Please check data and enter 'OK' into input2.")
} %>% invisible()

# input2("OK")

