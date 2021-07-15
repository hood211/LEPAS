####
# Code for getting data Plankton Index
# JMH 03 June 2021, updated 6 July 2021

###
# Required libraries
###
library(tidyverse)
library(RSQLite)
library(here)

####
# Get data from database
####

# location of database
# database should be kept outside of repo
# I keep right outside of my github repo, find with getwd
# put in name of db
LEPASdbPath <- file.path(here::here("/Users/hood.211/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/FADX09/01_LEPASgitHub/"), 
                         "LEPAS_20210201.db")

# Connect to database -- change filepath as needed.
lpdb <- dbConnect(SQLite(), LEPASdbPath)

PF <- dbGetQuery(lpdb, "SELECT 
       PF.ID,
       PF.Sample_ID,
       PF.Genus_type,
       PF.Phyto_density,
       PF.Phyto_biomass
       FROM Phyto_final PF")

PS <- dbGetQuery(lpdb, "SELECT 
       PS.Sample_ID,
       PS.Sample_date,
       PS.Sample_site,
       PS.Water_body,
       PS.Project
       FROM Phyto_sample_info PS")

PT <- dbGetQuery(lpdb, "SELECT 
       PT.Genus_type,
       PT.Taxa_name,
       PT.Kingdom,
       PT.Phylum,
       PT.Class,
       PT.Order1,
       PT.Family,
       PT.Genus,
       PT.Type,
       PT.Edible
       FROM Phyto_taxa_groups PT")

SS <- dbGetQuery(lpdb, "SELECT 
       SS.Sample_site,
       SS.Latitude,
       SS.Longitude
        FROM Sampling_sites SS")


PP <- PF %>% 
  left_join(PS, by = "Sample_ID") %>% 
  left_join(PT, by = "Genus_type") %>% 
  left_join(SS, by = "Sample_site") %>% 
  select(-ID) %>% 
  filter(Project == "LEPAS") %>% 
  mutate(across(c(Longitude,Latitude), ~as.numeric(.))) %>% 
  filter(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996",
                            "1279_20m", "1280_15m", "1281_5m", "1281_10m", "1318_20m", "1319_15m", "1320_10m", "1310_5m",
                            # not exactly core LEPAS, but fills in some gaps
                            "1320_5m", "1310_10m")) %>% 
  # These types shouldn't be included in the total Microcystis value. Verified by Danny after convo with ruth
  filter(Genus_type != "Microcystis_sm_celled_by_cell",
         Genus_type != "Microcystis_lg_celled_by_col") %>% 
  mutate(across(c(Genus_type,Sample_site:Edible), ~as.factor(.))) %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) 
  
# duplicates - nope
blah[duplicated(PP %>% 
                  select(Sample_ID, Genus_type))]






# write.csv(PP, file.path(here::here("03_exports","PlanktonIndexPP_20210706.csv")))
# save.image(file.path(here::here("04_SavedRimages","20200603_LEPASdat4PlanktonIndex_rdat")))
load(file.path(here::here("04_SavedRimages","20200603_LEPASdat4PlanktonIndex_rdat")))


