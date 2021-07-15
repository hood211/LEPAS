# re: public data request by Dupont and Monsanto
# Export all raw phyto, zooplankton, and temp/do data
# JMH
# 13 Mar 2021

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

blah <- dbGetQuery(lpdb, "SELECT
                   ZS.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZS.Project,
                   ZS.Latitude,
                   ZS.Longitude,
                   Secchi_depth_m,
                   Depth_m,
                   Ave_temp
                   FROM Sample_inventory ZS") %>% 
          filter(Project == "Reservoir") %>% 
          mutate(Sample_date = as.Date(Sample_date),
                 Sample_site = as.factor(Sample_site),
                 Project = as.factor(Project))


write.csv(blah, "Reservoir_ZP4Hannah.csv")
