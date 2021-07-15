#####
# Code for pulling data for FTG
# JMH 1 Jun 2021

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

# Pull relevant columns from tables using embedded SQL code.
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
                   PT.Edible,
                   PT.Conversion_um_per_cell,
                   PT.Volumetric_equation
                   FROM Phyto_taxa_groups PT") 

# Write .csv file
write.csv(PT, file.path(here::here("03_exports","Phyto_taxa_groupsTable.csv")))


