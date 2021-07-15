####
# Code for getting site summaries for Joe
# JMH 07 June 2021 

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

# doesn't seem to be reservoir PP
# unique(PS$Project)
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
  # filter(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996",
  #                           "1279_20m", "1280_15m", "1281_5m", "1281_10m", "1318_20m", "1319_15m", "1320_10m", "1310_5m",
  #                           # not exactly core LEPAS, but fills in some gaps
  #                           "1320_5m", "1310_10m")) %>% 
  mutate(across(c(Genus_type,Sample_site:Edible), ~as.factor(.))) %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"))

ggplot(PP, aes(y = Phyto_biomass, x = Sample_date)) +
  geom_point()+
  facet_grid(Phylum ~ Sample_site, scales = "free_y") +
  scale_y_log10()


PPlist <- PP %>% 
  select(Sample_ID, Sample_date, Sample_site, Latitude, Longitude) %>% 
  distinct() %>% 
  group_by(Sample_site, Latitude, Longitude) %>% 
  summarise(n = n(),
            FirstDate = min(Sample_date),
            LastDate = max(Sample_date))

write.csv(PPlist, file.path(here::here("03_exports","PPsiteCounts_20210606.csv")))





# Zooplankton
ZS <- dbGetQuery(lpdb, "SELECT 
                   ZS.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZS.Project
                   FROM Zoop_sample_info ZS")

ZT2 <- dbGetQuery(lpdb, "SELECT 
                   ZT.Genus_sp_lifestage,
                   ZT.Genus_sp_95toPresent,
                   ZT.Order1
                   FROM Zoop_taxa_groups ZT") %>% 
                    distinct()

SI <- dbGetQuery(lpdb, "SELECT
                 SI.Sample_ID,
                 SI.Latitude,
                 SI.Longitude
                 FROM Sample_inventory SI")


ZP <- SI %>% 
  left_join(ZS, by = "Sample_ID") 

ZPres <- ZP %>% 
  filter(Project == "Reservoir") %>% 
  select(Sample_ID, Sample_date, Sample_site, Latitude, Longitude) %>% 
  distinct() %>% 
  group_by(Sample_site, Latitude, Longitude) %>% 
  summarise(n = n(),
            FirstDate = min(Sample_date),
            LastDate = max(Sample_date))

ZPlepas <- ZP %>% 
  filter(Project == "LEPAS") %>% 
  select(Sample_ID, Sample_date, Sample_site, Latitude, Longitude) %>% 
  distinct() %>% 
  group_by(Sample_site, Latitude, Longitude) %>% 
  summarise(n = n(),
            FirstDate = min(Sample_date),
            LastDate = max(Sample_date))

write.csv(ZPres, file.path(here::here("03_exports","ZPResSiteCounts_20210607.csv")))
write.csv(ZPlepas, file.path(here::here("03_exports","ZPLepasSiteCounts_20210607.csv")))
write.csv(ZT2 %>% 
            select(-Genus_sp_lifestage), file.path(here::here("03_exports","ZPTaxaTable_20210607.csv")))
