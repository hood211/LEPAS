







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
# also getting stuff to calculate density of eggs
# I do the joining in R because I trust the results more. 
ZF <- dbGetQuery(lpdb, "SELECT 
                   ZF.Sample_ID,
                   ZF.Genus_sp_lifestage,
                   ZF.Life_stage,
                   ZF.Zoop_density,
                   ZF.Zoop_biomass
                   FROM Zoop_final ZF") 

ZS <- dbGetQuery(lpdb, "SELECT 
                   ZS.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZS.Project
                   FROM Zoop_sample_info ZS")

ZT <- dbGetQuery(lpdb, "SELECT 
                   ZT.Genus_sp_lifestage,
                   ZT.Genus,
                   ZT.Family,
                   ZT.Order1
                   FROM Zoop_taxa_groups ZT")


ZP <- ZF %>% 
  left_join(ZS, by = "Sample_ID") %>% 
  left_join(ZT, by = "Genus_sp_lifestage") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  #pulling out eggs
  filter(Life_stage != "Egg") %>% 
  #pulling out nauplii
  filter(Life_stage != "Nauplii") %>% 
  # filter(!is.na(Zoop_len_avg)) %>% 
  filter(Project == "LEPAS") %>% 
  mutate(ZoopMeanBiomass = Zoop_biomass/Zoop_density) %>% 
  # remove NAs
  filter(!is.na(ZoopMeanBiomass)) %>% 
  # remove zeros
  filter(ZoopMeanBiomass >0) %>% 
  select(-Zoop_density, -Zoop_biomass)

ggplot(ZP, aes(y = ZoopMeanBiomass, x = Sample_date)) +
  geom_point() +
  facet_wrap(vars(Genus), scales = "free_y")

# TOTAL ZP
# aggregate by sample ID and then all samples
ZPtot <- ZP %>% 
  group_by(Sample_ID) %>% 
  summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
  ungroup()%>% 
  summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
  mutate(Group = "Total Zooplankton") %>% 
  mutate(GroupCat = "TotZP") %>% 
  select(Group, ZoopMeanBiomass, GroupCat)
  
  

# MAJOR GROUPS
# Cladocera
# Copepod
# Velifers
# Calanoid
# Cyclopoid
# Byth
ZPmg <- ZP %>% 
  mutate(Order2 = as.factor(ifelse(grepl("B_longimanus",Genus_sp_lifestage), "Bythotrephes", 
                                ifelse(Order1 == "Ploima", "Rotifer",
                                   ifelse(Order1 == "Rotifera spp.", "Rotifer", Order1))))) %>% 
  group_by(Sample_ID, Order2) %>% 
    summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(Order2) %>% 
    summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
  rename(Group = "Order2") %>% 
  mutate(GroupCat = "MajorGroup") %>% 
  filter(Group != "Poecilostomatoida")

# MINOR GROUPS
# CerioDaphnia
# Chydoridae
# Bosmina (split)
# Daphnia
# Diaphanosoma
# Rotiers
Zming <- ZP %>% 
  mutate(MinorGroup = as.factor(ifelse(grepl("Ceriodaphnia", Genus_sp_lifestage), "Ceriodaphnia", 
                                  ifelse(grepl("Chydorids", Genus_sp_lifestage), "Chydorids",
                                    ifelse(grepl("B_longirostris", Genus_sp_lifestage), "Bosmina",
                                        ifelse(grepl("B_coregoni", Genus_sp_lifestage), "Eubosmina",
                                           ifelse(Genus == "Daphnia", "Daphnia",
                                              ifelse(Genus == "Diaphanosoma", "Diaphanosoma",
                                                  ifelse(Order1 == "Ploima", "Rotifer", 
                                                      ifelse(Genus == "Leptodora", "Leptodora",
                                                          ifelse(Order1 == "Rotifera spp.", "Rotifer","OTHER"))))))))))) %>% 
  group_by(Sample_ID, MinorGroup) %>% 
  summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(MinorGroup) %>% 
  summarise(ZoopMeanBiomass = mean(ZoopMeanBiomass, na.rm = TRUE)) %>% 
  rename(Group = "MinorGroup") %>% 
  mutate(GroupCat = "MinorGroup") %>% 
  filter(Group != "OTHER")

ZPlgs <- rbind(ZPtot, ZPmg,Zming) 

write.csv(ZPlgs, file.path(here::here("03_exports", "ZoopMasses.csv")))
# save.image(file.path(here::here("04_SavedRimages","ZoopMasses_Rdat")))
