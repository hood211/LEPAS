#####
# Code for pulling data for FTG
# Originally DRO (11 Feb 20), revised JMH 24 Feb 21

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
zfin <- dbGetQuery(lpdb, "SELECT 
                   ZF.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZF.Genus_sp_lifestage,
                   ZF.Life_stage,
                   ZF.Number_counted,
                   ZF.Zoop_density,
                   ZF.Zoop_biomass,
                   ZF.Zoop_len_avg,
                   ZT.Order1,
                   ZS.Project,
                   ZS.Subsampled_vol_mL,
                   ZS.Diluted_vol_mL
                   FROM Zoop_final ZF
                   LEFT JOIN Zoop_sample_info ZS ON ZF.Sample_ID = ZS.Sample_ID
                   LEFT JOIN Zoop_taxa_groups ZT ON ZF.Genus_sp_lifestage = ZT.Genus_sp_lifestage
                   WHERE Sample_date LIKE '%2020%' AND Project='LEPAS'")

# LEFT JOIN Sample_inventory SI ON ZF.Sample_ID = SI.Sample_ID
# SI.Vol_sampled_L

# missing the density for eggs so have to calculate by hand
# get the volume sampled. Can't seem to do this in one query
# no sample volumes for 2020 FTG sites
zVolSmp <-  dbGetQuery(lpdb, "SELECT 
                   SI.Sample_ID,
                   SI.Sample_date,
                   SI.Sample_site,
                   SI.Meter_1,
                   SI.Meter_2,
                   SI.Meter_B1,
                   SI.Meter_B2,
                   SI.Vol_sampled_L
                   FROM Sample_inventory SI") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"),
         Vol_sampled_L2 = (Meter_2 - Meter_1)*5.2765) %>% #protocol says meter_1 - meter_2
  filter(Sample_date > as.POSIXct("2019-01-01", format = "%Y-%m-%d")) %>% 
  filter(Sample_site %in% c("1279_20m", "1281_10m", "27-918", "37-890")) 

# my calc is the same as what is in database after I change formula
ggplot(zVolSmp, aes(y = Vol_sampled_L2, x = Vol_sampled_L)) +geom_point()

zfin2 <- zfin %>% 
  left_join(zVolSmp, by = "Sample_ID") %>% 
  # Zoop density with egg densities too
  mutate(Zoop_density2 = (Number_counted*Diluted_vol_mL)/(Subsampled_vol_mL*Vol_sampled_L2))
  # swithc out Zoop_density with Zoop_density2
         
# CHECKS 
# these are the same
ggplot(zfin2, aes(y = Zoop_density2, x = Zoop_density)) + geom_point()

# and filled in 1752 NAs - Nice
dim(zfin2[zfin2$Life_stage == "Egg",])


# overwite zp density and remove sampling stuff
Zfin3 <- zfin2 %>% 
  mutate(Zoop_density = Zoop_density2) %>% 
  select(-c(Subsampled_vol_mL:Zoop_density2)) %>% 
  rename(Sample_date = "Sample_date.x", Sample_site = "Sample_site.x")
  

# NEED TO TAKE UP HERE AND TRANSFER THESE TO EGGS





############
# PROCESS AND SUMMARIZE DATA
############

############
# 1) aggregate across younger/older ( Genus_sp_lifestage and lifestage2 (w/o younger older))
############

FTG1 <- Zfin3 %>% 
  # turn NAs in density into zeros
  # mutate(Zoop_density = ifelse(is.na(Zoop_density), 0, Zoop_density)) %>% 
  
  # make all younger individuals immatures - year-to-year comparisons are not possible without this
  # Limnocalanus is the only copepod this doesn't apply too because identified to species as immature
  # Ultimately this places these younger individuals into "othercala" and othercyclo"
  mutate(Genus_sp_lifestage = ifelse(Order1=="Calanoida" & Life_stage=="Younger", "Calanoida_immature", Genus_sp_lifestage),
         Genus_sp_lifestage = ifelse(Order1=="Cyclopoida" & Life_stage=="Younger", "Cyclopidae_immature", Genus_sp_lifestage)) %>% 
  # make new lifestage with younger/older combined and Egg
  mutate(Life_stage2 = as.factor(ifelse(Life_stage == "Egg", Life_stage,
                                        ifelse(Life_stage == "Older", "YO",
                                               ifelse(Life_stage == "Younger", "YO", Life_stage))))) %>% 
  # THIS CORRECTS AN ERROR IN THE 2020 UPLOAD
  mutate(Genus_sp_lifestage = ifelse(Genus_sp_lifestage == "Ploesoma_sp", "Ploesoma_sp.",Genus_sp_lifestage)) %>% 
  mutate(across(c(Sample_ID, Sample_site,  Genus_sp_lifestage, Life_stage), factor)) %>% 
  # drop columns
  select( -Project, -Order1) %>%
  # select FTG sites
  filter(Sample_site %in% c("1279_20m", "1281_10m", "27-918", "37-890")) %>% 
  # true grouping variables are smp_id, and GSL
  # but we wat to keep smp_date and smp_site
  group_by(Sample_ID,  Genus_sp_lifestage, Sample_date, Sample_site, Life_stage2) %>%
  # In DRD's code, dups were generated by using mutate here, not summarize
  summarize(Number_counted = sum(Number_counted, na.rm=T),
            Zoop_density = sum(Zoop_density, na.rm=T),
            Zoop_biomass = sum(Zoop_biomass, na.rm=T),
            Zoop_len_avg = mean(Zoop_len_avg, na.rm=T)) %>% 
  ungroup() %>% 
  # # drop Bytho because we don't trust data
  filter(across(Genus_sp_lifestage, ~ !grepl("B_longimanus",.))) %>%
  # # drop Cercopagis because we don't trust data
  filter(across(Genus_sp_lifestage, ~ !grepl("C_pengoi",.)))



############
# aggregate to genus_sp (from year of counting) and lifestage2 (w/o younger older)
############

# get different taxonomy groups
# Doing the grouping at the genus_sp_95to present level
LEPASgenSp <- dbGetQuery(lpdb, "SELECT 
                  ZT.Genus_sp_lifestage,
                  ZT.Genus_sp_95toPresent,
                  biop_152_codes,
                  biop_152_names
                  FROM Zoop_taxa_groups ZT") %>% 
  # has some duplicate values due to younger/older which need to be removed
  distinct()

FTG2 <- FTG1 %>% 
  left_join(LEPASgenSp, by = "Genus_sp_lifestage") %>% 
  mutate(across(c(Genus_sp_95toPresent, biop_152_codes, biop_152_names), factor)) %>% 
  # correcting a typo in the biop codes
  mutate(biop_152_names = fct_recode(biop_152_names, "Cop eggs" = "Cop  eggs")) %>% 
  # grouping by Smp ID, ZZZ, and Life stage2, rest along for ride - removing Genus_sp_95toPresent &Life_stage2
  group_by(Sample_ID, biop_152_names, Sample_date, Sample_site, biop_152_codes)%>% 
  summarize(Number_counted = sum(Number_counted, na.rm=T),
            Zoop_density = sum(Zoop_density, na.rm=T),
            Zoop_biomass = sum(Zoop_biomass, na.rm=T),
            Zoop_len_avg = mean(Zoop_len_avg, na.rm=T)) %>%
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) 

# THIS INCLUDES BOSMINA AND EUBOSMINA. THOSE REALLY CAN NOT BE IDENTIFIED.


############
# check data
############

# look are these closely - is anything weird, if so chase down
# e.g, I found a typo in Genus_sp_lifestage because there were nas in the biop codes
summary(FTG2)


# are there dups
FTG2c <- FTG2 %>% 
  mutate(key = paste0(Sample_ID, "_", biop_152_names),
         dups = duplicated(key),
         Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"))

# none
FTG2c[FTG2c$dups == TRUE, ]


# summary by key
# not sure this is useful
# FTG2c %>%
#   split(.$key) %>%
#   map(summary)

# quick plot to check for weird data
ggplot(FTG2, aes(y = log(Zoop_density+1), x = Sample_date, color = Sample_site)) +
  geom_point() +
  facet_wrap(vars(biop_152_names), scales = "free_y")

ggplot(FTG2, aes(y = Zoop_density, x = Sample_date, color = Sample_site)) +
  geom_point() +
  facet_wrap(vars(biop_152_names), scales = "free_y")

ggplot(FTG2, aes(y = log(Zoop_biomass+1), x = Sample_date, color = Sample_site)) +
  geom_point() +
  facet_wrap(vars(biop_152_names), scales = "free_y")

ggplot(FTG2, aes(y = Zoop_biomass, x = Sample_date, color = Sample_site)) +
  geom_point() +
  facet_wrap(vars(biop_152_names), scales = "free_y")

# Write .csv file -- change filepath as needed.
write.csv(FTG2, file.path(here::here("03_exports","FTGdata_2020.csv")))

save.image(file.path(here::here("04_SavedRimages","FTG_exportAnnualData.R")))
