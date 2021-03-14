#####
# Code for pulling data for FTG
# JMH 24 Feb 21

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
                   ZF.Number_counted,
                   ZF.Zoop_density,
                   ZF.Zoop_biomass,
                   ZF.Zoop_len_avg
                   FROM Zoop_final ZF") 

ZS <- dbGetQuery(lpdb, "SELECT 
                   ZS.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZS.Project,
                   ZS.Subsampled_vol_mL,
                   ZS.Diluted_vol_mL
                   FROM Zoop_sample_info ZS")

ZT <- dbGetQuery(lpdb, "SELECT 
                   ZT.Genus_sp_lifestage,
                   ZT.Order1
                   FROM Zoop_taxa_groups ZT")


ZP <- ZF %>% 
  left_join(ZS, by = "Sample_ID") %>% 
  left_join(ZT, by = "Genus_sp_lifestage") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # FTG dataseet starts in 2001
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
  # FTG sites
  filter(Sample_site %in% c("1279_20m", "1281_10m", "27-918", "37-890")) 



# missing the density for eggs in 2019 and 2020 so have to calculate by hand
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
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # also 20090713 samples had the wrong meter readings -  so meter readings are 0, 661 for site 1279_20m and 0, 401 for 1281_10m 
  mutate(Meter_1 = ifelse(Sample_ID == "20090713-1281_10m" | Sample_ID == "20090713-1279_20m",0, Meter_1),
         Meter_2 = ifelse(Sample_ID == "20090713-1281_10m", 401, Meter_2),
         Meter_2 = ifelse(Sample_ID == "20090713-1279_20m", 661, Meter_2)) %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"),
         Vol_sampled_L2 = (Meter_2 - Meter_1)*5.2765) %>% #protocol says meter_1 - meter_2
  # FTG years
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
  # FTG sites
  filter(Sample_site %in% c("1279_20m", "1281_10m", "27-918", "37-890")) %>% 
  mutate(V1toV2ratio = Vol_sampled_L2/Vol_sampled_L,
         V1equalsV2 = as.factor(ifelse(V1toV2ratio <= 0.99 |V1toV2ratio >= 1.01, "dif","same")))

# my calc is the same as what is in database after I change formula
ggplot(zVolSmp, aes(y = Vol_sampled_L2, x = Vol_sampled_L)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,25000) +
  ylim(0,25000)

ggplot(zVolSmp %>% 
         filter(V1equalsV2 == "dif"), aes(y = Vol_sampled_L2, x = Vol_sampled_L, label = Sample_ID)) +
  geom_text() +
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,25000) +
  ylim(0,25000)



# zVolSmp[zVolSmp$V1equalsV2 == "dif",]
# 20070829-1281_10m - use Vsampled_L
# 20090713-1279_20m - use Vsampled_L
# 20150604-1281_10m - use Vsampled_L2

SwitchV2toV1 = c("20070829-1281_10m", "20090713-1279_20m", "20090713-1281_10m")

# I fixed density for this, but don't have the biomass to fix these
DropBiomass4These = c("20070829-1281_10m", "20090713-1279_20m", "20090713-1281_10m")

# fix volumes
zVolSmp2 <- zVolSmp %>% 
  mutate(Vol_sampled_L2 = ifelse(Sample_ID %in% SwitchV2toV1, Vol_sampled_L, Vol_sampled_L2))


zfin2 <- ZP %>% 
  left_join(zVolSmp2, by = "Sample_ID") %>% 
  # the database should be corrected with these cathy got these from counter sheets
  mutate(Subsampled_vol_mL = ifelse(Sample_ID == "20090713-1279_20m", 8, Subsampled_vol_mL),
         Subsampled_vol_mL = ifelse(Sample_ID == "20090713-1281_10m", 8, Subsampled_vol_mL),
         Diluted_vol_mL = ifelse(Sample_ID == "20090713-1279_20m", 3300, Diluted_vol_mL),
         Diluted_vol_mL = ifelse(Sample_ID == "20090713-1281_10m", 3300, Diluted_vol_mL)) %>% 
  # Zoop density with egg densities too
  # 2019/2020 is missing egg densities
  mutate(Zoop_density1a = (Number_counted*Diluted_vol_mL)/(Subsampled_vol_mL*Vol_sampled_L),
          Zoop_density2 = (Number_counted*Diluted_vol_mL)/(Subsampled_vol_mL*Vol_sampled_L2),
         blah = Zoop_density2 - Zoop_density) %>% 
  select(Sample_ID:Zoop_len_avg, Sample_date = "Sample_date.x", Sample_site = "Sample_site.x",
         Project:Order1,Meter_1:blah)


# CHECKS 
# this just compares density in database and hand calcualted good except.
# there's a line of weird samples with much lower densities than expected
ggplot(zfin2, aes(y = Zoop_density1a, x = Zoop_density)) + 
  geom_point() +
  # ylim(0,50) +
  geom_abline(intercept = 0, slope = 1, color = "red")

# 42 of these associated with 2 samples: 3 Jun 2004 37-890 and 29 Aug 2007 1281_10m
blahPos <- zfin2 %>% 
  filter(blah > 0.1)
unique(blahPos$Sample_ID)


# these are the two with the wrong meter readings
blahNeg <- zfin2 %>% 
  filter(blah < -0.1)
unique(blahNeg$Sample_ID)

ggplot(blahPos, aes(y = Zoop_density2, x = Zoop_density)) + 
  geom_point() +
  ylim(0,300) +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(blahNeg, aes(y = Zoop_density1a, x = Zoop_density)) + 
  geom_point() +
  # ylim(0,300) +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(blahNeg, aes(y = Zoop_density2, x = Zoop_density)) + 
  geom_point() +
  # ylim(0,300) +
  geom_abline(intercept = 0, slope = 1, color = "red")

# write.csv(rbind(blahPos, blahNeg), "WeirdDensityDays.csv")
       


# overwite zp density and remove sampling stuff
Zfin3 <- zfin2 %>% 
  # use new density data
  mutate(Zoop_density = Zoop_density2) %>% 
  #remove sampling stuff
  select(Sample_ID:Project, Order1) %>% 
  # had to remove these because I don't have bandwidth to fix these biomasses
  mutate(Zoop_biomass = ifelse(Sample_ID %in% DropBiomass4These, as.numeric("NA"), Zoop_biomass))
  
summary(Zfin3)

# Looks ok
ggplot(Zfin3, aes(y = Zoop_density, x = Sample_date, color = Sample_site)) +
  geom_point() 

ggplot(Zfin3, aes(y = Zoop_biomass, x = Sample_date, color = Sample_site)) +
  geom_point() 

############
# PROCESS AND SUMMARIZE DATA
############

############
# 1) aggregate across younger/older ( Genus_sp_lifestage and lifestage2 (w/o younger older))
############

FTG1 <- Zfin3 %>% 
  # make all younger individuals immatures - year-to-year comparisons are not possible without this
  # Limnocalanus is the only copepod this doesn't apply too because identified to species as immature
  # Ultimately this places these younger individuals into "othercala" and othercyclo"
  # this puts diaptomid and temoridae immature into the Calanoida immature
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
  distinct() %>% 
  # This was an error in the database, which doesn't really affect the biop codes, but should be addressed
  mutate(Genus_sp_95toPresent = ifelse(Genus_sp_lifestage == "E_phaleratus_eggs", "Ectocyclops",
                                  ifelse(Genus_sp_lifestage == "E_phaleratus_w_eggs", "Ectocyclops",
                                    ifelse(Genus_sp_lifestage == "E_phaleratus_wo_eggs", "Ectocyclops",Genus_sp_lifestage)))) %>% 
  # also making a biop codes that are consistent across time
  mutate(biop_152_names95toPres = biop_152_names,
         # diap immature and Temoridae immature were created in ~ 2014
         # diaptomid and temoridae stuff is book keeping because the reapportionment removes them
         biop_152_names95toPres = ifelse(biop_152_names95toPres == "Diaptomidae.immature", "othercala",
                                         ifelse(biop_152_names95toPres == "Temoridae.immature", "othercala", biop_152_names95toPres))) %>% 
  # also the Bosminidae can't really be identified to species
  mutate(biop_152_names95toPres = ifelse(biop_152_names95toPres == "Eub.Coreg" | biop_152_names95toPres == "Bo.Longer", "Bosminidae", biop_152_names95toPres))

# write.csv(LEPASgenSp, "LEPASgenSp.csv")


FTG2 <- FTG1 %>% 
  left_join(LEPASgenSp, by = "Genus_sp_lifestage") %>% 
  mutate_at(vars(Sample_ID, Genus_sp_lifestage, Genus_sp_95toPresent, biop_152_codes, biop_152_names, biop_152_names95toPres), factor) %>% 
  # correcting a typo in the biop codes
  mutate(biop_152_names = fct_recode(biop_152_names, "Cop eggs" = "Cop  eggs")) %>% 
  # grouping by Smp ID, ZZZ, and Life stage2, rest along for ride - removing Genus_sp_95toPresent &Life_stage2
  # note that not grouping by biop_152_names95toPres, its just there.
  group_by(Sample_ID, biop_152_names, Sample_date, Sample_site, biop_152_codes) %>% 
  summarize(Number_counted = sum(Number_counted, na.rm=T),
            Zoop_density = sum(Zoop_density, na.rm=T),
            Zoop_biomass = sum(Zoop_biomass, na.rm=T),
            Zoop_len_avg = mean(Zoop_len_avg, na.rm=T)) %>%
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) 




############
# check data
############

# look are these closely - is anything weird, if so chase down
# e.g, I found a typo in Genus_sp_lifestage because there were nas in the biop codes
summary(FTG2, maxsum = 100)


# are there dups
FTG2c <- FTG2 %>% 
  mutate(key = paste0(Sample_ID, "_", biop_152_names),
         dups = duplicated(key),
         Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"))

# none
FTG2c[FTG2c$dups == TRUE, ]

# adding in the biop_names that are consistent
FTG3 <- FTG2 %>% 
  left_join(LEPASgenSp %>% 
              select(biop_152_names, biop_152_names95toPres) %>% 
              # duplicates
              distinct(), by = "biop_152_names")



# quick plot to check for weird data

FTG4 <- FTG3 %>% 
  mutate(Y = as.numeric(as.character(strftime(Sample_date, format = "%Y"))))

pdf("FTGcheckPlots.pdf", height = 8, width = 13)
for(i in 1:length(unique(FTG4$Y))){
  # i = 1
  YEAR = unique(FTG4$Y)[i]
  plot_i <- ggplot(FTG4 %>% 
           filter(Y == YEAR), aes(y = Zoop_biomass, x = Sample_date, color = Sample_site)) +
    geom_point() +
    facet_wrap(vars(biop_152_names), scales = "free_y") +
    ggtitle(YEAR)
  print(plot_i)
}
dev.off()

pdf("FTGcheckPlotsDensity.pdf", height = 8, width = 13)
for(i in 1:length(unique(FTG4$Y))){
  # i = 1
  YEAR = unique(FTG4$Y)[i]
  plot_i <- ggplot(FTG4 %>% 
                     filter(Y == YEAR), aes(y = Zoop_density, x = Sample_date, color = Sample_site)) +
    geom_point() +
    facet_wrap(vars(biop_152_names), scales = "free_y") +
    ggtitle(YEAR)
  print(plot_i)
}
dev.off()



# Write .csv file -- change filepath as needed.
write.csv(FTG3, file.path(here::here("03_exports","FTGdata_01to20.csv")))

save.image(file.path(here::here("04_SavedRimages","FTGdata_01to20.R")))
# load(file.path(here::here("04_SavedRimages","FTGdata_01to20.R")))
