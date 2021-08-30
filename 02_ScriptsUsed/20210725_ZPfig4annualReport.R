
####
# Code for getting data for FY2021 annual report
# JMH 25 July 2021

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
                         "LEPAS_20210725.db")

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
                   ZF.Zoop_biomass
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

ZT2 <- dbGetQuery(lpdb, "SELECT 
                   ZT.Genus_sp_lifestage,
                   ZT.Genus_sp_95toPresent,
                   ZT.Order1
                   FROM Zoop_taxa_groups ZT") %>% 
  distinct()

ZT3 <- dbGetQuery(lpdb, "SELECT 
                   ZT.Genus_sp_95toPresent,
                   ZT.Order1
                   FROM Zoop_taxa_groups ZT") %>% 
  distinct()

SI <- dbGetQuery(lpdb, "SELECT
                 SI.Sample_ID,
                 SI.Sample_date,
                 SI.Sample_site,
                 SI.Latitude,
                 SI.Longitude,
                 SI.secchi_depth_m,
                 SI.Surface_temp,
                 SI.Bottom_temp,
                 SI.Surface_DO,
                 SI.Bottom_DO
                 FROM Sample_inventory SI")


ZP <- ZF %>% 
  left_join(ZS, by = "Sample_ID") %>% 
  left_join(ZT, by = "Genus_sp_lifestage") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # WB LEPAS sites sites
  filter(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996",
                            "1279_20m", "1280_15m", "1281_5m", "1281_10m", "1318_20m", "1319_15m", "1320_10m", "1310_5m",
                            # not exactly core LEPAS, but fills in some gaps
                            "1320_5m", "1310_10m")) 



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
  # WB LEPAS sites sites
  filter(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996")) %>% 
  mutate(V1toV2ratio = Vol_sampled_L2/Vol_sampled_L,
         V1equalsV2 = as.factor(ifelse(V1toV2ratio <= 0.99 |V1toV2ratio >= 1.01, "dif","same")))

# my calc is the same as what is in database after I change formula
# there are some of these that are off and that needs to be checked later
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
  select(Sample_ID:Zoop_biomass, Sample_date = "Sample_date.x", Sample_site = "Sample_site.x",
         Project:Order1,Meter_1:blah)


# CHECKS 
# this just compares density in database and hand calcualted good except.
# there's a line of weird samples with much lower densities than expected
ggplot(zfin2, aes(y = Zoop_density1a, x = Zoop_density)) + 
  geom_point() +
  # ylim(0,50) +
  geom_abline(intercept = 0, slope = 1, color = "red")

# just one of these
blahPos <- zfin2 %>% 
  filter(blah > 0.1)
unique(blahPos$Sample_ID)



# none of these
blahNeg <- zfin2 %>% 
  filter(blah < -0.1)
unique(blahNeg$Sample_ID)

ggplot(blahPos, aes(y = Zoop_density2, x = Zoop_density)) + 
  geom_point() +
  ylim(0,300) +
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
            Zoop_biomass = sum(Zoop_biomass, na.rm=T)) %>% 
  ungroup() %>% 
  # # drop Bytho because we don't trust data
  filter(across(Genus_sp_lifestage, ~ !grepl("B_longimanus",.))) %>%
  # # drop Cercopagis because we don't trust data
  filter(across(Genus_sp_lifestage, ~ !grepl("C_pengoi",.)))

#####
# 2) consistent names from 95 to present, also removes with/without eggs
####
ZP2 <- FTG1 %>% 
  left_join(ZT2, by = "Genus_sp_lifestage") %>% 
  #remove eggs
  filter(Life_stage2 != "Egg") %>% 
  group_by(Sample_ID, Life_stage2, Genus_sp_95toPresent, Sample_date, Sample_site, Order1) %>% 
  summarize(Number_counted = sum(Number_counted, na.rm=T),
            Zoop_density = sum(Zoop_density, na.rm=T),
            Zoop_biomass = sum(Zoop_biomass, na.rm=T)) 

# takes about 5 mins
ZPmonth <- ZP2 %>% 
  # Monthly average
  mutate(M = as.factor(as.character(strftime(Sample_date, format = "%m"))),
         Y = as.factor(as.character(strftime(Sample_date, format = "%Y")))) %>%  
  group_by(Y,M, Life_stage2, Sample_site, Order1, Genus_sp_95toPresent) %>% 
  summarise(across(c(Number_counted, Zoop_density, Zoop_biomass), mean, na.rm = T))

SI2 <- SI %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"),
         M = as.factor(as.character(strftime(Sample_date, format = "%m"))),
         Y = as.factor(as.character(strftime(Sample_date, format = "%Y")))) %>% 
  group_by(Y,M, Sample_site) %>% 
  summarise(Latitude = mean(Latitude, na.rm = TRUE),
            Longitude = mean(Longitude, na.rm = TRUE),
            Secchi_depth_m = mean(Secchi_depth_m, na.rm = T),
            Surface_temp = mean(Surface_temp, na.rm = T),
            Bottom_temp = mean(Bottom_temp, na.rm = T),
            Surface_DO = mean(Surface_DO, na.rm = T),
            Bottom_DO = mean(Bottom_DO, na.rm = T))

fin <- ZPmonth %>% 
  left_join(SI2, by = c("Y", "M", "Sample_site"))

finSum <- fin %>% 
  # drop eggs and veligers
  filter(Life_stage2 == "YO" | Life_stage2 == "Nauplii") %>% 
  # focus on copepods and cladocera
  filter(Order1 %in% c("Calanoida", "Cyclopoida", "Cladocera")) %>% 
  group_by(Y, M, Sample_site, Order1) %>% 
  summarise(across(Zoop_biomass, sum, na.rm = T)) %>% 
  pivot_wider(id_cols = c(Y, M, Sample_site), names_from = Order1, values_from = Zoop_biomass) %>% 
  mutate(TotCopCladBM = Calanoida + Cladocera + Cyclopoida)


# focus on the last 3 years and averaged by basin
finSumAfter2017 <- finSum %>% 
  filter(Y %in% c("2018", "2019", "2020")) %>% 
  mutate(basin = ifelse(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996"), "WB", "CB")) %>% 
  group_by(Y, M, basin) %>% 
  summarise(mean_se(TotCopCladBM)) %>% 
  mutate(Mn = as.numeric(as.character(M)),
         Mn = ifelse(Y== "2018", Mn,
                ifelse(Y == "2019", Mn + 0.1,
                  ifelse(Y == "2020", Mn + 0.2)))) %>% 
  mutate(basin = as.factor(basin),
          basin = fct_relevel(basin, c("CB", "WB")),
         basin = fct_recode(basin, "Central basin" = "CB", "Western basin" = "WB"))

  names(finSumAfter2017) <- c("Year","M","Basin", "TotCopCladBM", "TotCopCladBM_minSE", "TotCopCladBM_maxSE", "Mn")

png(file.path(here::here("05_plots","FY2021annualReportZPfig.png")),
      units = "in", height = 5, width = 7, res = 300)
ggplot(finSumAfter2017, aes(y = TotCopCladBM, x = Mn, color = Year)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = TotCopCladBM_minSE, ymax = TotCopCladBM_maxSE, x = Mn)) +
  geom_line(size = 0.75) +
  facet_wrap(vars(Basin)) +
  xlab("Month") +
  # ylab(expression(paste("Zooplankton biomass (µg dry mass ",L^-1,")"))) +
  ylab(expression(atop("Crustacean zooplankton biomass",
                       paste("(µg dry mass ",L^-1,")")))) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(color = "black", fill = "NA", size = 1),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.line = element_line(color = "black", size = 1),
        plot.background = element_rect(fill = "white", color =  "white"),
        # strip.background = element_blank(),
        strip.background = element_rect(fill = "transparent", color =  "transparent"),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = c(0.1, 0.80),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 14)) 
dev.off()

finSumAfter2017 %>% 
  filter(M == "07" | M == "08") %>% 
  group_by(Year, Basin) %>% 
  summarize(TotCopCladBM = mean(TotCopCladBM, na.rm = T))

# save.image(file.path(here::here("04_SavedRimages","20210725_ZPfig4annualReport_Rdat")))
# load(file.path(here::here("04_SavedRimages","20210725_ZPfig4annualReport_Rdat")))
