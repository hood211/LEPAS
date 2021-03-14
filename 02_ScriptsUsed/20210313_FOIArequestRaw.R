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

# ZOOPLANKTON DATA
zfin <- dbGetQuery(lpdb, "SELECT 
                   ZF.Sample_ID,
                   ZF.Genus_sp_lifestage,
                   ZF.Life_stage,
                   ZF.Number_counted,
                   ZF.Zoop_density,
                   ZF.Zoop_biomass
                   FROM Zoop_final ZF") 

zSmp <- dbGetQuery(lpdb, "SELECT
                   ZS.Sample_ID,
                   ZS.Sample_date,
                   ZS.Sample_site,
                   ZS.Project
                   FROM Zoop_sample_info ZS")

zTax <- dbGetQuery(lpdb, "SELECT
                    ZT.Genus_sp_lifestage,
                    ZT.Genus_sp_95toPresent
                    FROM Zoop_taxa_groups ZT") %>% 
  #there are 26 duplicated lines in this
  unique()



zfin2 <- zfin %>% 
  left_join(zSmp, by = "Sample_ID") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # remove projects that are not in Lake Erie or not supported by DOW
  filter(Project != "Reservoir") %>% 
  filter(Project != "Lake Guardian") %>% 
  # LEPAS was not supported by DOW prior to 2001
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
  filter(Sample_date < as.POSIXct("2018-01-01", format = "%Y-%m-%d")) %>% 
  # fix a typo
  mutate(Project = ifelse(Project == "FADR 75", "FADR75", Project)) %>% 
  droplevels() %>% 
  mutate(across(Sample_ID:Life_stage, factor))


levels(as.factor(zfin2$Project))
summary(zfin2, maxsum = 100)


# are there dups
zfinB <- zfin2 %>% 
  mutate(key = as.factor(paste0(Sample_ID, "_", Genus_sp_lifestage, "_", Life_stage)),
         dups = duplicated(key),
         Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"))

# 42
zfinBdups <- zfinB[zfinB$dups == TRUE, ]
summary(zfinBdups, maxsum = 100)
# dups come from four smpIDS
# No idea what is going on here
blah <- zfinB[zfinB$Sample_ID == "20190524-1281_5m",]
# there there are also dups associated with 
zfinBdups[1:3,]

# These are going to have to be tracked down, but for now I will average them - best I can do
zfin3 <- zfin2 %>% 
  group_by(Sample_ID, Sample_date, Sample_site, Genus_sp_lifestage, Life_stage) %>%
  summarise(across(c(Number_counted, Zoop_density, Zoop_biomass),mean), na.rm = TRUE) 


zfin3dups <- zfin3 %>% 
  mutate(key = paste0(Sample_ID, "_", Genus_sp_lifestage, "_", Life_stage),
         dups = duplicated(key))

# that took care of this
zfin3dupsTRUE <- zfin3dups[zfin3dups$dups == TRUE, ]

# not helpful unless huge
# ggplot(zfin3, aes(y = log(Zoop_density+1), x = Sample_date)) +
#   geom_point() +
#   facet_wrap(vars(Genus_sp_lifestage), scales = "free_y")

zfin4 <- zfin3 %>% 
  left_join(zTax, by = "Genus_sp_lifestage") %>% 
  select(-na.rm)

# write csv file
write.csv(zfin3, file.path(here::here("03_exports","DOWzoopData_raw.csv")))

# PHYTOPLANKTON

PPdat <- dbGetQuery(lpdb, "SELECT
                  PF.Sample_ID,
                  PF.Genus_type,
                  PF.Phyto_density,
                  PF.Phyto_biomass
                  FROM Phyto_final PF")


PPtax <- dbGetQuery(lpdb, "SELECT
                  PTG.Genus_type,
                  PTG.Taxa_name,
                  PTG.Phylum,
                  PTG.Genus
                 FROM Phyto_taxa_groups PTG")

PPsmp <- dbGetQuery(lpdb, "SELECT
                  PS.Sample_ID,
                  PS.Sample_date,
                  PS.Sample_site,
                  PS.Project
                  FROM Phyto_sample_info PS")


PP <- PPdat %>% 
  left_join(PPsmp, by = "Sample_ID") %>% 
  left_join(PPtax, by = "Genus_type")%>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # LEPAS was not supported by DOW prior to 2001
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
  filter(Sample_date < as.POSIXct("2018-01-01", format = "%Y-%m-%d"))

summary(PP)
levels(as.factor(PP$Project))
levels(as.factor(PP$Sample_site))
levels(as.factor(PP$Genus_type))

# are there dups
PP2 <- PP %>% 
  mutate(key = paste0(Sample_ID, "_", Genus_type),
         dups = duplicated(key),
         Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  select(-key, -dups)

# none
PP2dups <- PP2[PP2$dups == TRUE, ]

write.csv(PP2, file.path(here::here("03_exports","DOWphytoData_raw.csv")))

#######
# Temp and DO
#######
TmpDO <- dbGetQuery(lpdb, "SELECT
                    SI.Sample_ID,
                    SI.Sample_date,
                    SI.Sample_site,
                    SI.Latitude,
                    SI.Longitude,
                    SI.Project,
                    SI.Surface_temp,
                    SI.Bottom_temp,
                    SI.Surface_DO,
                    SI.Bottom_DO
                    FROM Sample_inventory SI")%>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # include only projects that are not in Lake Erie and supported by DOW
  filter(Project %in% c("FADR 75", "FADR75", "LEPAS")) %>% 
  # LEPAS was not supported by DOW prior to 2001
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
  filter(Sample_date < as.POSIXct("2018-01-01", format = "%Y-%m-%d")) %>% 
  # fix a typo
  mutate(Project = ifelse(Project == "FADR 75", "FADR75", Project)) %>% 
  droplevels() %>% 
  # there are zero temps at times when that makes no sense, likely NAs - going to fix this
  mutate(Surface_temp = ifelse(Surface_temp == 0, as.numeric("NA"), Surface_temp),
         Bottom_temp = ifelse(Bottom_temp == 0, as.numeric("NA"), Bottom_temp))

levels(as.factor(TmpDO$Project))
levels(as.factor(TmpDO$Sample_site))

summary(TmpDO)

# there are some zero values in the temp column
ggplot(TmpDO, aes(y = Surface_temp, x = Sample_date)) +
  geom_point()
ggplot(TmpDO, aes(y = Bottom_temp, x = Sample_date)) +
  geom_point()

ggplot(TmpDO, aes(y = Surface_DO, x = Sample_date)) +
  geom_point()
ggplot(TmpDO, aes(y = Bottom_DO, x = Sample_date)) +
  geom_point()


write.csv(TmpDO, file.path(here::here("03_exports","DOWtemp_do_Data_raw.csv")))

# save image
save.image(file.path(here::here("04_SavedRimages","20210313_FOIArequest.R")))
