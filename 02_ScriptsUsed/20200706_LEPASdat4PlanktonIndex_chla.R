####
# Code for getting data Plankton Index
# JMH  6 July 2021

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

Chl <- dbGetQuery(lpdb, "SELECT 
       PG.Sample_ID,
       PG.Sample_date,
       PG.Sample_site,
       PG.Chl_a_ugL_L
       FROM Pigments PG")


Chl2 <- Chl %>% 
  filter(Sample_site %in% c("27-918", "37-890", "36-873", "29-905", "16-970", "14-972", "8-994", "3-996",
                            "1279_20m", "1280_15m", "1281_5m", "1281_10m", "1318_20m", "1319_15m", "1320_10m", "1310_5m",
                            # not exactly core LEPAS, but fills in some gaps
                            "1320_5m", "1310_10m")) %>% 
  # These types shouldn't be included in the total Microcystis value. Verified by Danny after convo with ruth
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) 
  
# duplicates - nope
Chl2[duplicated(Chl2 %>% 
                  select(Sample_ID))]

ggplot(Chl2, aes(y = Chl_a_ugL_L, x = Sample_date)) +
  geom_point() +
  facet_wrap(vars(Sample_site), scales = "free_y")






write.csv(Chl2, file.path(here::here("03_exports","PlanktonIndexChla_20210706.csv")))
save.image(file.path(here::here("04_SavedRimages","20210706_LEPASdat4PlanktonIndex_chla_rdat")))
load(file.path(here::here("04_SavedRimages","20200603_LEPASdat4PlanktonIndex_rdat")))


