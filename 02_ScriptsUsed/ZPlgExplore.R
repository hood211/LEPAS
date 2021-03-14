#####
# Code for examining individual lengths
# JMH 14 Mar 2021

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

ZL <- dbGetQuery(lpdb, "SELECT 
       ZL.Sample_ID,
       ZL.Genus_sp_lifestage,
       ZL.Life_stage,
       ZL.Measured_number,
       ZL.Length_mm
        FROM Zoop_lengths ZL") %>% 
        # 3420 non unique rows
      distinct()

ZS <- dbGetQuery(lpdb, "SELECT 
       ZI.Sample_ID,
       ZI.Sample_date,
       ZI.Sample_site,
       ZI.Project,
       ZI.Counter_name
       FROM Zoop_sample_info ZI")

# this needs a primary key
ZT <- dbGetQuery(lpdb, "SELECT 
       ZT.Genus_sp_lifestage,
       ZT.Genus_sp_95toPresent,
       ZT.Life_stage,
       ZT.Has_eggs,
       ZT.Order1
       FROM Zoop_taxa_groups ZT") %>% 
      distinct()


ZP <- ZL %>% 
  left_join(ZS, by = "Sample_ID") %>% 
  # mutate(across(c(Genus_sp_lifestage,Life_stage,Project,Counter_name), factor)) %>% 
  left_join(ZT, by = c("Genus_sp_lifestage", "Life_stage")) %>% 
  mutate(Order1 = as.factor(Order1)) %>% 
  # typo in counter names
  mutate(Counter_name = ifelse(Counter_name == "CMD ", "CMD", Counter_name))

NotInOtherCounters <- c("CMD", "MK", "RDB")
ZP2 <- ZP %>% 
  mutate(Counter_name2 = as.factor(ifelse(Counter_name == "CMD", "CMD",
                                          # why aren't these people showing up
                                      ifelse(Counter_name == "MK", "MK",
                                        ifelse(Counter_name == "RDB", "RDB", "other"))))) %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d"))

CalaCops <- c("Leptodiaptomus minutus", "Skistodiaptomus oregonensis", "Leptodiaptomus ashlandi", "Leptodiaptomus sicilis")
ZPcal <- ZP2 %>% 
  filter(Order1 == "Calanoida") %>% 
  filter(Length_mm < 7)

pdf("CalanoidLengths.pdf")

for(i in 1:length(unique(ZPcal$Genus_sp_95toPresent))){
  # i = 1
  TAXA = unique(ZPcal$Genus_sp_95toPresent)[i]
  plot_i = ggplot(ZPcal %>% 
           filter(Genus_sp_95toPresent == TAXA), aes(y = Length_mm, x = Sample_date, color = Counter_name2, shape = as.factor(Has_eggs))) +
    geom_point() +
    ggtitle(TAXA)
  print(plot_i)
}
dev.off()

ZPcal2 <- ZPcal %>% 
  filter(Genus_sp_95toPresent %in% CalaCops) %>% 
  mutate(Counter_name3 = ifelse(Counter_name2 == "CMD", "CMD", "Other Counter")) %>% 
  mutate(After2016 = ifelse(Sample_date > as.POSIXct("2016-01-01"), "After2016", "Before2016"))

ggplot(ZPcal2, aes(y = Length_mm, x = Sample_date, fill = Counter_name3, shape = After2016)) +
  geom_point() +
  scale_fill_manual(values = c("CMD" = "blue", "Other Counter" = "transparent")) +
  scale_shape_manual(values = c(21,22))+
  facet_wrap(vars(Genus_sp_95toPresent))
