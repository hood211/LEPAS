# re: public data request by Dupont and Monsanto
# Taking Danny's summaries and removing non-DOW data
# JMH
# 13 Mar 2021

# Library
library(tidyverse)
library(here)

# Get data
# ZP
ZPfilepath <- file.path(here::here("/Users/hood.211/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/FADX09/01_LEPASgitHub/Monsanto_DupontFOIA/Zoops"))
zpCBsp <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_CentralB_Spring_Updated_20201014.csv"))
zpCBes <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_CentralB_EarlySummer_Updated_20201014.csv"))
zpCBls <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_CentralB_LateSummer_Updated_20201014.csv"))
zpWBsp <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_WesternB_Spring_Updated_20201014.csv"))
zpWBes <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_WesternB_EarlySummer_Updated_20201014.csv"))
zpWBls <- read.csv(here(ZPfilepath, "LEPAS_Zooplankton_WesternB_LateSummer_Updated_20201014.csv"))

# combine
ZP <- rbind(zpCBsp, zpCBes, zpCBls, zpWBsp, zpWBes, zpWBls) %>% 
  # remove data not supported by DOW
  filter(Year >= 2001 & Year <= 2017) %>% 
  select(-c(X, MaxTemp, Deg_day, Cum_deg_day, Site_consolidated))

write.csv(ZP, file.path(here::here("03_exports","DOWzoopSummaryData.csv")))

# PP
PPfilepath <- file.path(here::here("/Users/hood.211/Dropbox/JMH_dropbox/stephanson2/projects/6_Research/FADX09/01_LEPASgitHub/Monsanto_DupontFOIA/Phytos"))
ppCBsp <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_CentralB_Spring_Updated_20201208.csv"))
ppCBes <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_CentralB_EarlySummer_Updated_20201208.csv"))
ppCBls <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_CentralB_LateSummer_Updated_20201208.csv"))
ppWBsp <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_WesternB_Spring_Updated_20201208.csv"))
ppWBes <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_WesternB_EarlySummer_Updated_20201208.csv"))
ppWBls <- read.csv(here(PPfilepath, "LEPAS_Phytoplankton_WesternB_LateSummer_Updated_20201208.csv"))

PP <- rbind(ppCBsp, ppCBes, ppCBls, ppWBsp, ppWBes, ppWBls) %>% 
  filter(Year >= 2001 & Year <= 2017) %>% 
  select(-X)

write.csv(PP, file.path(here::here("03_exports","DOWphytoSummaryData.csv")))


save.image(file.path(here::here("04_SavedRimages","20210313_FOIArequestSummary_Rdat")))
