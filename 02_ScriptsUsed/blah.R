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

zfin2 <- zfin %>% 
  left_join(zSmp, by = "Sample_ID") %>% 
  mutate(Sample_date = as.POSIXct(Sample_date, format = "%Y-%m-%d")) %>% 
  # remove projects that are not in Lake Erie or not supported by DOW
  filter(Project != "Reservoir") %>% 
  filter(Project != "Lake Guardian") %>% 
  # LEPAS was not supported by DOW prior to 2001
  filter(Sample_date > as.POSIXct("2001-01-01", format = "%Y-%m-%d")) %>% 
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

# none
zfinBdups <- zfinB[zfinB$dups == TRUE, ]
summary(zfinBdups, maxsum = 100)
# dups come from four smpIDS
# seems like some of these samples were uploaded 2x. I wonder what is missing??
blah <- zfinB[zfinB$Sample_ID == "20190524-1281_5m",]
# there there are also dups associated with 
zfinBdups[1:3,]

# These are going to have to be tracked down, but for now I will average them - best I can do
zfin3 <- zfin2 %>% 
  group_by(Sample_ID, Genus_sp_lifestage, Life_stage) %>%
  summarise(across(c(Number_counted, Zoop_density, Zoop_biomass),mean), na.rm = TRUE) 


zfin3dups <- zfin3 %>% 
  mutate(key = paste0(Sample_ID, "_", Genus_sp_lifestage, "_", Life_stage),
         dups = duplicated(key))

# that took care of this
zfin3dupsTRUE <- zfin3dups[zfin3dups$dups == TRUE, ]
