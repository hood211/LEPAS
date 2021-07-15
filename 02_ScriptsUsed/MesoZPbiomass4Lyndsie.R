MesoZP <- c("Calanoida", "Cyclopoida", "Cladocera")

blah <- ZP3 %>% 
  filter(Order1 %in% MesoZP) %>% 
  group_by(Sample_ID, Sample_date, Sample_site) %>% 
  summarize(Zoop_density = sum(Zoop_density, na.rm = TRUE)) %>% 
  mutate(DOY = as.numeric(strftime(Sample_date, format = "%j")),
         Y= strftime(Sample_date, format = "%Y")) %>% 
  filter(Sample_site == "37-890" | Sample_site == "36-873")

hist(blah$Zoop_density)

ggplot(blah, aes(x = DOY, y = Zoop_density, color= Y)) +
  geom_point() +
  facet_wrap(vars(Sample_site)) +
  geom_hline(yintercept = 5/2.3, color = "red")


# what if we remove the immatures

MesoZPimm <- c("Calanoida spp. nauplii", "Cyclopoida spp. nauplii", "Calanoida spp.", "Cyclopoida spp.")
blah2 <- ZP3 %>% 
  filter(Order1 %in% MesoZP) %>% 
  filter(!(Genus_sp_95toPresent %in% MesoZPimm)) %>% 
  group_by(Sample_ID, Sample_date, Sample_site) %>% 
  summarize(Zoop_density = sum(Zoop_density, na.rm = TRUE)) %>% 
  mutate(DOY = as.numeric(strftime(Sample_date, format = "%j")),
         Y= strftime(Sample_date, format = "%Y")) %>% 
  filter(Sample_site == "36-873")

ggplot(blah2 %>% 
         filter(DOY > 150 & DOY < 200), aes(x = DOY, y = Zoop_density, color= Y)) +
  geom_line() +
  # facet_wrap(vars(Sample_site)) +
  facet_grid(Y ~ Sample_site, scales = "free_y")+
  geom_hline(yintercept = 5/2.3, color = "red")
