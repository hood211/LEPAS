# there was an error in the check in DOD code

# Datasheets with issues
# checked 2, 8, and 12 same issue
FileNames[c(2, 8, 12, 18, 23, 26)]


# this is what is in species_num
x <- sort(unique(Zoop_counts[Zoop_counts$Number_counted != 0,]$Genus_sp_lifestage))
x2 <- sort(unique(Zoop_counts[Zoop_counts$Number_counted != 0,]$Number_counted))

# and the contrast
# accounts for younger/older, unlike DOD code
y <- sort(unique(Zoop_counts[Zoop_counts$Number_counted != 0,]$Genus_sp_lifestage)) 
# Like DOD code, doesnt acount for younger/older
y1 <- sort(Zoop_counts[Zoop_counts$Number_counted != 0,]$Genus_sp_lifestage)
    # this will tell you whose extra
    y1.a <- as.data.frame(Zoop_counts[Zoop_counts$Number_counted != 0,]$Genus_sp_lifestage)
    y1.a$dups <- duplicated(Zoop_counts[Zoop_counts$Number_counted != 0,]$Genus_sp_lifestage) 
    y1.a %>% filter(dups == TRUE)
    Zoop_counts %>% filter(Genus_sp_lifestage == "M_edax_wo_eggs")

y2 <- sort(unique(Zoop_counts[Zoop_counts$Number_counted != 0,]$Number_counted))
y1[!(y1 %in% x)]


y2[!(y2 %in% x2)]


data[data$Number_counted == "45",]
D_thomasi_wo_eggs


Zoop_counts[Zoop_counts$Number_counted != 0 & Zoop_counts$Genus_sp_lifestage == "D_thomasi_wo_eggs",]
