# temporary run recon for SJ roundtable


# =================== SET UP ===================
# Load libraries -------------------------
library(tidyverse)

# Load data -------------------------
a20RR <- readxl::read_excel(path=here::here("data", "TermOTHER_Renfrew_data_extraction_Nov2024.xlsx"),
                   sheet="Sheet1") %>% 
  mutate(across(c(age2:age6), ~ round(., 0))) %>%
  mutate(Sector = case_when(Sector=="Recreational" ~ "Recreational catch",
                            Sector=="Escapement" ~ "Escapement (broodstock + natural spawners)",
                            Sector=="First Nation" ~ "Pacheedaht catch"),
         stock_ID = case_when(grepl("(Nitinat)|(Robertson)|(San Juan)|(Sarita)|(Conuma)|(Marble)", stock_ID, ignore.case=T) & 
                                !grepl("WCVI", stock_ID, ignore.case=T) ~ paste0(stock_ID, " (WCVI)"),
                              TRUE ~ stock_ID),
         stock_ID = gsub(stock_ID, pattern=" River", replacement=""),
         stock_ID = gsub(stock_ID, pattern=" \\(assumed\\)", replacement="")) 


########################################################################################################################################################

#                                                                         PLOTS

# =================== HIGH LEVEL SUMMARIES =================== 

# FIGURE: All sectors total Chinook -------------------------
a20RR$Sector <- factor(a20RR$Sector, levels=c("Recreational catch", "Escapement (broodstock + natural spawners)", "Pacheedaht catch", ordered=T))

pdf(file = here::here("termREN", "2023", "figures", 
                      paste0("temporary run recon all sectors.pdf")),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=a20RR %>% 
         pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "n") %>%
         mutate(age = str_sub(age, start=4, end=4)) %>%
         group_by(Year, Sector) %>%
         summarize(total=sum(n,na.rm=T)) %>%
         filter(Year%in%c(2020:2023), total>0)) +
  geom_bar(aes(x=Year, y=total, group=Sector, fill=Sector, colour=Sector), stat="identity", position="dodge", alpha=0.8, size=1, width=0.55) +
  scale_y_continuous(breaks=seq(0,8000, by=1000)) +
  labs(x="Return year", y="Number of Chinook (adults, all stocks)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=18),
        legend.text = element_text(colour="black", size=14),
        legend.title = element_text(face="bold", size=16),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.6)),
        legend.position = c(0.6,0.85))

dev.off()


# FIGURE: All sectors total SAN JUAN Chinook -------------------------
a20RR$Sector <- factor(a20RR$Sector, levels=c("Recreational catch", "Escapement (broodstock + natural spawners)", "Pacheedaht catch", ordered=T))

pdf(file = here::here("termREN", "2023", "figures", 
                      paste0("temporary run recon all sectors SJ.pdf")),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=a20RR %>% 
         filter() %>%
         pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "n") %>%
         mutate(age = str_sub(age, start=4, end=4)) %>%
         group_by(Year, Sector, stock_ID) %>%
         summarize(total = sum(n,na.rm=T)) %>%
         filter(Year%in%c(2020:2023), total>0) %>% 
         mutate(group = case_when(stock_ID=="unknown" ~ "(Gordon River escapement)",
                                  grepl("Fraser", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Fraser"),
                                  grepl("(SUS)|(Puget)|(Columbia)|(Washington)|(US)|(Non-Canadian)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Southern US"),
                                  grepl("(NEVI)|(ECVI)|(JDF_VIC)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " ECVI"),
                                  TRUE ~ stock_ID)) %>%
         group_by(Year, Sector, group) %>% 
         summarize(total2 = sum(total, na.rm=T)) %>%
         filter(grepl("san juan", group, ignore.case=T))) +
  geom_bar(aes(x=Year, y=total2, fill=group, linetype=Sector, group=Sector, alpha=group), stat="identity", position="dodge", colour="black") +
  scale_alpha_manual(values = c(0.3, 0.5), guide="none") +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  scale_y_continuous(breaks=seq(0,3000,by=500)) +
  labs(x="", y="Number of Chinook") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(face="bold", size=20),
        legend.text = element_text(colour="black", size=15),
        legend.title = element_blank(),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.8)),
        legend.position = c(0.2,0.8),
        #legend.direction = "horizontal",
        legend.key.size = unit(10, "mm")) +
  guides(linetype = guide_legend(override.aes = list(fill="transparent")))

dev.off()


# =================== STOCK COMP BREAKDOWNS =================== 

# FIGURE: Escapement composition (total) -------------------------
ggplot(data=a20RR %>% 
         filter(Sector=="Escapement (broodstock + natural spawners)") %>%
         pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "n") %>%
         mutate(age = str_sub(age, start=4, end=4)) %>%
         group_by(Year, stock_ID) %>%
         summarize(total=sum(n,na.rm=T)) %>%
         filter(Year%in%c(2020:2023), total>0)) +
  geom_bar(aes(x=Year, y=total, fill=stock_ID, colour=stock_ID), stat="identity", position="dodge") +
  theme_bw()




# FIGURE: Rec composition (by age) -------------------------
pdf(file = here::here("termREN", "2023", "figures", 
                      paste0("temporary run recon rec comp by age.pdf")),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=a20RR %>% 
         filter(Sector=="Recreational catch") %>%
         pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "n") %>%
         mutate(age = str_sub(age, start=4, end=4)) %>%
         group_by(Year, age, stock_ID) %>%
         summarize(total = sum(n,na.rm=T)) %>%
         filter(Year%in%c(2020:2023), total>0) %>% 
         mutate(group = case_when(grepl("Fraser", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Fraser"),
                                  grepl("(SUS)|(Puget)|(Columbia)|(Washington)|(US)|(Non-Canadian)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Southern US"),
                                  grepl("(NEVI)|(ECVI)|(JDF_VIC)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " ECVI"),
                                  TRUE ~ stock_ID)) %>%
         group_by(Year, age, group) %>% 
         summarize(total2 = sum(total, na.rm=T))) +
  geom_tile(aes(x=age, y=group, fill=total2, colour=total2), stat="identity", alpha=0.9)+
  scale_colour_viridis_b() +
  scale_fill_viridis_b() +
  labs(x="Total age", y="", fill="Number of kept,\nlegal Chinook\nterminal Area 20      ", colour="Number of kept,\nlegal Chinook\nterminal Area 20      ") +
  facet_wrap(~Year, nrow=1) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(face="bold", size=20),
        legend.text = element_text(colour="black", size=15),
        legend.title = element_text(face="bold", size=17),
        legend.background = element_rect(colour="white"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(15, "mm"),
        strip.text = element_text(size=16))

dev.off()




# FIGURE: Escapement composition (by age) -------------------------
pdf(file = here::here("termREN", "2023", "figures", 
                      paste0("temporary run recon escapement comp by age.pdf")),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=a20RR %>% 
         filter(Sector=="Escapement (broodstock + natural spawners)") %>%
         pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "n") %>%
         mutate(age = str_sub(age, start=4, end=4)) %>%
         group_by(Year, age, stock_ID) %>%
         summarize(total = sum(n,na.rm=T)) %>%
         filter(Year%in%c(2020:2023), total>0) %>% 
         mutate(group = case_when(grepl("Fraser", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Fraser"),
                                  grepl("(SUS)|(Puget)|(Columbia)|(Washington)|(US)|(Non-Canadian)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " Southern US"),
                                  grepl("(NEVI)|(ECVI)|(JDF_VIC)", stock_ID, ignore.case=T) ~ paste0(sub(" .*", "", stock_ID), " ECVI"),
                                  stock_ID=="unknown" ~ "(Gordon River)",
                                  TRUE ~ stock_ID)) %>%
         group_by(Year, age, group) %>% 
         summarize(total2 = sum(total, na.rm=T)) %>%
         filter(!grepl("gordon", group, ignore.case=T))) +
  geom_tile(aes(x=age, y=group, fill=as.numeric(total2), colour=as.numeric(total2)), stat="identity", alpha=0.9)+
  scale_colour_viridis_b() +
  scale_fill_viridis_b() +
  labs(x="Total age", y="", fill="Number of Chinook ", colour="Number of Chinook ") +
  facet_wrap(~Year, nrow=1) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(face="bold", size=20),
        legend.text = element_text(colour="black", size=15),
        legend.title = element_text(face="bold", size=17),
        legend.background = element_rect(colour="white"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(15, "mm"),
        strip.text = element_text(size=16))

dev.off()



#################

# =================== SAN JUAN ONLY =================== 

# FIGURE: ALL SECTORS SAN JUAN -----------------------
a20RR$Sector <- factor(a20RR$Sector, levels=c("Recreational catch", "Escapement (broodstock + natural spawners)", "Pacheedaht catch", ordered=T))

ggplot() +
  geom_bar(data=a20RR %>% 
             #filter(grepl("San Juan", stock_ID, ignore.case=T)) %>%
             pivot_longer(cols=c(age2:age6), names_to = "age", values_to = "count") %>%
             mutate(group = case_when(grepl("Natural san juan", stock_ID, ignore.case=T) ~ "Natural San Juan",
                                      grepl("Hatchery san juan", stock_ID, ignore.case=T) ~ "Hatchery San Juan",
                                      TRUE ~ stock_ID)) %>%
             group_by(Year, Sector, group) %>%
             summarize(n=sum(count,na.rm=T)),
           aes(x=Year, y=n, fill=group, colour=group, group=Sector), stat="identity", position = "dodge") +
  theme_bw()



