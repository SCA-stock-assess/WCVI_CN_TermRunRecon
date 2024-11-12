# termREN
# 03-escapement
# aug 2024


# ============================= SET UP  ============================
# Load high-use packages -------------------------------
library(tidyverse)

# Helpers -------------------------------
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
fecundity_at_age <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6),
                           fecundity = c(0,3000,3500,4000,4000),
                           Sex = "Female")
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
RENmap02 <- readxl::read_excel(path=paste0(here::here("termREN"), "/", analysis_year, "/", 
                                           list.files(path=paste0(here::here("termREN"), "/", analysis_year),
                                                                  pattern="^R_OUT - TERMREN_mapping_[0-9]{4}-output_from_02\\.xlsx$",
                                                      full.names=F)),   
                             sheet="Sheet1")



# Read Hatchery data joined to results -------------------------------
RENhatch <- readxl::read_excel(path=here::here("outputs", list.files(pat=here::here("outputs"),
                                                                     pattern="^R_OUT - WCVI_Escapement-FSC_BioData_\\d{4}-\\d{4}_WithResults_\\d{4}-\\d{2}-\\d{2}\\.xlsx$",
                                                                     full.names=F)),
                               sheet = "Esc biodata w RESULTS") %>%
  mutate(Sex = case_match(Sex, 
                          "M" ~ "Male",
                          "F" ~ "Female",
                          "J" ~ "Jack"))




############################################################################################################################################################

#                                                              CALCULATE BROODSTOCK AGES 


# 1. Broodstock age summary for return year of interest 
    # Return year of interest is assumed to be the year indicated in the termNIT mapping file

NIT_broodstock_ages <- full_join(NITepro %>% 
                              filter(`(R) RETURN YEAR` %in% NITmap02$TermRun_Year & 
                                       grepl("Nitinat R Fall Chinook", Spawning.Stock) & 
                                       `(R) RESOLVED TOTAL AGE`%in%c(1:6)) %>% 
                              group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
                              summarize(n=n()) %>%
                              ungroup(),
                            full_age_range) %>%
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  group_by(Maturity.Class) %>%
  mutate(propn = case_when(Maturity.Class %in% c("Male", "Female", "Jack") ~ n/sum(n,na.rm=T),
                           Maturity.Class=="Unsexed Adult" ~ 9999999999999999)) %>%
  mutate(TermRun_AGEStemp = "Broodstock, morts, other",
         TermRun_AGESspat = "Broodstock, morts, other",
         TermRun_AGESsex = case_when(Maturity.Class %in% c("Male", "Female", "Jack") ~ paste0("Broodstock, morts, other - ", Maturity.Class),
                                     Maturity.Class=="Unsexed Adult" ~ "Broodstock, morts, other - Total",
                                     ## ^^ If there were any "unknown" broodstock, this should be where they are accounted for (as "...Total") ^^
                                     TRUE ~ "FLAG"),
         TermRun_AGES_year = max(NITepro$`(R) RETURN YEAR`)) %>%
  pivot_wider(names_from=`(R) RESOLVED TOTAL AGE`, values_from=c(n, propn), names_prefix="age_") %>%
  print()
  


############################################################################################################################################################

#                                                                       Join + Export


# ============================== JOIN Broodstock ages to NITmapping file ==============================

NITmap03 <- left_join(NITmap02,
                      NIT_broodstock_ages,
                      by=c("TermRun_AGEStemp", "TermRun_AGESspat", "TermRun_AGESsex", "TermRun_AGES_year")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(Maturity.Class = coalesce(Maturity.Class.x, Maturity.Class.y),
         propn_age_2 = coalesce(propn_age_2.x, propn_age_2.y),
         propn_age_3 = coalesce(propn_age_3.x, propn_age_3.y),
         propn_age_4 = coalesce(propn_age_4.x, propn_age_4.y),
         propn_age_5 = coalesce(propn_age_5.x, propn_age_5.y),
         propn_age_6 = coalesce(propn_age_6.x, propn_age_6.y),
         .keep="unused") 


# ============================== EXPORT ==============================
# To github repo ---------------------------
writexl::write_xlsx(NITmap03, 
                    path=paste0(here::here("termNIT"), "/", analysis_year, 
                                "/R_OUT - TERMNIT_mapping_",
                                analysis_year,
                                "-output_from_03",
                                ".xlsx"))



