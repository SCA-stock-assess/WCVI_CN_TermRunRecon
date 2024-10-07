# termNIT 
# 03-escapement
# aug 2024


# SET UP 
library(tidyverse)


# Helpers
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
fecundity_at_age <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6),
                           fecundity = c(0,3000,3500,4000,4000),
                           Maturity.Class = "Female")
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
NITmap02 <- readxl::read_excel(path=paste0(here::here("termNIT"), "/", analysis_year, "/", 
                                           list.files(path=paste0(here::here("termNIT"), "/", analysis_year),
                                                                  pattern="R_OUT - TERMNIT_mapping_[0-9]{4}-output_from_02\\.xlsx$",
                                                      full.names=F)),   
                             sheet="Sheet1")



# Read EPRO data -------------------------------
NITepro <- readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R/", 
                                          list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
                                                     pattern="^R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS*")),
                              sheet="AllFacilities w RESULTS")




############################################################################################################################################################

#                                                              CALCULATE BROODSTOCK AGES 


# 1. Age summary for return year of interest 
    # Return year of interest is assumed to be the max year in the EPRO file (line ~55)

NITage_summary <- full_join(NITepro %>% 
                              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & 
                                       `(R) RESOLVED TOTAL AGE`%in%c(1:6)) %>% 
                              group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
                              summarize(n=n()) %>%
                              ungroup(),
                            full_age_range) %>%
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  group_by(Maturity.Class) %>%
  mutate(propn = case_when(Maturity.Class %in% c("Male","Female", "Jack") ~ n/sum(n,na.rm=T),
                           Maturity.Class=="Unsexed Adult" ~ 9999999999999999)) %>%
  mutate(TermRun_AGEStemp = "Broodstock, morts, other",
         TermRun_AGESspat = "Broodstock, morts, other",
         TermRun_AGESsex = case_when(Maturity.Class %in% c("Male", "Female", "Jack") ~ paste0("Broodstock, morts, other - ", Maturity.Class),
                                     Maturity.Class=="Unsexed Adult" ~ "Broodstock, morts, other - Total",
                                     TRUE ~ "FLAG"),
         TermRun_AGES_year = max(NITepro$`(R) RETURN YEAR`)) %>%
  pivot_wider(names_from=`(R) RESOLVED TOTAL AGE`, values_from=c(n, propn), names_prefix="age ") %>%
  print()
  


############################################################################################################################################################

#                                                            Join mapping file to AGE summary


# JOIN to mapping file
tt <- left_join(NITmap,
                NITage_summary,
                by=c("TermRun_AGEStemp", "TermRun_AGESspat", "TermRun_AGESsex", "TermRun_AGES_year"))





