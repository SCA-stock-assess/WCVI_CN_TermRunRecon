# termNIT 
# 01-terminal-catch
# oct 2024


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
NITmap <- readxl::read_excel(path=paste0(here::here("termNIT"), "/", analysis_year, "/", 
                                         list.files(path=paste0(here::here("termNIT"), "/", analysis_year),
                                                    pattern="^TERMNIT_mapping_[0-9]{4}\\.xlsx$",
                                                    full.names=F)),   
                             sheet="Sheet1")



# South Coast sport catch data -------------------------------
SCrecCatch <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/Salmon$/FMCR_Fishery_Monitoring_Catch_Reporting/Recreational_CM/Catch_Data/SC Sport Catch Creel Sub-area Disposition (Master Do No Edit).xlsx",
                                  sheet="YTD")



########################################################################################################################################################

#                                     CALCULATE CATCH ESTIMATE TO POPULATE COLUMN J "Enumeration" of TermNIT MAPPING FILE


# Area 21/22 terminal rec catch -------------------------------
  # Creel sub-area 21A + 121C (121C used to be 21B prior to 2022), July/Aug/Sept. 
  # No creel in Area 22 so manual estimate comes from Nitinat River Hatchery staff (if available)
NITrecCatch <- SCrecCatch %>%
  filter(SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept", MONTH%in%c("July", "August", "September"), 
         CREEL_SUB_AREA %in%c("21A", "21B", "121C")) %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T)) %>% 
  mutate(TermRun_sector01 = "Recreational",
         TermRun_sector02 = "Area 21 Terminal") %>% 
  print()


########################################################################################################################################################









