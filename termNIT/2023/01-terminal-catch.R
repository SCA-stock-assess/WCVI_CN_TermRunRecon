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


# WCVI rec catch biodata -------------------------------
SCrecBio <- readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R/",
                                             list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R/",
                                                        "^R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS [0-9]{4}-[0-9]{4}.xlsx$")),
                                 sheet="WCVI CN CREST Biodata CODED")


########################################################################################################################################################

#                                     CALCULATE CATCH ESTIMATE TO POPULATE COLUMN J "Enumeration" of TermNIT MAPPING FILE


# Area 21/22 terminal rec catch -------------------------------
  # Creel sub-area 21A + 121C (121C used to be 21B prior to 2022), July/Aug/Sept. 
  # No creel in Area 22 so manual estimate comes from Nitinat River Hatchery staff (if available)
NITrecCatch <- SCrecCatch %>%
  filter(SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept", MONTH%in%c("July", "August", "September"), 
         CREEL_SUB_AREA %in%c("21A", "21B", "121C", "Area 21")) %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T)) %>% 
  print()


########################################################################################################################################################

#                                                                  CALCULATE AGE SUMMARY by MONTH and YEAR

NITrecAges <- SCrecBio %>% 
  filter(SPECIES=="124", DISPOSITION=="Kept", SUBAREA%in%c("21A", "21B", "121C"), SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE)) %>% 
  group_by(YEAR, MONTH, RESOLVED_AGE) %>% 
  summarize(n = n()) %>% 
  group_by(YEAR, MONTH) %>% 
  mutate(month_sample_size = sum(n),
         propn = n/month_sample_size) %>%
  arrange(RESOLVED_AGE) %>%
  pivot_wider(names_from=RESOLVED_AGE, values_from=c(n, propn), names_prefix="age_") %>%
  print()
  

########################################################################################################################################################

#                                                      JOIN catch estimates + ages to calculate sample size flag

NITrecCatchbyAge <- full_join(NITrecCatch,
                              NITrecAges) %>%
  arrange(YEAR, MONTH) %>%
  mutate(month_sample_size = case_when(MONTH %in% c("July", "August", "September") & is.na(month_sample_size) & YEAR>=2017 ~ 0,
                                       TRUE ~ month_sample_size),
         biosample_size_flag = case_when(MONTH %in% c("July", "August", "September") & month_sample_size/monthly_catch_estimate < 0.1 ~ "FLAG",
                                         TRUE ~ "OK")) %>%
mutate(TermRun_sector01 = "Recreational",
       TermRun_sector02 = "Area 21 Terminal",
       MONTH = factor(MONTH, levels=month.name)) %>% 
  arrange(YEAR, MONTH) %>%
  print()


#### View age breakdown

ggplot() +
  geom_bar(data=NITrecCatchbyAge %>% 
             pivot_longer(cols=c(n_age_2:n_age_6, propn_age_2:propn_age_6), names_to = "metric", values_to = "n") %>% 
             filter(grepl("propn", metric)),
           aes(x=MONTH, y=n, group=metric, fill=metric, colour=metric), stat="identity") +
  geom_text(data=NITrecCatchbyAge %>% 
              pivot_longer(cols=c(n_age_2:n_age_6, propn_age_2:propn_age_6), names_to = "metric", values_to = "n") %>% 
              filter(grepl("n", metric)) %>% 
              group_by(YEAR, MONTH) %>% 
              summarize(sample_size = sum(n, na.rm=T)),
             aes(x=MONTH, y=1, label=sample_size)) +
  facet_wrap(~YEAR)


#### 

# Pool age samples for nearby months if sample rate <10% 









