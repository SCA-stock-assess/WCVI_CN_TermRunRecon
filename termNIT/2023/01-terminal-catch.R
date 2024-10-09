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
# NITrecCatch <- SCrecCatch %>%
#   filter(SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept", MONTH%in%c("July", "August", "September"), 
#          CREEL_SUB_AREA %in%c("21A", "21B", "121C", "Area 21")) %>% 
#   group_by(YEAR, MONTH) %>% 
#   summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T)) %>% 
#   print()


########################################################################################################################################################

#                                                                  CALCULATE AGE SUMMARY by MONTH and YEAR

# NITrecAges <- SCrecBio %>% 
#   filter(SPECIES=="124", DISPOSITION=="Kept", SUBAREA%in%c("21A", "21B", "121C"), SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE)) %>% 
#   group_by(YEAR, MONTH, RESOLVED_AGE) %>% 
#   summarize(n = n()) %>% 
#   group_by(YEAR, MONTH) %>% 
#   mutate(month_sample_size = sum(n),
#          propn = n/month_sample_size) %>%
#   arrange(RESOLVED_AGE) %>%
#   #pivot_wider(names_from=RESOLVED_AGE, values_from=c(n, propn), names_prefix="age_") %>%
#   print()
  

########################################################################################################################################################

#                                                      JOIN catch estimates + ages to calculate sample size flag

NITrecCatchbyAge <- full_join(SCrecCatch %>%
                                filter(SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept", MONTH%in%c("July", "August", "September"), 
                                       CREEL_SUB_AREA %in%c("21A", "21B", "121C", "Area 21")) %>% 
                                group_by(YEAR, MONTH) %>% 
                                summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T)),
                              SCrecBio %>% 
                                filter(SPECIES=="124", DISPOSITION=="Kept", SUBAREA%in%c("21A", "21B", "121C"), SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE)) %>% 
                                group_by(YEAR, MONTH, RESOLVED_AGE) %>% 
                                summarize(n = n())) %>%
  ungroup() %>% 
  mutate(MONTH = factor(MONTH, levels=month.name)) %>% 
  arrange(YEAR, MONTH, RESOLVED_AGE) %>%
  group_by(YEAR, MONTH) %>%
  mutate(month_sample_size = sum(n, na.rm=T),
         propn = n/month_sample_size,
         biosample_rate = month_sample_size/monthly_catch_estimate) %>%
  print()


# Do ages vary by month/year?
ggplot() +
  geom_bar(data=NITrecCatchbyAge,
           aes(x=MONTH, y=propn, group=as.factor(RESOLVED_AGE), fill=as.factor(RESOLVED_AGE), colour=as.factor(RESOLVED_AGE)), stat="identity") +
  geom_text(data=NITrecCatchbyAge %>% 
              group_by(YEAR, MONTH) %>% 
              summarize(month_sample_size=unique(month_sample_size)),
             aes(x=MONTH, y=1, label=month_sample_size)) +
  facet_wrap(~YEAR)
# Yes, mostly


########################################################################################################################################################

#                                                                 AGE SAMPLE POOLING

#  Pool age samples for nearby months if sample rate <10% ---------------------------------
# IDEAL RULES: 
#   If >2/3 months have a <10% sample rate FLAG, then pool all 3 months age samples together 
#   If 1/3 months has a <10% sample rate FLAG, pool it together with the month that has the next-lowest sample size, provided they are next to each other in time (e.g., do not pool June + Aug) - if not together in time, pool with the month next to it.
#   If no months have a <10% sample rate FLAG, no pooling! 


# Sample rate visualization: 
ggplot(data = NITrecCatchbyAge, 
       aes(x=MONTH, y=biosample_rate, fill=as.factor(YEAR), colour=as.factor(YEAR), label=as.factor(YEAR), group=as.factor(YEAR))) +
  geom_hline(yintercept=0.1, colour="gray60", linetype="dashed") +
  geom_point(size=3, alpha=0.5) +
  geomtextpath::geom_textline(linewidth=1, show.legend=F, hjust=0.38, text_smoothing=30, size=4, fontface=2) +
  theme_bw()


# THERE HAS NEVER REALLY BEEN A CASE WHERE ANY GIVEN MONTH HAS A GOOD SAMPLE RATE FOR AREA 21/121.
# THEREFORE, ages are pooled ACROSS ALL SUB-AREAS/MONTHS WITHIN A YEAR  


# Pooled ages ----------------------
NITrecCatchbyAge_pooled <- NITrecCatchbyAge %>% 
  group_by(YEAR) %>%
  mutate(n_age_2_pooled = sum(n_age_2, na.rm=T),
         n_age_3_pooled = sum(n_age_3, na.rm=T),
         n_age_4_pooled = sum(n_age_4, na.rm=T),
         n_age_5_pooled = sum(n_age_5, na.rm=T),
         n_age_6_pooled = sum(n_age_6, na.rm=T),
         propn_age_2_pooled = sum(n_age_2_pooled,na.rm=T)/sum(month_sample_size,na.rm=T))
  






























# mutate(TermRun_sector01 = "Recreational",
#        TermRun_sector02 = "Area 21 Terminal") %>% 