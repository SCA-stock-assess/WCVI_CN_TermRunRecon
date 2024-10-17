# termNIT 
# 01-terminal-catch
# oct 2024


# SET UP 
library(tidyverse)


# Helpers
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
NITmap <- readxl::read_excel(path=paste0(here::here("termNIT"), "/", analysis_year, "/", 
                                         list.files(path=paste0(here::here("termNIT"), "/", analysis_year),
                                                    pattern="^TERMNIT_mapping_[0-9]{4}\\.xlsx$",
                                                    full.names=F)),   
                             sheet="termNIT_map")



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

#                                                      SUMMARIZE/JOIN catch estimates + ages to calculate sample size flag

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
  mutate(n = case_when(is.na(n) ~ 0,
                       TRUE ~ n),
         month_sample_size = sum(n, na.rm=T),
         propn = n/month_sample_size,
         biosample_rate = case_when(MONTH=="June" ~ NA,
                                    MONTH %notin% "June" & monthly_catch_estimate==0 ~ 0,
                                    TRUE ~ month_sample_size/monthly_catch_estimate)) %>%
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


#  ========================= POOLING DESCISIONS =========================
# IDEAL RULES: 
#   If >2/3 months have a <10% sample rate FLAG, then pool all 3 months age samples together 
#   If 1/3 months has a <10% sample rate FLAG, pool it together with the month that has the next-lowest sample size, provided they are next to each other in time (e.g., do not pool June + Aug) - if not together in time, pool with the month next to it.
#   If no months have a <10% sample rate FLAG, no pooling! 


# Sample rate visualization -------------------------------
ggplot(data = NITrecCatchbyAge, 
       aes(x=MONTH, y=biosample_rate, fill=as.factor(YEAR), colour=as.factor(YEAR), label=as.factor(YEAR), group=as.factor(YEAR))) +
  geom_hline(yintercept=0.1, colour="gray60", linetype="dashed", size=1) +
  geom_point(size=3, alpha=0.5) +
  geomtextpath::geom_textline(linewidth=1.5, show.legend=F, hjust=0.38, text_smoothing=30, size=4, fontface=2, alpha=0.8) +
  #geom_text(aes(label=month_sample_size)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Sampling rate", x="", fill="Biosampling rate over \nmost recent 10 years:", colour="Biosampling rate over \nmost recent 10 years:") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        legend.position = c(0.1,0.8),
        legend.background = element_rect(colour="black"),
        legend.title = element_text(face='bold'))


# --> THERE HAS NEVER REALLY BEEN A CASE WHERE ANY GIVEN MONTH HAS A GOOD SAMPLE RATE FOR AREA 21/121.
#     THEREFORE, ages are pooled ACROSS ALL SUB-AREAS/MONTHS WITHIN A YEAR  


# Pooling samples based on rules above -------------------------------
NITrecCatchbyAge_pooled <- NITrecCatchbyAge %>%
  group_by(YEAR) %>%
  mutate(
    # --- POOL ITERATION 1: Combine June/July and August/September if any month's sample rate is < 10% OR the monthly sample size is <5  samples
    temporal_pool_it1 = case_when(biosample_rate > 0.1 & month_sample_size >= 5 ~ MONTH, 
                                  TRUE ~ NA)) %>% 
  mutate(temporal_pool_it1 = case_when(any(temporal_pool_it1=="July") & MONTH=="June" ~ "June",
                                       (MONTH=="July" & (biosample_rate < 0.1 | month_sample_size<5)) | 
                                         (MONTH=="June" & is.na(biosample_rate)) ~ "June, July",
                                       (MONTH=="August" & (biosample_rate < 0.1 | month_sample_size<5)) | 
                                         (MONTH=="September" & (biosample_rate < 0.1 | month_sample_size<5)) ~ "August, September",
                                       monthly_catch_estimate==0 ~ NA,
                                       TRUE ~ temporal_pool_it1)
  ) %>% 
  group_by(YEAR, temporal_pool_it1) %>% 
  # Calculate new pooled sample size resulting from pooling iteration 1: 
  mutate(pool_sample_size = sum(n, na.rm=T)) %>% 
  # Calculate the new total catch estimate for the pooling results from iteration 1, starting with pasting in July's catch alone (don't want June catch):
  mutate(pool_catch_estimate = case_when(MONTH=="July" ~ monthly_catch_estimate)) %>% 
  group_by(YEAR, temporal_pool_it1) %>% 
  # Now insert the pooled catch estimate for cases where Aug and Sept were pooled:
  mutate(pool_catch_estimate = case_when(MONTH%in%c("August", "September") ~ sum(unique(monthly_catch_estimate),na.rm=T),
                                         TRUE ~ pool_catch_estimate),
         # Calculate the new sample rate after pooling iteration 1: 
         pool_sample_rate = case_when(pool_catch_estimate==0 ~ NA,
                                      TRUE ~ pool_sample_size/pool_catch_estimate)) %>% 
  group_by(YEAR) %>%
  # --- POOL ITERATION 2: If, after iteration 1, sample rates are still < 10%, pool all months together for the entire year 
  mutate(temporal_pool_it2 = case_when((any(temporal_pool_it1=="June, July") & any(temporal_pool_it1=="August, September")) & 
                                          (pool_sample_rate < 0.1 | pool_sample_size<5 | is.na(pool_sample_rate)) ~
                                         paste(unique(temporal_pool_it1), collapse = ", "),
                                       TRUE ~ temporal_pool_it1)) %>% 
  ungroup() %>%
  print()



# Calculate new age composition ------------------------------- 
tt<- NITrecCatchbyAge_pooled %>% 
  group_by(YEAR, temporal_pool_it2, RESOLVED_AGE) %>% 
  summarize(n = sum(n, na.rm=T), monthly_catch_estimate=unique(monthly_catch_estimate), MONTH=unique(MONTH), 
            month_sample_size=unique(month_sample_size)) 


# ****** HERE NEXT DAY: FIGURE OUT HOW TO SUMMARIZE THE POOLED AGE COMPS TO JOIN TO MAPPING FILE. Remember, it doesn't haev to look EXACLTY like the old run 
#     recon files,but it does have to link the individual month to the pooling decision! 


########################################################################################################################################################

#                                                                        JOIN + Export

# ============================== JOIN rec catch, pooled ages to NITmapping file ==============================

NITmap03 <- left_join(NITmap,
                      #full_age_range,
                      NITrecCatchbyAge_pooled %>% 
                        group_by(YEAR, temporal_pool_it2, RESOLVED_AGE) %>% 
                        summarize(n = sum(n, na.rm=T), monthly_catch_estimate=unique(monthly_catch_estimate), MONTH=unique(MONTH), 
                                  month_sample_size=unique(month_sample_size)) %>% 
                        group_by(YEAR, temporal_pool_it2) %>% 
                        mutate(sample_size = sum(n, na.rm=T),
                               propn = case_when(sample_size==0 ~ 0,
                                                 TRUE ~ n/sample_size)) %>% 
                        #filter(!is.na(RESOLVED_AGE)) %>%
                        arrange(RESOLVED_AGE) %>%
                        pivot_wider(names_from = RESOLVED_AGE, values_from = c(n, propn), names_prefix = "age_") %>%
                        arrange(YEAR) %>%
                        filter(MONTH!="June") %>%     # Remove June samples from cases where they aren't required to pool with July:
                        rename(Enumeration = monthly_catch_estimate,
                               TermRun_AGEStemp = temporal_pool_it2) %>% 
                        mutate(TermRun_AGEStemp = "",
                               TermRun_AGESspat = "Broodstock, morts, other",
                               TermRun_AGESsex = case_when(Maturity.Class %in% c("Male", "Female", "Jack") ~ paste0("Broodstock, morts, other - ", Maturity.Class),
                                                           Maturity.Class=="Unsexed Adult" ~ "Broodstock, morts, other - Total",
                                                           ## ^^ If there were any "unknown" broodstock, this should be where they are accounted for (as "...Total") ^^
                                                           TRUE ~ "FLAG"),
                               TermRun_AGES_year = max(NITepro$`(R) RETURN YEAR`))
                      
                      
                        
                        
                        ,
                        
                        
                        
                        
                        
                        
                      by=c("TermRun_AGEStemp", "TermRun_AGESspat", "TermRun_AGESsex", "TermRun_AGES_year")) %>%
  
  
  
  
  mutate(Maturity.Class = coalesce(Maturity.Class.x, Maturity.Class.y),
         propn_age_2 = coalesce(propn_age_2.x, propn_age_2.y),
         propn_age_3 = coalesce(propn_age_3.x, propn_age_3.y),
         propn_age_4 = coalesce(propn_age_4.x, propn_age_4.y),
         propn_age_5 = coalesce(propn_age_5.x, propn_age_5.y),
         propn_age_6 = coalesce(propn_age_6.x, propn_age_6.y),
         .keep="unused") 

























# mutate(TermRun_sector01 = "Recreational",
#        TermRun_sector02 = "Area 21 Terminal") %>% 