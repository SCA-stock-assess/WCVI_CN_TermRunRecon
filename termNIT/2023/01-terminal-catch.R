# termNIT 
# 01-terminal-catch
# oct 2024


# ============================= SET UP ============================= 
# Load high-use packages -------------------------------
library(tidyverse)

# Helpers -------------------------------
full_age_range <- tibble(RESOLVED_AGE = c(2:6)) 
full_month_range <- tibble(MONTH=c("June", "July", "August", "September"))
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
NITmap <- readxl::read_excel(path=paste0(here::here("termNIT"), "/", analysis_year, "/", 
                                         list.files(path=paste0(here::here("termNIT"), "/", analysis_year),
                                                    pattern="^TERMNIT_mapping_[0-9]{4}\\.xlsx$",
                                                    full.names=F)),   
                             sheet="termNIT_map", skip=1)



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

# Joining catch and age data, calculating sample rate -------------------------------
NITrecCatchbyAge <- full_join(SCrecCatch %>%
                                filter(SPECIES%in%c("CHINOOK SALMON", "BOAT TRIPS"), DISPOSITION%in%c("Kept", "Effort"), MONTH%in%c("July", "August", "September"), 
                                       CREEL_SUB_AREA %in%c("21A", "21B", "121C", "Area 21")) %>% 
                                group_by(YEAR) %>% 
                                mutate(subareas_catch = paste0(unique(CREEL_SUB_AREA), collapse=", ")) %>%
                                group_by(YEAR, MONTH, SPECIES) %>% 
                                summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T), subareas_catch=unique(subareas_catch)) %>% 
                                pivot_wider(names_from = SPECIES, values_from = monthly_catch_estimate) %>% 
                                mutate(`CHINOOK SALMON` = case_when((!is.na(`BOAT TRIPS`) | `BOAT TRIPS`>0) & is.na(`CHINOOK SALMON`) ~ 0,
                                                                    TRUE ~ `CHINOOK SALMON`)) %>% 
                                pivot_longer(cols=c(`CHINOOK SALMON`, `BOAT TRIPS`), names_to = "SPECIES", values_to = "monthly_catch_estimate") %>% 
                                filter(SPECIES=="CHINOOK SALMON") %>%
                                select(-c(SPECIES)),
                              SCrecBio %>% 
                                filter(SPECIES=="124", DISPOSITION=="Kept", SUBAREA%in%c("21A", "21B", "121C"), SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE)) %>% 
                                mutate(subareas_sample = paste0(unique(SUBAREA), collapse=", ")) %>%
                                group_by(YEAR, MONTH, RESOLVED_AGE) %>% 
                                summarize(n = n(), subareas_sample=unique(subareas_sample)) %>% 
                                full_join(., full_month_range) %>%
                                full_join(., full_age_range) %>% 
                                ungroup() %>%
                                complete(YEAR, MONTH, RESOLVED_AGE, fill=list(n=0)) %>% 
                                filter(!is.na(RESOLVED_AGE) & !is.na(YEAR)) %>%
                                group_by(YEAR, MONTH) %>% 
                                fill(subareas_sample, .direction = "updown") %>% 
                                mutate(subareas_sample = case_when(is.na(subareas_sample) ~ "no samples",
                                                                   TRUE ~ subareas_sample))
                              ) %>%
  ungroup() %>% 
  mutate(subareas = case_when(subareas_sample==subareas_catch ~ subareas_sample,
                              subareas_sample!=subareas_catch ~ select(., subareas_sample, subareas_catch) %>% reduce(stringr::str_c, sep=", "),
                              is.na(subareas_sample) | is.na(subareas_catch) ~ coalesce(subareas_sample, subareas_catch))) %>% 
  select(-c(subareas_catch, subareas_sample)) %>%
  #complete(YEAR, MONTH, RESOLVED_AGE, fill=list(n=0)) %>%
  #filter(!is.na(RESOLVED_AGE)) %>% 
  group_by(YEAR, MONTH) %>%
  #fill(c(monthly_catch_estimate, subareas_catch), .direction="updown") %>%
  mutate(MONTH = factor(MONTH, levels=month.name)) %>%
  arrange(YEAR, MONTH, RESOLVED_AGE) %>% 
  ungroup() %>%
  arrange(YEAR, MONTH, RESOLVED_AGE) %>%
  group_by(YEAR, MONTH) %>%
  mutate(n = case_when(is.na(n) ~ 0,
                       TRUE ~ n),
         month_sample_size = sum(n, na.rm=T),
         propn = case_when(month_sample_size==0 ~ 0,
                           TRUE ~ n/month_sample_size),
         biosample_rate = case_when(MONTH=="June" ~ NA,
                                    MONTH %notin% "June" & (monthly_catch_estimate==0 | month_sample_size==0) ~ 0,
                                    TRUE ~ month_sample_size/monthly_catch_estimate)) %>%
  print()


# Do ages vary by month/year? -------------------------------
pdf(file = here::here("termNIT", "2023", "figures", 
                      paste0("Recreational fishery age composition ", 
                             min(NITrecCatchbyAge$YEAR), "-", max(NITrecCatchbyAge$YEAR), 
                             " (Terminal Nitinat areas).pdf")),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data=NITrecCatchbyAge %>% 
             filter(propn>0),
           aes(x=plyr::mapvalues(MONTH, from=month.name, to=month.abb), y=propn, 
               fill=as.factor(RESOLVED_AGE), colour=as.factor(RESOLVED_AGE)), stat="identity", alpha=0.8) +
  geom_text(data=NITrecCatchbyAge %>% 
              group_by(YEAR, MONTH) %>% 
              summarize(month_sample_size=unique(month_sample_size)),
             aes(x=plyr::mapvalues(MONTH, from=month.name, to=month.abb), y=1, label=month_sample_size)) +
  labs(x="", y="Age composition (%)", fill="Age: ", colour="Age: ") +
  facet_wrap(~YEAR) +
  theme_bw()   +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.spacing.x = unit(5, "mm"),
        legend.title = element_text(face="bold"))

# Yes, mostly

dev.off()

########################################################################################################################################################

#                                                                 AGE SAMPLE POOLING


#  ========================= POOLING DESCISIONS =========================
# IDEAL RULES: 
#   If >2/3 months have a <10% sample rate FLAG, then pool all 3 months age samples together 
#   If 1/3 months has a <10% sample rate FLAG, pool it together with the month that has the next-lowest sample size, provided they are next to each other in time (e.g., do not pool June + Aug) - if not together in time, pool with the month next to it.
#   If no months have a <10% sample rate FLAG, no pooling! 


# Sample rate visualization -------------------------------
pdf(file = here::here("termNIT", "2023", "figures", 
                      paste0("Recreational fishery sample rate ", 
                             min(NITrecCatchbyAge$YEAR), "-", max(NITrecCatchbyAge$YEAR), 
                             " (Terminal Nitinat areas).pdf")),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data = NITrecCatchbyAge, 
       aes(x=MONTH, y=biosample_rate, fill=as.factor(YEAR), colour=as.factor(YEAR), label=as.factor(YEAR), group=as.factor(YEAR))) +
  geom_hline(yintercept=0.1, colour="gray60", linetype="dashed", size=1) +
  geom_point(size=3, alpha=0.5) +
  geomtextpath::geom_textline(linewidth=1, show.legend=F, hjust=0.38, text_smoothing=30, size=4, fontface=2, alpha=0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Sampling rate", x="", fill="Biosampling rate over \nmost recent 10 years:", colour="Biosampling rate over \nmost recent 10 years:") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        legend.position = "none")

dev.off()

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
  mutate(pool_sample_size1 = sum(n, na.rm=T)) %>% 
  # Calculate the new total catch estimate for the pooling results from iteration 1, starting with pasting in July's catch alone (don't want June catch):
  mutate(pool_catch_estimate1 = case_when(MONTH=="July" ~ monthly_catch_estimate)) %>% 
  group_by(YEAR, temporal_pool_it1) %>% 
  # Now insert the pooled catch estimate for cases where Aug and Sept were pooled:
  mutate(pool_catch_estimate1 = case_when(MONTH%in%c("August", "September") ~ sum(unique(monthly_catch_estimate),na.rm=T),
                                         TRUE ~ pool_catch_estimate1),
         # Calculate the new sample rate after pooling iteration 1: 
         pool_sample_rate1 = case_when(pool_catch_estimate1==0 ~ NA,
                                      TRUE ~ pool_sample_size1/pool_catch_estimate1)) %>% 
  group_by(YEAR) %>%
  # --- POOL ITERATION 2: If, after iteration 1, sample rates are still < 10%, pool all months together for the entire year 
  mutate(temporal_pool_it2 = case_when((any(temporal_pool_it1=="June, July") & any(temporal_pool_it1=="August, September")) & 
                                          (pool_sample_rate1 < 0.1 | pool_sample_size1<5 | is.na(pool_sample_rate1)) ~
                                         paste(unique(temporal_pool_it1), collapse = ", "),
                                       TRUE ~ temporal_pool_it1)) %>% 
  ungroup() %>%
  # --- FINAL AGE COMP
  # Calculate number of age samples per year/pooling category (i.e., pooled numerator): 
  group_by(YEAR, temporal_pool_it2, RESOLVED_AGE) %>% 
  mutate(pool_n = sum(n, na.rm=T)) %>% 
  # Roll up further to calculate the total sample size for each year/pool category (i.e., pooled denominator), and then calculate final rolled-up age comp: 
  group_by(YEAR, temporal_pool_it2) %>% 
  mutate(pool_sample_size2 = sum(unique(pool_n)),
         pool_propn = case_when(pool_sample_size2==0 ~ 0,
                           TRUE ~ pool_n/pool_sample_size2)) %>%
  print()




########################################################################################################################################################

#                                                                        JOIN + Export

# ============================== JOIN rec catch, pooled ages to NITmapping file ==============================

NITmap01 <- left_join(NITmap %>% 
                        mutate(across(everything(), as.character)),
                      
                      NITrecCatchbyAge_pooled %>% 
                        group_by(YEAR, temporal_pool_it2, RESOLVED_AGE, MONTH) %>% 
                        summarize(monthly_catch_estimate = unique(monthly_catch_estimate),
                                  original_n = unique(month_sample_size),
                                  n = unique(pool_n),
                                  propn = unique(pool_propn),
                                  subareas = unique(subareas)
                                  ) %>% 
                        pivot_wider(names_from = RESOLVED_AGE, values_from = c(n, propn)) %>% 
                        arrange(YEAR) %>%
                        # If the year of interest is present in the data series, then retain that year only; otherwise, select last year's data:
                        filter(if_else(YEAR%in%NITmap$TermRun_Year, TRUE,  YEAR==(YEAR-1))) %>%
                        # Rename/create columns to assist with joining
                        mutate(TermRun_sector01 = "Recreational",
                               TermRun_sector02 = "Area 21 Terminal") %>% 
                        rename(Enumeration = monthly_catch_estimate,
                               TermRun_AGEStemp = temporal_pool_it2,
                               TermRun_temp_strata = MONTH,
                               TermRun_AGESspat = subareas,
                               TermRun_AGES_year = YEAR) %>% 
                        mutate(across(everything(), as.character)),
                        
                      by=c("TermRun_sector01", "TermRun_sector02", "TermRun_temp_strata")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(Enumeration = coalesce(Enumeration.x, Enumeration.y),
         TermRun_AGEStemp = coalesce(TermRun_AGEStemp.x, TermRun_AGEStemp.y),
         TermRun_AGESspat = coalesce(TermRun_AGESspat.x, TermRun_AGESspat.y),
         TermRun_AGEStemp = coalesce(TermRun_AGEStemp.x, TermRun_AGEStemp.y),
         TermRun_AGESspat = coalesce(TermRun_AGESspat.x, TermRun_AGESspat.y),
         TermRun_AGES_year = coalesce(TermRun_AGES_year.x, TermRun_AGES_year.y),
         .keep="unused") %>%
  relocate(c(Enumeration, contains("?"), contains("TermRun_AGES")), .after=TermRun_spatial_substrata) %>%
  print()
  



# ============================== EXPORT ==============================
# To github repo ---------------------------
writexl::write_xlsx(NITmap01, 
                    path=paste0(here::here("termNIT"), "/", analysis_year, 
                                "/R_OUT - TERMNIT_mapping_",
                                analysis_year,
                                "-output_from_01",
                                ".xlsx"))




