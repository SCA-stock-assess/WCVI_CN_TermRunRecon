# termNIT 
# 02-escapement-ageCorrection
# aug 2024


# SET UP 
library(tidyverse)


# Helpers
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
fecundity_at_age <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6),
                           fecundity = c(0,3000,3500,4000,4000))
"%notin%" <- Negate("%in%")
options(scipen=9999)



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
NITmap <- readxl::read_excel(path=here::here("termNIT", "2023", list.files(path=here::here("termNIT", "2023"),
                                                                           pattern="^TERMNIT_mapping*")),
                             skip=1,
                             sheet="termNIT_map")



# Read EPRO data -------------------------------
NITepro <- readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R/", 
                                          list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
                                                     pattern="^R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS*")),
                              sheet="AllFacilities w RESULTS")



############################################################################################################################################################

#                                                             Recreating the green (black) box



# 1. Work through the correction to the point where it is still broken out by Male, Female, Jack ---------------------------------------------
#   "Age proportions from SAMPLE (close to 50:50)"  to "Number by age and sex (CORRECTED): Broken Out" 

sexAgeCorrection_brokenOut <- 
  # ---- Age proportions from SAMPLE (close to 50:50): 
  full_join(NITepro %>% 
              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & `(R) RESOLVED TOTAL AGE`%in%c(2:6)) %>%
              group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
              summarize(n_age=n()) %>% 
              group_by(Maturity.Class) %>% 
              mutate(n_sample=sum(n_age),
                     propn_age_sample = n_age/n_sample) %>% 
              ungroup() %>% 
              group_by(`(R) RESOLVED TOTAL AGE`) %>% 
              ## (new) JACK CORRECTION NUMERICAL: 
              mutate(n_age_jackCORR = case_when(Maturity.Class=="Female" ~ n_age,
                                                 Maturity.Class=="Jack" & `(R) RESOLVED TOTAL AGE`!=2 ~ n_age-n_age,
                                                 Maturity.Class=="Male" & `(R) RESOLVED TOTAL AGE`==2 ~ n_age-n_age)) %>% 
              group_by(`(R) RESOLVED TOTAL AGE`, Maturity.Class%in%c("Male", "Jack")) %>% 
              mutate(n_age_jackCORR = case_when(is.na(n_age_jackCORR) ~ sum(n_age),
                                                 TRUE~n_age_jackCORR)) %>%
              ungroup() %>%
              select(-c(`Maturity.Class %in% c("Male", "Jack")`)) %>% 
              group_by(Maturity.Class) %>% 
              # ---- CORRECTED age proportions (for jack ID issues): 
              mutate(n_sample_jackCORR = sum(n_age_jackCORR),
                     propn_age_sample_jackCORR = n_age_jackCORR/unique(n_sample_jackCORR)) %>% 
              ungroup(),
            full_age_range) %>% 
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  mutate(across(c(n_age, propn_age_sample, n_age_jackCORR, propn_age_sample_jackCORR), ~case_when(is.na(.)~0, TRUE~.))) %>%
  group_by(Maturity.Class) %>%
  fill(n_sample, .direction="updown") %>% 
  # (Import escapement estimate and observed sex ratio from mapping file to do the rest of the math): 
  mutate(escapement_estimate = NITmap[NITmap$TermRun_sector01=="Escapement - mainstem" & NITmap$TermRun_sex_strata=="Total (incl Jacks)",]$Enumeration,
         true_sex_ratio = case_when(Maturity.Class=="Male" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                      NITmap$TermRun_sex_strata=="Male",]$Enumeration,
                                    Maturity.Class=="Female" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                        NITmap$TermRun_sex_strata=="Female",]$Enumeration,
                                    Maturity.Class=="Jack" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                      NITmap$TermRun_sex_strata=="Jack",]$Enumeration)) %>%
  # ---- Number by age and sex (CORRECTED): 
  mutate(n_sex_CORR.breakout = escapement_estimate*true_sex_ratio,
         n_sexAge_CORR.breakout = n_sex_CORR.breakout*propn_age_sample_jackCORR) %>%  
  group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
  # ---- (new) Proportion by age and sex (CORRECTED):
  mutate(propn_sexAge_Corr.breakout = n_sexAge_CORR.breakout/unique(escapement_estimate)) %>%
  full_join(.,
            NITepro %>%
              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & `(R) RESOLVED TOTAL AGE`%in%c(2:6)) %>%
              group_by(`(R) RESOLVED TOTAL AGE`) %>%
              summarize(n_age = n()) %>%
              mutate(propn_age_sample = n_age/sum(n_age),
                     Maturity.Class="Total",
                     n_sample=sum(n_age))) %>%
  select(-c(n_age)) %>%
  group_by(`(R) RESOLVED TOTAL AGE`) %>% 
  mutate(propn_sexAge_Corr.breakout = case_when(Maturity.Class=="Total" ~ sum(propn_sexAge_Corr.breakout,na.rm=T)/1,
                                                TRUE ~ propn_sexAge_Corr.breakout),
         TermRun_AGEStemp = "Broodstock corrected",
         TermRun_AGESspat = "Broodstock corrected",
         TermRun_AGESsex = paste0("Broodstock corrected - ", Maturity.Class)) %>%
  fill(escapement_estimate, .direction="down") %>%
  print()




# 1. Work through the correction following that as it is rolled up by Female and Male (incl Jacks) ---------------------------------------------
#   "Number by age and sex (CORRECTED): Rolled up"  to  "Proportion by age and sex (CORRECTED): Rolled up"
sexAgeCorrection_rolledUp <- sexAgeCorrection_brokenOut %>% 
  mutate(Maturity.Class = case_when(Maturity.Class %in% c("Male", "Jack") ~ "Males (incl Jacks)",
                                           TRUE ~ Maturity.Class)) %>%
  group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`, escapement_estimate) %>% 
  summarize(n_sample = sum(n_sample),
            n_sexAge_CORR.rollup = sum(n_sexAge_CORR.breakout)) %>%
  group_by(Maturity.Class) %>% 
  mutate(n_sex_CORR.rollup = sum(n_sexAge_CORR.rollup),
         propn_sexAge_CORR.rollup = n_sexAge_CORR.rollup/unique(escapement_estimate)) %>%
  group_by(`(R) RESOLVED TOTAL AGE`) %>% 
  mutate(propn_sexAge_CORR.rollup = case_when(Maturity.Class=="Total" ~ sum(propn_sexAge_CORR.rollup/1,na.rm=T),
                                              TRUE ~ propn_sexAge_CORR.rollup),
         TermRun_AGEStemp = "Broodstock corrected",
         TermRun_AGESspat = "Broodstock corrected",
         TermRun_AGESsex = paste0("Broodstock corrected - ", Maturity.Class),
         ) %>%
  print()

  

tt<- full_join(sexAgeCorrection_brokenOut,sexAgeCorrection_rolledUp)

#***** clean up this join ^^ next day  and join to mapping file.... 
##########################################################################

#                                                                    CALCULATE EGG DEPOSITION


fecundityAgePropn <- left_join(sexAgeCorrection_brokenOut %>%
                                 filter(Maturity.Class=="Female"),
                               fecundity_at_age) %>% 
  mutate(fecundity_AgeCorr = n_sexAge_CORR*fecundity,
         total_egg_deposition = sum(fecundity_AgeCorr)) %>%
  print()
 












