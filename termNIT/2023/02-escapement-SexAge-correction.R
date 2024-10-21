# termNIT 
# 02-escapement-ageCorrection
# aug 2024


# ============================= SET UP  ============================
# Load high-use packages -------------------------------
library(tidyverse)


# Helpers -------------------------------
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
fecundity_at_age <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6),
                           fecundity = c(0,3000,3500,4000,4000),
                           Maturity.Class = "Female")
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
NITmap01 <- readxl::read_excel(path=here::here("termNIT", "2023", list.files(path=here::here("termNIT", "2023"),
                                                                             pattern="^R_OUT - TERMNIT_mapping_[0-9]{4}-output_from_01\\.xlsx$",
                                                                             full.names = F)),
                               skip=1,
                               sheet="Sheet1")



# Read EPRO data -------------------------------
NITepro <- readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R/", 
                                          list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
                                                     pattern="^R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS*")),
                              sheet="AllFacilities w RESULTS")



############################################################################################################################################################

#                                                     Recreating the green (black) sex/age correction box


# ============================== SEX/AGE CORRECTION ==============================

# 1. Work through the correction to the point where it is still broken out by Male, Female, Jack ---------------------------------------------
#   "Age proportions from SAMPLE (close to 50:50)"  to "Number by age and sex (CORRECTED): Broken Out" 

sexAgeCorrection <- full_join(
  # ---- Age proportions from SAMPLE (close to 50:50): 
  sexAgeCorrection_brokenOut <- full_join(
    NITepro %>% 
      filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year &     ## change this line if you want to do a multi-year average (would need to add extra code for this)
               grepl("Nitinat R Fall Chinook", Spawning.Stock) & 
               `(R) RESOLVED TOTAL AGE`%in%c(2:6)) %>%
      group_by(`(R) RETURN YEAR`, Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
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
      ungroup() %>%
      rename(TermRun_AGES_year = `(R) RETURN YEAR`),
    full_age_range) %>% 
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  mutate(across(c(n_age, propn_age_sample, n_age_jackCORR, propn_age_sample_jackCORR), ~case_when(is.na(.)~0, TRUE~.))) %>%
  group_by(Maturity.Class) %>%
  fill(c(TermRun_AGES_year, n_sample), .direction="updown") %>% 
  # (Import escapement estimate and observed sex ratio from mapping file to do the rest of the math): 
  mutate(escapement_estimate = NITmap[NITmap$TermRun_sector01=="Escapement - mainstem" & NITmap$TermRun_sex_strata=="Total (incl Jacks)",]$Enumeration,
         true_sex_ratio = case_when(Maturity.Class=="Male" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                      NITmap$TermRun_sex_strata=="Male",]$Enumeration,
                                    Maturity.Class=="Female" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                        NITmap$TermRun_sex_strata=="Female",]$Enumeration,
                                    Maturity.Class=="Jack" ~ NITmap[NITmap$TermRun_sector02=="Actual sex ratio (from hatchery staff)" & 
                                                                      NITmap$TermRun_sex_strata=="Jack",]$Enumeration)) %>%
  # ---- Number by age and sex (CORRECTED): 
  mutate(n_sex_CORR = escapement_estimate*true_sex_ratio,
         n_sexAge_CORR = n_sex_CORR*propn_age_sample_jackCORR) %>%  
  group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
  # ---- (new) Proportion by age and sex (CORRECTED):
  mutate(propn_sexAge_CORR = n_sexAge_CORR/unique(escapement_estimate)) %>%
  full_join(.,
            NITepro %>%
              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & `(R) RESOLVED TOTAL AGE`%in%c(2:6)) %>%
              group_by(`(R) RETURN YEAR`, `(R) RESOLVED TOTAL AGE`) %>%
              summarize(n_age = n()) %>%
              mutate(propn_age_sample = n_age/sum(n_age),
                     Maturity.Class="Total",
                     n_sample=sum(n_age)) %>%
              rename(TermRun_AGES_year = `(R) RETURN YEAR`)) %>%
  select(-c(n_age)) %>%
  group_by(`(R) RESOLVED TOTAL AGE`) %>% 
  mutate(propn_sexAge_CORR = case_when(Maturity.Class=="Total" ~ sum(propn_sexAge_CORR,na.rm=T)/1,
                                                TRUE ~ propn_sexAge_CORR),
         TermRun_AGEStemp = "Broodstock corrected",
         TermRun_AGESspat = "Broodstock corrected",
         TermRun_AGESsex = paste0("Broodstock corrected - ", Maturity.Class)) %>%
  fill(escapement_estimate, .direction="down") 
  
  ,


# 2. Work through the correction following that as it is rolled up by Female and Male (incl Jacks) ---------------------------------------------
#   "Number by age and sex (CORRECTED): Rolled up"  to  "Proportion by age and sex (CORRECTED): Rolled up"
sexAgeCorrection_brokenOut %>% 
  mutate(Maturity.Class = case_when(Maturity.Class %in% c("Male", "Jack") ~ "Males (incl Jacks)",
                                           TRUE ~ Maturity.Class)) %>%
  group_by(TermRun_AGES_year, Maturity.Class, `(R) RESOLVED TOTAL AGE`, escapement_estimate) %>% 
  summarize(n_sample = sum(n_sample),
            n_sexAge_CORR = sum(n_sexAge_CORR)) %>%
  group_by(Maturity.Class) %>% 
  mutate(n_sex_CORR = sum(n_sexAge_CORR),
         propn_sexAge_CORR = n_sexAge_CORR/unique(escapement_estimate)) %>%
  group_by(`(R) RESOLVED TOTAL AGE`) %>% 
  mutate(propn_sexAge_CORR = case_when(Maturity.Class=="Total" ~ sum(propn_sexAge_CORR/1,na.rm=T),
                                              TRUE ~ propn_sexAge_CORR),
         TermRun_AGEStemp = "Broodstock corrected",
         TermRun_AGESspat = "Broodstock corrected",
         TermRun_AGESsex = paste0("Broodstock corrected - ", Maturity.Class)) 
) %>% 
  # 3. Fecundity/egg deposition ---------------------------------------------
  ungroup() %>%  
  left_join(.,
            fecundity_at_age) %>% 
  mutate(total_fecundity = n_sexAge_CORR*fecundity,
         total_egg_deposition = case_when(Maturity.Class=="Female" ~ sum(total_fecundity, na.rm=T),
                                          TRUE ~ NA),
         propn_sexAge_CORR = case_when(Maturity.Class=="Jack" & `(R) RESOLVED TOTAL AGE`==2 ~ propn_age_sample,
                                       TRUE ~ propn_sexAge_CORR)) %>%
  print()



# Remove temporary table for sake of join above
remove(sexAgeCorrection_brokenOut) 
  


# ============================== EXPORT for record ==============================
# To github repo ---------------------------
writexl::write_xlsx(sexAgeCorrection, 
                    path=paste0(here::here("outputs"), 
                                "/R_OUT - Nitinat sex correction ",
                                max(sexAgeCorrection$TermRun_AGES_year),
                                ".xlsx"))



# To DFO Network drive ---------------------------
writexl::write_xlsx(sexAgeCorrection, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/TERMNIT/Nit",
                                max(sexAgeCorrection$TermRun_AGES_year),
                                "/R_OUT - Nitinat sex correction ",
                                max(sexAgeCorrection$TermRun_AGES_year),
                                ".xlsx"))




####################################################################################################################################################

#                                                                     JOIN to mapping file


# ============================== JOIN simplified output to mapping file ==============================
NITmap02 <- left_join(NITmap,
                      sexAgeCorrection %>% 
                        select(TermRun_AGES_year, Maturity.Class, `(R) RESOLVED TOTAL AGE`, propn_sexAge_CORR:TermRun_AGESsex) %>% 
                        pivot_wider(names_from=`(R) RESOLVED TOTAL AGE`, names_prefix="propn_age_", values_from=propn_sexAge_CORR),
                      by=c("TermRun_AGEStemp", "TermRun_AGESspat", "TermRun_AGESsex", "TermRun_AGES_year"))


# ============================== EXPORT ==============================
# To github repo ---------------------------
writexl::write_xlsx(NITmap02, 
                    path=paste0(here::here("termNIT"), "/", analysis_year, 
                                "/R_OUT - TERMNIT_mapping_",
                                analysis_year,
                                "-output_from_02",
                                ".xlsx"))



