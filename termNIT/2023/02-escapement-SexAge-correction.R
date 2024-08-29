# termNIT 
# 02-escapement-ageCorrection
# aug 2024


# SET UP 
library(tidyverse)


# Helpers
full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))
"%notin%" <- Negate("%in%")


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

# 1. Summarize broodstock age and sampling data and force jack correction
#   "Age proportions from SAMPLE (close to 50:50)"  and "CORRECTED age proportions" 
agePropnSAMPLE <- 
  # ---- Age proportions from SAMPLE (close to 50:50): 
  full_join(NITepro %>% 
                              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & `(R) RESOLVED TOTAL AGE`%in%c(1:6)) %>%
                              group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
                              summarize(n_age=n()) %>% 
                              group_by(Maturity.Class) %>% 
                              mutate(n=sum(n_age),
                                     propn_age = n_age/n) %>% 
                              ungroup(),
                            full_age_range) %>% 
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  mutate(propn_age = case_when(is.na(propn_age) ~ 0,
                               TRUE ~ propn_age),
         n_age = case_when(is.na(n_age) ~ 0,
                           TRUE ~ n_age)) %>%
  #select(-c(n_age)) %>% 
  group_by(Maturity.Class) %>%
  fill(n, .direction="updown") %>%
  full_join(.,
            NITepro %>% 
              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & `(R) RESOLVED TOTAL AGE`%in%c(1:6)) %>%
              group_by(`(R) RESOLVED TOTAL AGE`) %>% 
              summarize(n_age = n()) %>%
              mutate(propn_age = n_age/sum(n_age),
                     Maturity.Class="Total",
                     n=sum(n_age))) %>%
  select(-c(n_age)) %>%
  mutate(CORR_propn_age = case_when(Maturity.Class=="Jack" & `(R) RESOLVED TOTAL AGE`==2 ~ 1,
                                    Maturity.Class=="Jack" & `(R) RESOLVED TOTAL AGE`!=2 ~ 0,
                                    Maturity.Class=="Male" & `(R) RESOLVED TOTAL AGE`==2 ~ 0,
                                    TRUE ~ NA)) %>%
  group_by(Maturity.Class) %>% 
  # ---- CORRECTED age proportions: 
  mutate(CORR_propn_age = case_when(Maturity.Class!="Jack" ~ propn_age/sum(propn_age),
                                    TRUE ~ CORR_propn_age)) %>%
  # (Import escapement estimate from mapping file to do the rest of the math): 
  mutate(escapement_estimate = NITmap[NITmap$TermRun_sector01=="Escapement - mainstem" & NITmap$TermRun_sex_strata=="Total (incl Jacks)",]$Enumeration,
         true_sex_ratio = case_when(Maturity.Class=="Male" ~ )) %>%
  # ---- Number by age and sex (CORRECTED): 
  mutate(n_ageSex_CORR = )
  print()

  





















