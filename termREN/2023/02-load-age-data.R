# 02-load-ages.R 



# ============================= SET UP ============================= 
# Load high-use packages -------------------------------
library(tidyverse)

# Helpers -------------------------------
options(scipen=9999)

full_age_range <- tibble(RESOLVED_AGE = c(2:6)) 
full_month_range <- tibble(MONTH=c("June", "July", "August", "September"))
"%notin%" <- Negate("%in%")
source(here::here("set-analysis-year.R"))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ============================= LOAD CREST BIODATA =============================
# Option 1: Run source CRESTcompile.R -------------------------------     only need to do a few times
  # source(here::here("scripts", "joins", "4-CRESTcompile-BDWR_grouped.R"))
  # outputs as crestBDWR_CNgrouped.recSubGroups

# Option 2: Load data directly -------------------------------     
crestBDWR_CNgrouped.recSubGroups <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R", 
                                                                       pattern = "^R_OUT - Biological_Data_With_Results.*\\.xlsx$",
                                                                       full.names = T),
                                                       sheet=1, guess_max = 10000)


# ============================= SUMMARIZE CREST BIODATA =============================
tr.rec.ages <- crestBDWR_CNgrouped.recSubGroups %>%
  filter(SUBAREA%in%c("20A", "20B", "20E", "20-1", "20-3", "Area 20 (West)"), SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE), DISPOSITION=="Kept") %>%
  arrange(SUBAREA) %>%
  mutate(subareas_sample = paste0(unique(SUBAREA), collapse=", ")) %>%
  group_by(YEAR, MONTH, AREA, SUBAREA, RESOLVED_AGE) %>%
  summarize(n = n(), TermRun_spatial_substrata=unique(subareas_sample)) %>%
  full_join(., full_month_range) %>%
  #full_join(., full_age_range) %>%
  ungroup() %>%
  complete(YEAR, MONTH, AREA, SUBAREA, RESOLVED_AGE, fill=list(n=0)) %>%
  group_by(YEAR, MONTH) %>%
  fill(c(AREA), .direction="updown") %>%
  filter(!is.na(RESOLVED_AGE) & !is.na(YEAR) & !is.na(MONTH)) %>%
  group_by(YEAR, MONTH) %>%
  fill(TermRun_spatial_substrata, .direction = "updown") %>%
  mutate(TermRun_spatial_substrata = case_when(is.na(TermRun_spatial_substrata) ~ "no samples",
                                     TRUE ~ TermRun_spatial_substrata)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ============================= LOAD EPRO BIODATA =============================
# Option 1: Run source CRESTcompile.R -------------------------------     only need to do a few times
# source(here::here("scripts", "joins", "3-EPRO_biodata_with_results.R"))
# outputs as wcviCNepro_w_Results

# Option 2: Load data directly -------------------------------     
wcviCNepro_w_Results <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R", 
                                                           pattern = "^R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS.*\\.xlsx$",
                                                           full.names = T),
                                           sheet=2)



# ============================= SUMMARIZE EPRO BIODATA =============================
tr.esc.bio <- wcviCNepro_w_Results %>%
  filter(grepl("San Juan", Spawning.Stock.Name), Species=="Chinook") %>%
  group_by(`(R) RETURN YEAR`, Sample.Maturity.Class, Parent.Activity.Type, Source.Location.Type, `(R) RESOLVED TOTAL AGE`) %>%
  summarize(n=n()) %>%
  filter(!is.na(`(R) RESOLVED TOTAL AGE`)) %>%
  print()























# Clean for purposes of source() call ------------------------
remove(crestBDWR_CNgrouped.recSubGroups, wcviCNepro_w_Results, full_age_range, full_month_range, "%notin%")


