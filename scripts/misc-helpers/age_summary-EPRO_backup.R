# Age data dump summary
# Jan 2024
# For cases where EPRO has not updated available age data yet (lag is common)


# Load libraries -----------------------
library(tidyverse)
library(readxl)
library(writexl)
library(here)



# Helpers -----------------------
analysis_year <- 2023





# Load data from github -----------------------
wcvi.cn.ages <- list.files(here("outputs"), pattern = "^R_OUT - ALL South Coast Chinook Age results ", full.names = T) %>%
  purrr::set_names(
    list.files(here("outputs"), pattern = "^R_OUT - ALL South Coast Chinook Age results ", full.names = F)) %>%
  map(~readxl::read_excel(path = .x, trim_ws=T), id="path" ) %>% 
  list_rbind(names_to = "file_source") %>% 
  print()



############################################################################################################################################################


#                                                   SUMMARIZE ANALYSIS YEAR AGE RESULTS BY PROJECT/AREA


# Compile analysis year results and summarize % by age -----------------------

wcvi.cn.ages.SUMMARY <- wcvi.cn.ages %>% 
  filter(`(R) SAMPLE YEAR`==analysis_year,
         grepl("Hatchery|Escapement", PADS_ProjectName, ignore.case = T),
         !is.na(PADS_GrAge)) %>%
  mutate(min_Total_Age = case_when(PADS_GrAge %in% c(11:66) ~ as.numeric(substr(PADS_GrAge, 1,1)),
                                   grepl("M", PADS_GrAge, ignore.case=T) ~ as.numeric(substr(PADS_GrAge, 1,1))+1)) %>% 
  filter(!is.na(min_Total_Age)) %>%
  group_by(PADS_ProjectName, PADS_Location, PADS_GearMrpName, min_Total_Age) %>% 
  summarize(n=n()) %>% 
  group_by(PADS_ProjectName, PADS_Location, PADS_GearMrpName) %>% 
  mutate(total_n = sum(n),
         age_propn = n/total_n) %>% 
  arrange(min_Total_Age) %>%
  pivot_wider(names_from = min_Total_Age, values_from = c(n, age_propn), names_prefix = "age") %>%
  print()

  

############################################################################################################################################################

#                                                   EXPORT


# To github -----------------------
writexl::write_xlsx(wcvi.cn.ages.SUMMARY, 
                    path = paste0(here("outputs"),
                                  "/(temp) R_OUT - age summary if EPRO has not updated yet ",
                                  analysis_year,
                                  ".xlsx"))



# To Sharepoint -----------------------
writexl::write_xlsx(wcvi.cn.ages.SUMMARY, 
                    path = paste0("C:/Users/",
                                  Sys.info()["login"],
                                  "/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/",
                                  analysis_year,
                                  "/Communal data/Misc biodata dumps",
                                  "/(temp) R_OUT - age summary if EPRO has not updated yet ",
                                  analysis_year,
                                  ".xlsx"))



# To DFO Network drive -----------------------
writexl::write_xlsx(wcvi.cn.ages.SUMMARY, 
                    path = paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/",
                                  analysis_year,
                                  "/(temp) R_OUT - age summary if EPRO has not updated yet ",
                                  analysis_year,
                                  ".xlsx"))




