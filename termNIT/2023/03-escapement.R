# termNIT 
# 03-escapement
# aug 2024


# SET UP 
library(tidyverse)



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


#                                                              Join mapping file to AGE summary 

full_age_range <- tibble(`(R) RESOLVED TOTAL AGE` = c(2:6))


# 1. Age summary for return year of interest 
    # Return year of interest is assumed to be the max year in the EPRO file

NITage_summary <- full_join(NITepro %>% 
                              filter(`(R) RETURN YEAR` %in% NITmap$TermRun_Year & grepl("Nitinat R Fall Chinook", Spawning.Stock) & 
                                       `(R) RESOLVED TOTAL AGE`%in%c(1:6)) %>% 
                              group_by(Maturity.Class, `(R) RESOLVED TOTAL AGE`) %>% 
                              summarize(n=n()) %>%
                              ungroup(),
                            full_age_range) %>%
  complete(Maturity.Class, `(R) RESOLVED TOTAL AGE`, fill=list(`(R) RESOLVED TOTAL AGE`=0), explicit=F) %>%
  mutate(TermRun_AGEStemp = "Broodstock, morts, other",
         TermRun_AGESspat = "Broodstock, morts, other",
         TermRun_AGESsex = case_when(Maturity.Class=="Male" ~ "Broodstock, morts, other - Males",
                                     Maturity.Class=="Female" ~ "Broodstock, morts, other - Females",
                                     Maturity.Class=="Jack" ~ "Broodstock, morts, other - Jacks",
                                     TRUE ~ "FLAG"),
         TermRun_AGES_year = max(NITepro$`(R) RETURN YEAR`)) %>%
  print()
  







