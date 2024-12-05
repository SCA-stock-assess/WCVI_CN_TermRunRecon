
# Dump and combine age data 


# Load libraries ------------------------
library(saaWeb)
library(tidyverse)
library(writexl)
library(here)

# Helper ------------------------
"%notin%" <- Negate("%in%")




# ============================ 1. DUMP CWT TAGCODES ============================

# Chinook release tagcodes ------------------------
cn_relTagCodes <- saaWeb:::runCwtExtractorQuery(here::here("scripts", "json", "CWT_Releases_CN_2012-present.json"), config_file=here::here("saaWeb.config"), 
                                                user_name = NULL, password=NULL) %>%
  setNames(paste0('MRP_', names(.))) %>% 
  select(`MRP_Tagcode`, `MRP_Species Name`, `MRP_Release Agency Code`, `MRP_Project Name`, `MRP_Country Code`, `MRP_Brood Year`, `MRP_Release Year`, 
         `MRP_Recovery Years`, `MRP_Hatchery Site Name`, `MRP_Hatchery PSC Basin Name`, `MRP_Release Site Name`, `MRP_Release PSC Basin Name`, 
         `MRP_Stock Site Name`, `MRP_Stock PSC Basin Name`, `MRP_Hatchery Site Prov/State Code`:`MRP_Stock Site Prov/State Code`, `MRP_Total Released`) %>%
  mutate(`(R) TAGCODE` = MRP_Tagcode,
         `MRP_Stock Site Name` = case_when(is.na(`MRP_Stock Site Name`) ~ paste0(`MRP_Hatchery Site Name`, sep=" - ", `MRP_Release Site Name`),
                                           TRUE ~ `MRP_Stock Site Name`)) %>% 
  print()





# ============================ 2. EXPORT ============================

# 2.1. Export to StA drive, WCVI Term Run folder ------------------------
writexl::write_xlsx(cn_relTagCodes, 
                    paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/R_OUT - Chinook CWT release tagcodes BY ", 
                           min(cn_relTagCodes$`MRP_Brood Year`), 
                           "-", 
                           max(cn_relTagCodes$`MRP_Brood Year`), 
                           ".xlsx")
                    )



# 2.2. Export to github repo ------------------------
writexl::write_xlsx(cn_relTagCodes, here("outputs", 
                                           paste0("R_OUT - Chinook CWT release tagcodes BY ", 
                                                  min(cn_relTagCodes$`MRP_Brood Year`), 
                                                  "-", 
                                                  max(cn_relTagCodes$`MRP_Brood Year`), 
                                                  ".xlsx")
                                           )
                    )


















