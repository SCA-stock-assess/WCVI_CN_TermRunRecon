# Reliable PBT baselines 
# Apr 2024



# Load packages ----------------------
library(tidyverse)



# ======================== Load PBT inventory ========================  

# **DELETE THIS SOON - new inventory file with tag rate already calculated!
   #readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/2023-08-24 Chinook PBT Inventory BYs 2013-2021_draft .xlsx",
#                                        sheet="2013-2021 update", guess_max=10000) %>% 
#   rename(`(R) SAMPLE YEAR`=...1,
#          status=...2) %>% 
#   fill(`(R) SAMPLE YEAR`, .direction="down") %>% 
#   filter(`(R) SAMPLE YEAR`!="Comments") %>%
#   pivot_longer(cols=c(Ashlu:WOSS_RIVER), names_to = "MGL_Brood_Collection", values_to = "n") %>% 
#   filter(grepl("Bedwell|Burman|Campbell|Conuma|Cowichan|Cypre|Gold|Goldstream|Kennedy|Lang|Leiner|Campbell|Qualicum|Marble|Nahmint|Nanaimo|Nimpkish|Nitinat|
#                Oyster|Phillips|Puntledge|Quinsam|Robertson|Salmon River Jnst|San Juan|Sarita|Sooke|Sucwoa|Tahsis|Thornton|Toquart|Tranquil|Woss", 
#                MGL_Brood_Collection, ignore.case=T)) %>% 
#   pivot_wider(names_from = "status", values_from = n) %>%
#   group_by(`(R) SAMPLE YEAR`, MGL_Brood_Collection) %>% 
#   summarize(propn_genotyped = as.numeric(Genotyped)/as.numeric(Brood)) %>% 
#   mutate_all(~ifelse(is.nan(.), NA, .)) %>%
#   mutate(MGL_Brood_Collection = gsub(str_to_title(gsub(MGL_Brood_Collection, pattern="_", replacement=" ")), pattern=" River", replacement="")) %>%
#   print()
  
  
# Load full PBT inventory file -------------------------
PBT_inventory <-  readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/bch_v4.2b_2013-2022_brood-counts_tagging_rates_2024-06-05.xlsx",
                     sheet="ch_supplementary_file_to-check") %>% 
  filter(collection_extract %notin% c("ProvState_extract", "repunit_extract", "Notes", "StockCode_extract")) %>% 
  pivot_longer(cols=c("ALOUETTE_RIVER":"YUKON_RIVER@WHITEHORSE"), names_to = "system", values_to = "val") %>% 
  mutate(`(R) SAMPLE YEAR` = str_sub(collection_extract, start=1, end=4),
         `(R) STOCK` = trimws(gsub(gsub(gsub(gsub(str_to_title(gsub(system, pattern="_",  replacement=" ")), 
                                                pattern="River", replacement=""),
                                           pattern="Creek", replacement=""),
                                      pattern="  ", replacement=" "), 
                                 pattern=" -", replacement=" ")),
         val = case_when(grepl(">1", val) ~ as.numeric(1),
                         val=="1*" ~ NA,
                         TRUE ~ as.numeric(val))
  ) %>%
  print()
  
 
# Simplify to extract just the tagging rates for the broodstock (makes manipulation easier below and helpful for reference) 
PBT_tagrates <- PBT_inventory %>% 
  filter(grepl("tag_rate", collection_extract, ignore.case=T))



######################################################################################################################################################


# ========================= DETERMINE FIRST FULL RETURN YEAR =========================
# Require 5 consecutive years with PBT tag rate > 70% (arbitrary cut off but kinda based on discussions with ER)

# Define function to find rolling 5 year window of full PBT results -------------------------
# TY chatGPT :)
findFirstFullPBTBY <- function(x, other_col) {
  x <- ifelse(is.na(other_col), NA, x)
  roll_max <- zoo::rollapply(x, width=5, FUN=max, align="right", fill=NA)
  return(roll_max)
}


# Filter out unreliable years, and create new variable indicating first full reliable return year for PBT -------------------------
PBTreliableLong <- PBT_tagrates %>% 
  mutate_at("(R) SAMPLE YEAR", as.numeric) %>%
  filter(val>=0.7) %>%
  group_by(`(R) STOCK`) %>% 
  complete(.,  `(R) SAMPLE YEAR`=2013:analysis_year) %>%
  # *** cheating for now 
  mutate(val = case_when(grepl("San Juan", `(R) STOCK`) & `(R) SAMPLE YEAR`==2018 ~ 0.999999,
                                     TRUE ~ as.numeric(val))) %>% 
  # ***
  group_by(`(R) STOCK`) %>%
  mutate(firstFullReturnYr = findFirstFullPBTBY(`(R) SAMPLE YEAR`, val)+1,
         fullPBT_BYid = case_when(!is.na(firstFullReturnYr) ~ paste(system, "-", firstFullReturnYr),
                                  TRUE ~ NA)) %>%
  print()


# Simplify for easy referencing in other files 
PBTreliableShort <- PBTreliableLong %>% 
  filter(!is.na(firstFullReturnYr)) %>% 
  group_by(`(R) STOCK`) %>% 
  filter(firstFullReturnYr==min(firstFullReturnYr)) %>% 
  select(`(R) STOCK`, firstFullReturnYr, fullPBT_BYid)





# Clean up for when this script is sourced by other scripts -------------------------
remove(findFirstFullPBTBY, PBT_inventory, PBTreliableLong) 



# ======================== EXPORT ========================  

# To Network -------------------------
writexl::write_xlsx(PBTreliableShort, 
                    "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/R_OUT - First return year with a full PBT baseline by stock - draft working.xlsx")



# To github -------------------------
writexl::write_xlsx(PBTreliableShort, 
                    here::here("outputs", 
                               "R_OUT - First return year with a full PBT baseline by stock - draft working"))



######################################################################################################################################################


# ========================= IDENTIFY ANY BROODYEAR WITH A PBT REFERENCE =========================
# Any BYs with PBT tag rate > 70% (arbitrary cut off but kinda based on discussions with ER)

PBT_byBY <- PBT_tagrates %>% 
  group_by(`(R) STOCK`) %>% 
  filter(val>=0.7)



# ======================== EXPORT ========================  

# To Network -------------------------
writexl::write_xlsx(PBT_byBY, 
                    "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/R_OUT - All BYs with reliable PBT by stock - draft working.xlsx")





