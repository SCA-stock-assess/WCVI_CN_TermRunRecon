# Reliable PBT baselines 
# Apr 2024



# Load packages ----------------------
library(tidyverse)



# ======================== Load PBT inventory ========================  
# Load data, calculate % of each BY genotyped -------------------------
SC_PBT_inventory <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/2023-08-24 Chinook PBT Inventory BYs 2013-2021_draft .xlsx",
                                       sheet="2013-2021 update", guess_max=10000) %>% 
  rename(`(R) SAMPLE YEAR`=...1,
         status=...2) %>% 
  fill(`(R) SAMPLE YEAR`, .direction="down") %>% 
  filter(`(R) SAMPLE YEAR`!="Comments") %>%
  pivot_longer(cols=c(Ashlu:WOSS_RIVER), names_to = "MGL_Brood_Collection", values_to = "n") %>% 
  filter(grepl("Bedwell|Burman|Campbell|Conuma|Cowichan|Cypre|Gold|Goldstream|Kennedy|Lang|Leiner|Campbell|Qualicum|Marble|Nahmint|Nanaimo|Nimpkish|Nitinat|
               Oyster|Phillips|Puntledge|Quinsam|Robertson|Salmon River Jnst|San Juan|Sarita|Sooke|Sucwoa|Tahsis|Thornton|Toquart|Tranquil|Woss", 
               MGL_Brood_Collection, ignore.case=T)) %>% 
  pivot_wider(names_from = "status", values_from = n) %>%
  group_by(`(R) SAMPLE YEAR`, MGL_Brood_Collection) %>% 
  summarize(propn_genotyped = as.numeric(Genotyped)/as.numeric(Brood)) %>% 
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(MGL_Brood_Collection = gsub(str_to_title(gsub(MGL_Brood_Collection, pattern="_", replacement=" ")), pattern=" River", replacement="")) %>%
  print()


# Define function to find rolling 5 year window of full PBT results -------------------------
# TY chatGPT :)
findFirstFullPBTBY <- function(x, other_col) {
  x <- ifelse(is.na(other_col), NA, x)
  roll_max <- zoo::rollapply(x, width=5, FUN=max, align="right", fill=NA)
  return(roll_max)
}

# Filter out unreliable years, and create new variable indicating first full reliable BY for PBT -------------------------
SC_PBTreliable <- SC_PBT_inventory %>% 
  mutate_at("(R) SAMPLE YEAR", as.numeric) %>%
  filter(propn_genotyped>=0.85) %>%
  group_by(MGL_Brood_Collection) %>% 
  complete(.,  `(R) SAMPLE YEAR`=2013:analysis_year) %>%
  # *** cheating for now 
  mutate(propn_genotyped = case_when(MGL_Brood_Collection=="San Juan" & `(R) SAMPLE YEAR`%in%c(2022,2023) ~ 0.999999,
                                     TRUE ~ propn_genotyped)) %>% 
  # ***
  group_by(MGL_Brood_Collection) %>%
  mutate(firstFullBY = findFirstFullPBTBY(`(R) SAMPLE YEAR`, propn_genotyped)+1,
         fullPBT_BYid = case_when(!is.na(firstFullBY) ~ paste(MGL_Brood_Collection, "-", firstFullBY),
                                  TRUE ~ NA)) %>%
  print()


# Clean up for joins -------------------------
remove(findFirstFullPBTBY, SC_PBT_inventory)



# ======================== EXPORT ========================  

# To Network -------------------------
writexl::write_xlsx(SC_PBTreliable, 
                    "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/SC - Earliest reliable return year using PBT baseline by stock - draft working.xlsx")



# To github -------------------------
writexl::write_xlsx(SC_PBTreliable, here::here("outputs", "R_OUT - SC Earliest reliable return year using PBT baseline by stock - draft working.xlsx"))

