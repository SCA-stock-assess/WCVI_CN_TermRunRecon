# PBT summary pivot
library(tidyverse)


# Read in PBT inventory data --------------
pbtsummary <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/results/2024-12-05 Chinook_PBT Summary_2013-2023.xlsx") %>%
  filter(`wcvi y/n`=="WCVI") %>%
  group_by(Species, `wcvi y/n`, Brood_Regional_Area, Brood_Year, Brood, Brood_Recorded, Brood_Genotyped) %>%
  summarize(Brood_Recorded = unique(Brood_Recorded),
            Brood_Genotyped = unique(Brood_Genotyped), 
            BY_tagrate = case_when(Brood_Recorded==0 ~ NA,
                                   TRUE ~ Brood_Genotyped/Brood_Recorded)) %>%
  arrange(Brood, Brood_Year) 




# ================== Export ================== 
# To github repo ---------------------------
writexl::write_xlsx(pbtsummary,
                    path=here::here("outputs", "R_OUT - PBT_Tag_Rates_AllStocks-All BYs.xlsx"))



# To DFO Network drive ---------------------------
# Biodata management:
writexl::write_xlsx(pbtsummary,
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/",
                                "R_OUT - PBT_Tag_Rates_WCVIStocks-All BYs.xlsx"))

# Run reconstruction data summaries:
writexl::write_xlsx(pbtsummary,
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/",
                                "R_OUT - PBT_Tag_Rates_WCVIStocks-All BYs.xlsx"))
