# PBT summary pivot


pbtsummary <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/results/2024-12-05 Chinook_PBT Summary_2013-2023.xlsx") %>%
  filter(`wcvi y/n`=="WCVI") %>%
  group_by(Species, `wcvi y/n`, Brood_Regional_Area, Brood_Year, Brood, Brood_Recorded, Brood_Genotyped) %>%
  summarize(Brood_Recorded = unique(Brood_Recorded),
            Brood_Genotyped = unique(Brood_Genotyped), 
            BY_tagrate = case_when(Brood_Recorded==0 ~ NA,
                                   TRUE ~ Brood_Genotyped/Brood_Recorded)) %>%
  arrange(Brood, Brood_Year)  


  pivot_wider(names_from = Brood,
              values_from = Stock_Tag_Rate)
