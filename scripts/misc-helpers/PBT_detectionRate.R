# look for san juan samples

library(tidyverse)
"%notin%" <- Negate("%in%")

bdwr <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/R_OUT - Biological_Data_with_Results AND TERM GROUPINGS 2017-2024.xlsx",
                           sheet=2, guess_max = 10000)

sj.bdwr <- bdwr %>% 
  filter(grepl("san juan", `(R) Term Sum`, ignore.case=T), SAMPLE_TYPE=="Sport") %>%
  group_by(YEAR, AREA, RESOLVED_STOCK_SOURCE) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = RESOLVED_STOCK_SOURCE, values_from = n)




wcvi.bdwr <- full_join(
  bdwr %>% 
    filter(`(R) Term RR Roll Ups` %notin% c("NON-WCVI", "Unknown", "QUILAYUTE", "SKAGIT", "OMEGA PACIFIC HATCHERY", "BEDWELL ESTUARY SEAPEN"), 
           SAMPLE_TYPE=="Sport", ADIPOSE_FIN_CLIPPED%in%c("Y","N"), SPECIES==124) %>% 
    group_by(YEAR, `(R) Term RR Roll Ups`, ADIPOSE_FIN_CLIPPED) %>%
    summarize(n=n()) %>%
    pivot_wider(names_from = ADIPOSE_FIN_CLIPPED, values_from = n) %>%
    rename(TOTAL_UNCLIPPED=N,
           TOTAL_CLIPPED=Y),
  
  bdwr %>%
    filter(`(R) Term RR Roll Ups` %notin% c("NON-WCVI", "Unknown", "QUILAYUTE", "SKAGIT", "OMEGA PACIFIC HATCHERY", "BEDWELL ESTUARY SEAPEN"), 
           SAMPLE_TYPE=="Sport", ADIPOSE_FIN_CLIPPED%in%c("Y","N"), SPECIES==124) %>%
    mutate(PBT_DETECT = case_when(ADIPOSE_FIN_CLIPPED=="Y" & PBT_BROOD_YEAR %notin% c("0", "Not Loaded", "GSI 0000") & !is.na(PBT_BROOD_YEAR)  ~ "PBT YES & YES CLIP",
                                  ADIPOSE_FIN_CLIPPED=="Y" & (PBT_BROOD_YEAR %in% c("0", "Not Loaded", "GSI 0000") | is.na(PBT_BROOD_YEAR)) ~ "PBT NO & YES CLIP",
                                  PBT_BROOD_YEAR %notin% c("0", "Not Loaded", "GSI 0000") & !is.na(PBT_BROOD_YEAR) & ADIPOSE_FIN_CLIPPED=="N" ~ "PBT YES & NO CLIP",
                                  
                                  #(PBT_BROOD_YEAR %in% c("0", "Not Loaded", "GSI 0000") | is.na(PBT_BROOD_YEAR)) & ADIPOSE_FIN_CLIPPED=="Y" & PROB_1==1 ~ "PBT NO & YES CLIP & PROB1=1",
                                  TRUE ~ NA)) %>%
    filter(!is.na(PBT_DETECT)) %>%
    group_by(YEAR, `(R) Term RR Roll Ups`, PBT_DETECT) %>% 
    summarize(n=n()) %>%
    pivot_wider(names_from = PBT_DETECT, values_from = n)
  ) %>%
  relocate("PBT YES & YES CLIP",  .after="PBT NO & YES CLIP") %>%
  rowwise() %>%
  mutate(quick_check = case_when(sum(`PBT NO & YES CLIP`,`PBT YES & YES CLIP`,na.rm=T)==TOTAL_CLIPPED ~ "ok",
                                 TRUE ~ "FLAG")) %>%
  print()

write.csv(wcvi.bdwr, "C:/Users/DAVIDSONKA/Desktop/PBT detection.csv",row.names=F)




