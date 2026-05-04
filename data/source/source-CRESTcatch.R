
# Load CREST catch estimates from Salmon Drive 

CREST.catch <- readxl::read_excel(path="//ent.dfo-mpo.ca/dfo-mpo/GROUP/PAC/Reg_Shares/FHM/SMCS/Salmon/FMCR_Fishery_Monitoring_Catch_Reporting/Recreational_CM/Catch_Data/SC Sport Catch Creel Sub-area Disposition (Master Do Not Edit).xlsx",
                          sheet="YTD") %>%
  filter(SPECIES%in%c("CHINOOK SALMON", "BOAT TRIPS"), DISPOSITION%in%c("Kept", "Effort"), MONTH%in%c("July", "August", "September")) %>% 
  #mutate(subareas_catch = paste0(unique(CREEL_SUB_AREA), collapse=", ")) %>%
  group_by(YEAR, MONTH, PFMA, CREEL_SUB_AREA, SPECIES) %>% 
  summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T)) %>% 
  ungroup() %>%
  print()
