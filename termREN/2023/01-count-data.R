# Compiling enumeration data 
# July 2025


# set up ----------------
library(tidyverse)
analysis_year <- 2023
"%notin%" <- Negate("%in%")


# ==================== TERMINAL CATCH ====================

# FIRST NATIONS -----------------------

  # Not currently a database to pull FN catch from - need to reach out to Pacheedaht directly

# FSC (Marine & Freshwater) ---------
fsc <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/FIRST_NATIONS/CATCH_DATA/PACHEEDAHT/PFN FSC catch ALL YRS.xlsx",
                                 sheet=1) %>%
  filter(#Year==analysis_year, 
         Area==20, `Sub-Area`%in%c("20-1","20-2","20-3"), Species=="Chinook") %>%
  group_by(Year, Location, Area) %>%
  summarize(Count = sum(`Pcs Kept`)) %>%
  ungroup() %>%
  rename(#TermRun_spatial_substrata = `Sub-Area`,
         TermRun_sector02 = Location,
         TermRun_spatial_strata=Area,
         TermRun_Year=Year) %>%
  mutate(across(everything(), as.character),
         TermRun_sector01 = "First Nations",
         TermRun_temp_strata = "July-September",
         TermRun_sex_strata = "Total",
         TermRun_sector02 = paste0("FSC - ", TermRun_sector02),
         TermRun_spatial_strata = case_when(grepl("In-River", TermRun_sector02, ignore.case=T) ~ "Freshwater",
                                            TRUE ~ TermRun_spatial_strata),
         TermRun_spatial_substrata = case_when(grepl("Marine", TermRun_sector02, ignore.case=T) ~ "20-1, 20-2, 20-3"),
         TermRun_sector00="Terminal Fisheries") %>%
  print()


#(No EO)


# RECREATIONAL -----------------------
# Port Renfrew: Areas 20E + 20A + 20B
rec <- readxl::read_excel(path="//ent.dfo-mpo.ca/dfo-mpo/GROUP/PAC/Reg_Shares/FHM/SMCS/Salmon/FMCR_Fishery_Monitoring_Catch_Reporting/Recreational_CM/Catch_Data/SC Sport Catch Creel Sub-area Disposition (Master Do Not Edit).xlsx",
                          sheet="YTD") %>%
  filter(SPECIES%in%c("CHINOOK SALMON", "BOAT TRIPS"), DISPOSITION%in%c("Kept", "Effort"), MONTH%in%c("July", "August", "September"), 
         CREEL_SUB_AREA %in%c("20A", "20B", "20E", "Area 20 (West)", "Area 20 (WCVI)")) %>% 
  mutate(subareas_catch = paste0(unique(CREEL_SUB_AREA), collapse=", ")) %>%
  group_by(YEAR, MONTH, SPECIES) %>% 
  summarize(monthly_catch_estimate = sum(ESTIMATE, na.rm=T), subareas_catch=unique(subareas_catch)) %>% 
  ungroup() %>%
  pivot_wider(names_from = SPECIES, values_from = monthly_catch_estimate) %>% 
  mutate(`CHINOOK SALMON` = case_when((!is.na(`BOAT TRIPS`) | `BOAT TRIPS`>0) & is.na(`CHINOOK SALMON`) ~ 0,
                                      TRUE ~ `CHINOOK SALMON`)) %>% 
  pivot_longer(cols=c(`CHINOOK SALMON`, `BOAT TRIPS`), names_to = "SPECIES", values_to = "monthly_catch_estimate") %>% 
  filter(SPECIES=="CHINOOK SALMON") %>%
  select(-c(SPECIES)) %>%
  rename(TermRun_Year=YEAR,
         TermRun_temp_strata=MONTH,
         TermRun_spatial_substrata=subareas_catch,
         Count=monthly_catch_estimate) %>%
  mutate(TermRun_sector01="Recreational",
         TermRun_sector02="Area 20 Terminal",
         TermRun_spatial_strata = 20,
         TermRun_sector00="Terminal Fisheries") %>%
  mutate(across(everything(), as.character)) %>%
  print()



# COMMERCIAL GILLNET -----------------------
# No fishery

# COMMERCIAL SEINE NET -----------------------
# No fishery







# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



# ==================== ESCAPEMENT ====================

# HATCHERY BROODSTOCK -----------------------
broodstock <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R/",
                                                 pattern="^Adult_Management_93-4MILE_AllStocks-AllSpecies",
                                                 full.names = T),
                                 sheet=1, skip=1) %>% 
  janitor::clean_names() %>%
  mutate(TermRun_Year = as.numeric(stringr::str_sub(spawning_stock, start=1, end=4))) %>%
  filter(activity_type %in% c("Adult Selection", "Mortalities")) %>% 
  group_by(TermRun_Year, maturity_class) %>%
  summarize(Count = sum(total_count, na.rm=T)) %>%
  #mutate(total=sum(Count)) %>%
  ungroup() %>%
  rename(TermRun_sex_strata=maturity_class) %>% 
  mutate(TermRun_sector00 = "Escapement",
         TermRun_sector01 = "Escapement - mainstem",
         TermRun_sector02 = "Broodstock, morts, other",
         across(everything(), as.character),
         TermRun_spatial_strata = "Seine, fence",
         TermRun_temp_strata="September,October") %>%
  print()


# HATCHERY FIRST SET SEX RATIO -----------------------
# If available
first_set <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R/",
                                                pattern="^First_Set_93-4MILE_AllStocks-AllSpecies",
                                                full.names = T),
                                sheet=1, skip=1) %>%
  mutate(TermRun_Year = lubridate::year(Date)) %>%
  pivot_longer(cols=c(Female:Total), names_to = "Sex", values_to = "Count") %>% 
  filter(Sex!="Total") %>% 
  group_by(TermRun_Year, Sex) %>%
  summarize(Count=sum(Count)) %>% 
  mutate(propn=Count/sum(Count)) %>%
  ungroup() %>%
  select(-c(Count)) %>%
  rename(TermRun_sex_strata = Sex,
         Count=propn) %>% 
  mutate(TermRun_sector00 = "Escapement",
         TermRun_sector01 = "Escapement - sex/age correction",
         TermRun_sector02 = "Actual sex ratio (from hatchery staff)",
         TermRun_temp_strata = "September,October",
         across(everything(), as.character)) %>% 
  print()



# SAN JUAN RIVER NATURAL SPAWNERS -----------------------
# Can't use New Esc Index for broodstock as it's not split by M/F
spawners <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/ESCAPEMENT/Data/NEW ESCAPEMENT INDEX.xls",
                                 sheet="EscData", skip=4) %>%
  filter(Area==20) %>% 
  mutate(`Ck Escape` = case_when(Year==2015 & System=="Gordon" ~ 0,
                                 TRUE ~ as.numeric(`Ck Escape`))) %>%
  group_by(Year, System) %>%
  summarize(broodstock = sum(`Brd Rem Ck`, na.rm=T),
            total_esc = sum(`Ck Escape`, na.rm=T)) %>%
  mutate(natural_spawners = total_esc-broodstock) %>%
  ungroup() %>%
  pivot_longer(c(broodstock:natural_spawners), names_to = "data_type", values_to = "Count") %>%
  filter(data_type%notin%c("total_esc", "broodstock")) %>%
  mutate(TermRun_sector01 = case_when(System=="San Juan" ~ "Escapement - mainstem",
                                      System=="Gordon" ~ "Escapement - other"),
         TermRun_sector02 = case_when(data_type=="natural_spawners" ~ "Natural spawners"),
         TermRun_sector00="Escapement",
         TermRun_sex_strata="Adults",
         TermRun_temp_strata="September,October") %>%
  select(-c(data_type)) %>%
  rename(TermRun_Year = Year,
         TermRun_spatial_strata = System) %>%
  mutate(across(everything(), as.character)) %>%
  print()







# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



# ==================== ESCAPEMENT: OTHER ====================

# LENS CREEK -----------------------


# HARRIS CREEK -----------------------


# RENFREW CREEK -----------------------


# FAIRY CREEK -----------------------


# GORDON RIVER -----------------------
# Dealt with above in "spawners" 


# OTHER SYSTEMS (List) -----------------------





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# INPUT INTO MAPPING FILE


# ==================== READ MAPPING FILE & JOIN ====================

compiled_counts <- bind_rows(fsc, rec, broodstock, first_set, spawners) 

mapping_out <- readxl::read_excel(path=here::here("termREN", analysis_year, 
                                                   paste0("TERMREN_mapping_", analysis_year, "_tt.xlsx")),
                                   sheet="termREN_map", skip=1) %>%
  mutate(across(everything(), as.character)) %>%
  left_join(compiled_counts) %>%
  relocate(Count, .before=`AGES_borrowed?`) %>%
  mutate(Count = case_when(is.na(Count) ~ "",
                           TRUE ~ Count)) %>%
  print()



# ==================== EXPORT MAPPING FILE ====================

MAPPING.OUT <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb=MAPPING.OUT, "Sheet1")
openxlsx::writeData(wb=MAPPING.OUT, sheet="Sheet1", x=mapping_out,
                    headerStyle = openxlsx::createStyle(textDecoration = "bold"))
openxlsx::addStyle(wb=MAPPING.OUT, sheet="Sheet1", cols=11:19, rows=1:(nrow(mapping_out)+1), style=openxlsx::createStyle(fgFill= "#FFF2CC"), gridExpand=T)
openxlsx::addStyle(wb=MAPPING.OUT, sheet="Sheet1", cols=c(13,14,16), rows=8:11, style=openxlsx::createStyle(fgFill= "#D9D9D9"), gridExpand=T)
openxlsx::conditionalFormatting(wb=MAPPING.OUT, sheet="Sheet1", cols=10, rows=2:(nrow(mapping_out)+1), rule="J2==\"\"", 
                                style=openxlsx::createStyle(bgFill="yellow"))
openxlsx::saveWorkbook(MAPPING.OUT, file=here::here("termREN", analysis_year, paste0("R_OUT - TERMREN_mapping_", analysis_year, "-output_from_00.xlsx")), 
                       overwrite = T)                   



