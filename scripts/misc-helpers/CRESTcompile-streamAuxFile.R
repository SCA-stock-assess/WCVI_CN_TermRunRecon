# aux stream lookup file for CREST Term group codings
# Feb 2024



# Load libraries ---------------------------
library(saaWeb)   




# Load NuSEDS query function ---------------------------
#source(here("scripts","functions","pullChinookNusedsData.R"))
  # not working



# Build stream-area aux file ---------------------------
# This is for applying the "area.origin" field so that we can group fish as being from "Other Area 23", "Other Area 25", etc.
streamAreas <- #left_join(
  # Load stream by area from NuSEDS query - a little slow.
  #pullNusedsData(here("scripts","json","nuseds_Chinook_stream-area.json"), password=NULL) %>%      # saaWeb bugged, manual load for now
  readxl::read_excel(path=here::here("data", "NuSEDS_streamAuxFile.xlsx"),
            sheet="Data") %>%
  group_by(`Waterbody Name`) %>% 
  summarize(Area=unique(Area)) %>% 
  mutate(Area = as.numeric(gsub('[A-z]+', '', Area)),                                                              # Remove the sub-area letter from Area name
         `Waterbody Name` = str_to_title(`Waterbody Name`)) %>%                                                    # Make waterbody name title case ('Title Case') so that it can match CREST
  rename(statarea.origin=Area,
         RESOLVED_STOCK_ORIGIN=`Waterbody Name`)%>%
  # Had to remove Salmon River in Fraser for now because it is causing issues:
  filter(RESOLVED_STOCK_ORIGIN!="Salmon River" & statarea.origin!=29) %>%                                                
  # Make some manual adjustments to names so that they match the CREST stock IDs:
  mutate(RESOLVED_STOCK_ORIGIN = case_when(RESOLVED_STOCK_ORIGIN=="Qualicum River" ~ "Big Qualicum River",           
                                           TRUE ~ as.character(RESOLVED_STOCK_ORIGIN)),
         RESOLVED_STOCK_ORIGIN = case_when(RESOLVED_STOCK_ORIGIN=="Tranquil Creek" ~ "Tranquil River",
                                           TRUE ~ as.character(RESOLVED_STOCK_ORIGIN)),
         RESOLVED_STOCK_ORIGIN = gsub("Toquart", "Toquaht", RESOLVED_STOCK_ORIGIN)) %>%
  ungroup() %>%
  # More manual adjustments... ugh... adding systems that are not in NuSEDS:
  add_row(RESOLVED_STOCK_ORIGIN="Robertson Creek", statarea.origin=23) %>%        
  add_row(RESOLVED_STOCK_ORIGIN="Omega Pacific Hatchery", statarea.origin=23) %>%
  print()