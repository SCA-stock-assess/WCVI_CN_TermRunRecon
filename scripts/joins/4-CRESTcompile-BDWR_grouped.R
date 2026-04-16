
# To compile multiple areas and years of Biological Data With Results reports from CREST, and to re-group/summarize them for use in run reconstruction
# March 2024

# Work flow is:
# 1.1. Download most recent year BDWR report from CREST (and also maybe last year, just to refresh) for all WCVI creel areas 
#       1.2. Store new file(s) on Network drive location (if you are refreshing a year, e.g., re-downloading 2024 in 2025, delete last year's old 2024 version): "Y:\WCVI\CHINOOK\WCVI_TERMINAL_RUN\Annual_data_summaries_for_RunRecons\CREST-BDWRcompile_base-files\1-Import-to-R"
# 2.   Load CREST files into R from Network drive  
# 3.   Load some look-up tables used to group the rec sub-areas and assign origin PFMAs to stock IDs
# 4.   Define a few helper variables/group things for final stock assignments 
# 5.   Roll up the stock assignments based on previously established groups 
# 6.   Export to git and network for subsequent use in run reconstructions 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== BEFORE YOU START: ====================
## Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects from Environment including hidden objects.
gc()                            # free up memory and report the memory usage.

# ***** MANUAL UPATE: 
analysis_year <- 2025


## Load packages & helpers ----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== LOAD CREST BDWR BASE FILES (2017-present data) ==================== 


## Read CREST files as large list ---------------------------
# Load base files to compile
crestBDWR.LL <- lapply(list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                 pattern="^[0-9]{4}_Biological_Data_With_Results_.*\\.xlsx$", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="Biological_Data_With", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBDWR.LL) <- list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                  pattern="^[0-9]{4}_Biological_Data_With_Results_.*\\.xlsx$", full.names=F)


## Convert the Large List into a useable R dataframe ---------------------------
crestBDWR <- crestBDWR.LL %>%
  #do.call("rbind", crestBDWR.LL) %>%
  #tibble::rownames_to_column(var="file_source") %>%
  reduce(full_join) %>%
  print()


## Clean up ---------------------------
remove(crestBDWR.LL)


# ==================== EXPORT the full compiled BDWR time series (2017-present) ====================
# Just an export of the full dataset without the manipulation below in case useful for other purposes - not really relevant for term run
writexl::write_xlsx(crestBDWR, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/R_OUT - ",
                                min(crestBDWR$YEAR), "-", max(crestBDWR$YEAR),
                                "_Biological_Data_With_Results.xlsx"))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== LOAD LOOKUP TABLES ==================== 


## Stream-Area auxiliary file ---------------------------
# This is for if we want roll up groups like "Other Area 23", "Other Area 25", etc. we need to know the PFMA associated with the source population
# We may be moving away from this method due to the new GSI groupings. Have retained this for now but in future might be redundant.
# Should load pullNusedsData function and streamAreas dataframe: 
source(here::here("scripts", "misc-helpers", "CRESTcompile-streamLookups.R"))      
# --> saves as streamAreas



## Term Run rec sub-group lookup table ---------------------------
# Dictates which PFMAs correspond to, e.g., Inner Barkley, Outer Barkley, Barkley Corridor, etc. 
# Compiled by PLB
termRun_RecSubGroups <- readxl::read_excel(path=here::here("data", "lookups", "LOOKUP_CREST_PFMA-termRun-subgroup.xlsx"), 
                                           sheet="RRAreaLU")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ============================= DEFINE SOME HELPING VARIABLES/GROUPS ============================= 
# Used in the final grouping stage below 

## Focal streams in each area to highlight ---------------------------
# As mentioned above, this is probably a bit outdated with new GSI groups
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
#focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")   # added gold, leiner, tahsis Aug 2025 PLB request
#focal_a25XTRA <- c("GOLD", "LEINER", "TAHSIS")   # added gold, leiner, tahsis Aug 2025 PLB request
#focal_a25ALL <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")    


## Used to remove the river/creek suffix later ---------------------------
# This is needed just for cleaning up the stock ID names so that "SAN JUAN" and "SAN JUAN RIVER" are called the same thing for later use in pivot tables
stopwords <- c(" River", " Creek")


## Pull out vector of logical PBT BYs present in data ---------------------------
# This identifies "real" PBT broodyears in CREST - some PBT_brood_year results are given as "0", or "Not Loaded" so we want to identify a string of years in the data that are actually REAL PBT results to assign hatchery origin later
PBT_BYs <- crestBDWR %>% 
  filter(PBT_BROOD_YEAR %notin% c("0", "Not Loaded") & !is.na(PBT_BROOD_YEAR) & !grepl("GSI", PBT_BROOD_YEAR)) %>% 
  group_by(PBT_BROOD_YEAR) %>%
  summarize() %>%
  pull(PBT_BROOD_YEAR)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ============================= CODE TERM RUN GROUPS =============================

crestBDWR_CNgrouped <- left_join(crestBDWR, 
                                 streamAreas) %>% 
  filter(SPECIES==124) %>%
  
  mutate(
    # Create updated RESOLVED_AGE column to incorporate PBT ages (weren't being pulled in in 2025)
    `(R) Resolved BY` = case_when(!is.na(CWT_BROOD_YEAR) ~ CWT_BROOD_YEAR,
                                  !is.na(PBT_BROOD_YEAR) & PBT_BROOD_YEAR %in% PBT_BYs ~ as.numeric(PBT_BROOD_YEAR),
                                  !is.na(AGE_GR) & AGE_GR!=0 ~ YEAR - RESOLVED_AGE),
    
    `(R) Resolved Age` = YEAR - `(R) Resolved BY`,

    
    # Create updated HATCHERY_ORIGIN column to incorporate PBT and re-code for our terms (ALSO SEE MORE ON THIS BELOW - ~Line 163)
    `(R) Origin` = case_when(HATCHERY_ORIGIN == "Y" ~ "Hatchery",                                                                                                      # If HATCHERY_ORIGIN is a "Y", make it "Hatchery"
                             PBT_BROOD_YEAR %in% PBT_BYs ~ "Hatchery",                                                                                               # If PBT_BROOD_YEAR is a real year in the data, make it "Hatchery"
                             HATCHERY_ORIGIN != "Y" & PBT_BROOD_YEAR %notin% PBT_BYs ~ "Unknown",                                                                      # If HATCHERY_ORIGIN isn't "Y" and the given PBT BY isn't in the logical list of BYs, make it Unknown
                             ADIPOSE_FIN_CLIPPED == "N" & THERMALMARK == "Not Marked" & !grepl("san juan|nahmint|thornton|toquart|tahsis|leiner", RESOLVED_STOCK_ORIGIN) ~ "Natural",                   # If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural", except for 2 stock-specific cases we know of that are not thermally marked on WCVI
                             #ADIPOSE_FIN_CLIPPED == "N" & grepl("columbia|puget|washington", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "Natural",                           # Can't assume this due to DIT releases 
                             TRUE ~ "Unknown"),                                                                                                                      # If it's none of these scenarios, make it "Unknown"
    
    # Create updated RESOLVED_STOCK_ORIGIN column to incorporate 386 missing otolith/CWT IDs 
    `(R) Resolved stock origin` = case_when(is.na(RESOLVED_STOCK_ORIGIN) &
                                            !grepl("no result|no tag|no head|lost tag", CWT_RESULT, ignore.case=T) ~ 
                                              stringr::str_to_title(gsub("_", " ", CWT_RESULT)),        # Identify fish that have no RESOLVED_STOCK_ORIGIN, but do have a logical CWT tag code, and apply the CWT Stock ID result
                                          TRUE ~ RESOLVED_STOCK_ORIGIN),
    `(R) Resolved stock origin` = case_when(is.na(RESOLVED_STOCK_ORIGIN) & is.na(`(R) Resolved stock origin`) & 
                                              (grepl("no result|no tag|no head|lost tag", CWT_RESULT, ignore.case=T) | is.na(CWT_RESULT)) &
                                            !is.na(OTO_STOCK) ~ gsub("H-", "", gsub(pattern="S-", replacement="", stringr::str_to_title(OTO_STOCK))),                         # Identify fish that have no RESOLVED_STOCK_ORIGIN, but do have a RESOLVED_STOCK_SOURCE and some sort of logical otolith stock result, and apply the Otolith Stock ID result
                                          TRUE ~ RESOLVED_STOCK_ORIGIN),
    `(R) Resolved stock origin` = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PBT_BROOD_YEAR %notin% PBT_BYs & PROB_1 < 0.8 ~ "Unknown",
                                            TRUE ~ `(R) Resolved stock origin`)) %>%
  
  mutate(
    # Create 'Term RR Roll Ups' column: this is just to identify NON-WCVI from the WCVI-specific stocks of interest 
    `(R) Term RR Roll Ups` = case_when(
      is.na(`(R) Resolved stock origin`) ~ "Unknown",                                                                                                          # Base case: if is.na(`(R) Resolved stock origin`) make it "Unknown" stock ID
      RESOLVED_STOCK_ROLLUP %notin% c("NWVI", "SWVI") & !is.na(RESOLVED_STOCK_ROLLUP) | 
        grepl("chickaloon|esquimalt|nimpkish|puntledge|sooke", `(R) Resolved stock origin`, ignore.case=T) ~ "NON-WCVI",                                       # If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ORIGIN == "Tofino Hatchery" ~ "Tofino Hatchery (Bedwell?)",                                                                               # Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ROLLUP %in% c("NWVI", "SWVI") & grepl("nitinat|robertson|conuma|tahsis", `(R) Resolved stock origin`, ignore.case=T) ~ 
        toupper(gsub(paste0("\\b(",paste(stopwords, collapse="|"),")\\b"), "", `(R) Resolved stock origin`)),                                                  # If it IS from NWVI or SWVI, this bit takes the stock ID from RESOLVED_STOCK_ORIGIN and removes 'creek' or 'river' so it just becomes uppercase BURMAN, CONUMA, etc.
      TRUE ~ `(R) Resolved stock origin`),                                                                                                                     # If none of these, default to the `(R) Resolved stock origin`, which may need manual tweaking each year (but at least results won't be excluded)

    
    # Revise the `(R) Resolved origin` column using the `(R) Term RR Roll Ups` column for PBT natural based on tag rates - updated Apr 2026 to allow for 1P tag rates
    `(R) Resolved origin` = case_when(`(R) Origin` != "Hatchery" & grepl("bedwell", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2016,2019) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("burman", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2014,2019,2021,2022,2023,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("conuma", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2020,2021,2022,2023,2024,2025) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("gold", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2022,2023,2025) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("leiner", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("marble", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2022) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("nahmint", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2018,2019,2020,2023,2024,2025) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("nitinat", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021,2022,2023,2024,2025) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("robertson", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2013,2015,2019,2020,2021,2023,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("san juan", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021,2022,2023,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("sarita", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2013,2015,2019,2020,2021,2022,2023,2024,2025) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("tahsis", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021,2022,2023,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("thornton", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2023,2024) ~ "Natural* (this population should not have natural production)",
                                      `(R) Origin` != "Hatchery" & grepl("toquart", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2023,2024) ~ "Natural",
                                      TRUE ~ `(R) Origin`),
    
    
    # Create 'TermRollup - GSI Grouped' column: Using Term RR Roll Ups, apply our new GSI stock groupings
    `(R) Term Rollup - GSI Grouped` = case_when(
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("san juan", `(R) Term RR Roll Ups`, ignore.case=T) ~ "SAN JUAN",                                                                    # If stock ID is assigned by GSI and that result says San Juan, make it San Juan
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("sooke|nitinat", `(R) Term RR Roll Ups`, ignore.case=T) ~ "SOOKE/NITINAT",                                                          # If stock ID is assigned by GSI and that result says Sooke or Nitinat, make it Sooke/Nitinat
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("sarita", `(R) Term RR Roll Ups`, ignore.case=T) ~ "SARITA",
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("toquart|thornton", `(R) Term RR Roll Ups`, ignore.case=T) ~ "OUTER BARKLEY",                                                       # If stock ID is assigned by GSI and that result says Toquart or Thornton, make it Outer Barkley
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("nahmint", `(R) Term RR Roll Ups`, ignore.case=T) ~ "NAHMINT",                                                                      # If stock ID is assigned by GSI and that result says Nahmint, make it Nahmint
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("stamp|robertson|gold", `(R) Term RR Roll Ups`, ignore.case=T) ~ "INNER BARKLEY",                                                   # If stock ID is assigned by GSI and that result says Stamp, Robertson or Gold, make it Inner Barkley
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("tranquil|kennedy|cypre|bedwell", `(R) Term RR Roll Ups`, ignore.case=T) ~ "CLAYOQUOT",                                             # If stock ID is assigned by GSI and that result says Tranquil, Kennedy, Cypre or Bedwell, make it Inner Barkley
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("zeballos|tlupana|tahsis|tahsish|moyeha|megin|leiner|kaouk|conuma|burman", `(R) Term RR Roll Ups`, ignore.case=T) ~                 # If stock ID is assigned by GSI and that result says one of these many Area 26/26 systems, make it Nootka/Kyuquot
        "NOOTKA/KYUQUOT",
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("marble", `(R) Term RR Roll Ups`, ignore.case=T) ~ "MARBLE",                                                                        # If stock ID is assigned by GSI and that result says Marble, make it Marble
      RESOLVED_STOCK_SOURCE == "DNA" & `(R) Resolved origin` != "Hatchery" & grepl("colonial|cayeghle|cayeagle", `(R) Term RR Roll Ups`, ignore.case=T) ~ "COLONIAL-CAYEGHLE",                                         # If stock ID is assigned by GSI and that result says Colonial or Cayeghle, make it Colonial-Cayeghle
      TRUE ~ `(R) Term RR Roll Ups`),                                                                                                                                                                                  # If none of these cases apply, default to the Term RR Roll Ups column assignment
    
    
    # Create 'TermSum' column: this simply pastes the origin (hat/nat) to the Roll Up created above (for general use) ---
    `(R) Term Sum` = paste(`(R) Resolved origin`, `(R) Term Rollup - GSI Grouped`, sep=" "),
    

    # Create 'TermCON' column: This is the TermCON file specific stock IDs to be used ---
    `(R) TermCON` = case_when(
      is.na(`(R) Resolved stock origin`) ~ `(R) Term Sum`,                                                                                                  # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                                # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      grepl("BEDWELL|BURMAN|CONUMA|KAOUK|MARBLE|NITINAT|ROBERTSON|SAN JUAN",`(R) Term RR Roll Ups`, ignore.case=T) ~ `(R) Term Sum`,                                                                                 # Identify all of the focal Area 25 rivers above that get their own group throughout the term RR process                                            
      grepl("seapen", `(R) Term RR Roll Ups`, ignore.case=T) ~ "Hatchery Tahsis River H",                                                                   # Manual fix for the pesky seapen fish
      `(R) Resolved origin` == "Hatchery" & grepl("gold|leiner|tahsis",`(R) Term RR Roll Ups`, ignore.case=T) ~ `(R) Term Sum`,                             # NEW- for PLB Aug 2025. Pull out hatchery Leiner, Tahsis and Gold specifically
      !grepl("BEDWELL|BURMAN|CONUMA|KAOUK|MARBLE|NITINAT|ROBERTSON|SAN JUAN",`(R) Term RR Roll Ups`, ignore.case=T) & statarea.origin == 25 ~ 
        paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                                                                                             # Identify all systems not in focal_a25 above, but still in Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      !grepl("BEDWELL|BURMAN|CONUMA|KAOUK|MARBLE|NITINAT|ROBERTSON|SAN JUAN",`(R) Term RR Roll Ups`, ignore.case=T) & statarea.origin == 23 ~ 
        paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                                                                                             # Identify all systems not in focal_a25 above, and from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      !grepl("BEDWELL|BURMAN|CONUMA|KAOUK|MARBLE|NITINAT|ROBERTSON|SAN JUAN",`(R) Term RR Roll Ups`, ignore.case=T) & statarea.origin %notin% c(23,25) ~ 
        paste(`(R) Resolved origin`, "Other WCVI", sep=" ")),                                                                                               # Identify all systems not in focal_a25, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
                                                                                                                                                              

    # Create TermNIT column: This is the TermNIT file specific stock IDs to be used ---
    `(R) TermNIT` = case_when(
      is.na(`(R) Resolved stock origin`) ~ `(R) Term Sum`,                                                                                              # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                      # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      `(R) Term RR Roll Ups` %in% focal_a22 ~ `(R) Term Sum`,                                                                                     # Identify all of the focal Area 22 rivers above that get their own group throughout the term RR process
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin == 25 ~ paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                  # Identify all systems not in focal_a22, but are from Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin == 23 ~ paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                  # Identify all systems not in focal_a22, but are from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin %notin% c(23,25) ~ paste(`(R) Resolved origin`, "Other WCVI", sep=" ")),         # Identify all systems not in focal_a22, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
    

    # Create TermArea23 column: This is the TermArea23 file specific stock IDs to be used ---
    `(R) TermArea23` = case_when(
      is.na(`(R) Resolved stock origin`) ~ `(R) Term Sum`,                                                                                            # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                    # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      `(R) Term RR Roll Ups` %in% focal_a23 ~ `(R) Term Sum`,                                                                                   # Identify all of the focal Area 23 rivers above that get their own group throughout the term RR process
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin == 25 ~ paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                # Identify all systems not in focal_a23, but are from Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin == 23 ~ paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                # Identify all systems not in focal_a23, but are from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin %notin% c(23,25) ~ paste(`(R) Resolved origin`, "Other WCVI", sep=" ")),       # Identify all systems not in focal_a23, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
    

  # New TermSum column 2026 - to assess the impact of assuming all Unknown [WCVI] are Natural JUST FOR 2025
  `(R) TERM SUM 2026` = case_when(YEAR==2025 & `(R) Resolved origin`=="Unknown" & 
                                    !is.na(`(R) Term Rollup - GSI Grouped`) & `(R) Term Rollup - GSI Grouped` %notin% c("NON-WCVI", "Unknown") ~
                                    paste0("Natural (assumed) ", `(R) Term Rollup - GSI Grouped`),
                                  TRUE ~ `(R) Term Sum`)
  ) %>%
  relocate("(R) Resolved origin", .after=`(R) Origin`) %>% 
  relocate("(R) TERM SUM 2026", .after=`(R) Term Sum`) %>%
  print()
  



#   # 4. Assign WCVI/NON-WCVI (level 1) and identify "orphans"
#   `(R) Term Run Group 1` = case_when(RESOLVED_STOCK_ROLLUP %in% c("SWVI", "NWVI") ~ paste(`(R) Origin`, sep=" ", "WCVI"),
#                                      RESOLVED_STOCK_ROLLUP %notin% c("SWVI", "NWVI") & !is.na(RESOLVED_STOCK_ROLLUP) ~ paste(`(R) Origin`, sep=" ", "Non-WCVI"),
#                                      is.na(RESOLVED_STOCK_ROLLUP) & !is.na(`(R) Resolved Stock Rollup`) ~ "Manual rollup required - use '(R) Resolved Stock Rollup' column ",
#                                      is.na(`(R) Resolved Stock Rollup`) ~ paste(`(R) Origin`, sep=" ", "Unknown"),
#                                      TRUE ~ "FLAG"),
# `(R) Term Run Group 2` = case_when(grepl(" WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP),
#                                    grepl("Non-WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP),
#                                    TRUE ~ paste(`(R) Term Run Group 1`)),
# `(R) Term Run Group 3` = case_when(!is.na(RESOLVED_STOCK_ORIGIN) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN),
#                                    is.na(RESOLVED_STOCK_ORIGIN) & !is.na(`(R) Resolved Stock Rollup`) ~ paste0(`(R) Origin`, sep=" ", `(R) Resolved Stock Rollup`),
#                                    is.na(RESOLVED_STOCK_ORIGIN) & is.na(`(R) Resolved Stock Rollup`) ~ paste(`(R) Origin`, sep=" ", "Unknown"), 
#                                    TRUE ~ "FLAG"),
# `(R) Term Run Group 4` = case_when(grepl(" WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Term Run Group 3`, " (WCVI)"),
#                                    TRUE ~ `(R) Term Run Group 3`))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ============================= JOIN TO TERM RUN GROUP LOOKUP TABLE =============================

# Join the compiled BDWR to the Rec sub-groups lookup file made by Piper-Lynn
# TBH I don't know why I did this here when I already did it above, but lets just keep it here cuz the code works
crestBDWR_CNgrouped.recSubGroups <- left_join(crestBDWR_CNgrouped,
                                              termRun_RecSubGroups,
                                              by="SUBAREA")
  

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ============================= CREATE EXCEL WORKBOOK =============================

## Create empty Excel Workbook ---------------------------
R_OUT_CREST.Bio <- openxlsx::createWorkbook()

## Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.Bio, "CREST Biodata Compiled")


## Add data to tabs ---------------------------
openxlsx::writeData(R_OUT_CREST.Bio, sheet="CREST Biodata Compiled", x=crestBDWR_CNgrouped.recSubGroups)



# ==================== EXPORT EXCEL FILE ==================== 

# To github ---------------------------
openxlsx::saveWorkbook(R_OUT_CREST.Bio,
                       file=paste0(here::here("outputs"),
                                   "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                                   min(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   "-",
                                   max(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To Network ---------------------------
# As Excel:
openxlsx::saveWorkbook(R_OUT_CREST.Bio, 
                       file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                                   min(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   "-",
                                   max(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)

# As csv: 
write.csv(crestBDWR_CNgrouped.recSubGroups,
          file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R",
                      "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                      min(crestBDWR_CNgrouped.recSubGroups$YEAR),
                      "-",
                      max(crestBDWR_CNgrouped.recSubGroups$YEAR),
                      ".csv"), row.names=F)



#/ END!



# Clean up for purposes of source() calls 
#remove(list=c("crestBDWR", "crestBDWR_CNgrouped", "streamAreas", "termRun_RecSubGroups", "focal_a22", "focal_a23", "focal_a25XTRA", "focal_a25ALL", "PBT_BYs", "R_OUT_CREST.Bio", 
#              "stopwords", "%notin%"))




























