# crestCompile2 - out of TERM RUN 
# for BDWR compilation separate from term run (just here for record keeping for now)
# March 2024



# Load packages ---------------------------
library(tidyverse)



# Helpers ---------------------------
"%notin%" <- Negate("%in%")




#############################################################################################################################################################


# ==================== LOAD CREST BIODATA WITH RESULTS BASE FILES (2017-present data) ==================== 


# Read CREST files as large list ---------------------------
# Load base files to compile
crestBDWR.LL <- lapply(list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                 pattern="^[0-9]{4}_Biological_Data_With_Results_.*\\.xlsx$", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="Biological_Data_With", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBDWR.LL) <- list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                  pattern="^[0-9]{4}_Biological_Data_With_Results_.*\\.xlsx$", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
crestBDWR <- crestBDWR.LL %>%
  #do.call("rbind", crestBDWR.LL) %>%
  #tibble::rownames_to_column(var="file_source") %>%
  reduce(full_join) %>%
  print()


# Clean up ---------------------------
remove(crestBDWR.LL)



# ==================== LOAD STREAM AREA AUX FILE ==================== 
# NuSEDS ---------------------------
# This is for if we want roll up groups like "Other Area 23", "Other Area 25", etc.
# Should load pullNusedsData function and streamAreas dataframe: 
source(here::here("scripts", "misc-helpers", "CRESTcompile-streamLookups.R"))      
# saves as streamAreas


# ============== EXPORT the full compiled BDWR time series (2017-present) ============== 
writexl::write_xlsx(crestBDWR, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/R_OUT - ",
                    min(crestBDWR$YEAR), "-", max(crestBDWR$YEAR),
                    "_Biological_Data_With_Results.xlsx"))



# ==================== LOAD TERM RUN REC SUBGROUP LOOKUP TABLE ==================== 
# Dictates which PFMAs correspond to, e.g., Inner Barkley, Outer Barkley, Barkley Corridor, etc. 
termRun_RecSubGroups <- readxl::read_excel(path=here::here("data", "lookups", "LOOKUP_CREST_PFMA-termRun-subgroup.xlsx"), 
                                           sheet="RRAreaLU")





#############################################################################################################################################################


#                                                                     CREATE CHINOOK TERM RUN GROUPS

# ============================= DEFINE HELPING VARS/GROUPS ============================= 

# Focal streams in each area to highlight (old) ---------------------------
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
#focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")   # added gold, leiner, tahsis Aug 2025 PLB request
focal_a25XTRA <- c("GOLD", "LEINER", "TAHSIS")   # added gold, leiner, tahsis Aug 2025 PLB request
focal_a25ALL <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")    


# Used to remove the river/creek suffix later ---------------------------
stopwords <- c(" River", " Creek")


# Pull out vector of logical PBT BYs present in data ---------------------------
PBT_BYs <- crestBDWR %>% 
  filter(PBT_BROOD_YEAR %notin% c("0", "Not Loaded") & !is.na(PBT_BROOD_YEAR) & !grepl("GSI", PBT_BROOD_YEAR)) %>% 
  group_by(PBT_BROOD_YEAR) %>%
  summarize() %>%
  pull(PBT_BROOD_YEAR)






# ============================= CODE TERM RUN GROUPS =============================

crestBDWR_CNgrouped <-
  # Join CREST biodata and streamAreas aux file ----
left_join(crestBDWR, 
          streamAreas) %>% 
  filter(SPECIES==124) %>%
  
  mutate(
    # --- Update existing CREST columns ---
    # -------- Create updated RESOLVED_AGE column to incorporate PBT
    `(R) Resolved BY` = case_when(!is.na(CWT_BROOD_YEAR) ~ CWT_BROOD_YEAR,
                                  !is.na(PBT_BROOD_YEAR) & PBT_BROOD_YEAR %in% PBT_BYs ~ as.numeric(PBT_BROOD_YEAR),
                                  !is.na(AGE_GR) & AGE_GR!=0 ~ YEAR - RESOLVED_AGE),
    
    `(R) Resolved Age` = YEAR - `(R) Resolved BY`,

    
    # -------- Create updated HATCHERY_ORIGIN column to incorporate PBT and re-code for our terms (ALSO SEE MORE ON THIS BELOW - ~Line 163)
    `(R) Origin` = case_when(HATCHERY_ORIGIN == "Y" ~ "Hatchery",                                                                                                      # If HATCHERY_ORIGIN is a "Y", make it "Hatchery"
                             PBT_BROOD_YEAR %in% PBT_BYs ~ "Hatchery",                                                                                               # If PBT_BROOD_YEAR is a real year in the data, make it "Hatchery"
                             HATCHERY_ORIGIN != "Y" & PBT_BROOD_YEAR %notin% PBT_BYs ~ "Unknown",                                                                      # If HATCHERY_ORIGIN isn't "Y" and the given PBT BY isn't in the logical list of BYs, make it Unknown
                             ADIPOSE_FIN_CLIPPED == "N" & THERMALMARK == "Not Marked" & !grepl("san juan|nahmint|thornton|toquart|tahsis|leiner", RESOLVED_STOCK_ORIGIN) ~ "Natural",                   # If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural", except for 2 stock-specific cases we know of that are not thermally marked on WCVI
                             TRUE ~ "Unknown"),                                                                                                                      # If it's none of these scenarios, make it "Unknown"
    
    # -------- Create updated RESOLVED_STOCK_ORIGIN column to incorporate 386 missing otolith/CWT IDs 
    # `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & 
    #                                           (!is.na(CWT_RESULT) & CWT_RESULT%notin%c("No Tag", "No Head", "Lost Tag") & !grepl("No Result", CWT_RESULT, ignore.case=T)) 
    #                                         ~ stringr::str_to_title(gsub("_", " ", CWT_RESULT)),
    #                                         TRUE ~ RESOLVED_STOCK_ROLLUP),
    # `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & is.na(`(R) Resolved Stock Rollup`) & !is.na(OTO_STOCK) 
    #                                         ~ stringr::str_to_title(OTO_STOCK),
    #                                         TRUE ~ `(R) Resolved Stock Rollup`)) %>% 
    
    `(R) Resolved stock origin` = case_when(is.na(RESOLVED_STOCK_ORIGIN) & !is.na(RESOLVED_STOCK_SOURCE) &
                                            (!is.na(CWT_RESULT) & CWT_RESULT %notin% c("No Tag", "No Head", "Lost Tag") & 
                                               !grepl("No Result", CWT_RESULT, ignore.case=T)) ~ stringr::str_to_title(gsub("_", " ", CWT_RESULT)),        # Identify fish that have no RESOLVED_STOCK_ORIGIN, but do have a RESOLVED_STOCK_SOURCE and some sort of logical CWT tag code, and apply the CWT Stock ID result
                                          TRUE ~ RESOLVED_STOCK_ORIGIN),
    `(R) Resolved stock origin` = case_when(is.na(RESOLVED_STOCK_ORIGIN) & !is.na(RESOLVED_STOCK_SOURCE) & is.na(`(R) Resolved stock origin`) & 
                                            !is.na(OTO_STOCK) ~ gsub("H-", "", gsub("S-", "S", stringr::str_to_title(OTO_STOCK))),                         # Identify fish that have no RESOLVED_STOCK_ORIGIN, but do have a RESOLVED_STOCK_SOURCE and some sort of logical otolith stock result, and apply the Otolith Stock ID result
                                          TRUE ~ RESOLVED_STOCK_ORIGIN)) %>%
  
  mutate(
    # --- Add new columns ---
    # -------- Create 'Term RR Roll Ups' column: this is just to identify NON-WCVI from the WCVI-specific stocks of interest ---
    `(R) Term RR Roll Ups` = case_when(
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown",                                                                                                            # Base case: if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown" stock ID
      RESOLVED_STOCK_ROLLUP %notin% c("NWVI", "SWVI") ~ "NON-WCVI",                                                                                        # If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ORIGIN == "Tofino Hatchery" ~ "Tofino Hatchery (Bedwell?)",                                                                             # Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ROLLUP %in% c("NWVI", "SWVI") ~ toupper(gsub(paste0("\\b(",paste(stopwords, collapse="|"),")\\b"), "", `(R) Resolved stock origin`))),        # If it IS from NWVI or SWVI, this bit takes the stock ID from RESOLVED_STOCK_ORIGIN and removes 'creek' or 'river' so it just becomes uppercase BURMAN, CONUMA, etc.

    
    # -------- Revise the `(R) Origin column` using the `(R) Term RR Roll Ups` column for PBT natural based on tag rates ---
    `(R) Resolved origin` = case_when(`(R) Origin` != "Hatchery" & grepl("bedwell", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2016,2019) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("burman", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2014,2015,2023) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("conuma", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2020,2021,2022,2024) ~ "Natural",
                                      #`(R) Origin` != "Hatchery" & grepl("gold", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2020,2023) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("leiner", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2014,2019,2020,2021,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("marble", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2022) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("nahmint", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2016,2019,2020) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("nitinat", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("robertson", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2013,2015,2019,2020,2021,2023,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("san juan", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2022,2023) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("sarita", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2013,2015,2016,2017,2019,2020,2021,2022,2023) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("tahsis", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2021,2024) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("thornton", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2023,2024) ~ "Natural* (this population should not have natural production)",
                                      `(R) Origin` != "Hatchery" & grepl("tlupana", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2014) ~ "Natural",
                                      `(R) Origin` != "Hatchery" & grepl("toquart", `(R) Term RR Roll Ups`, ignore.case=T) & `(R) Resolved BY` %in% c(2019,2020,2023) ~ "Natural",
                                      TRUE ~ `(R) Origin`),
    
    
    # -------- Create 'TermRollup - GSI Grouped' column: Using Term RR Roll Ups, apply our new GSI stock groupings ---
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
    
    
    # -------- Create 'TermSum' column: this simply pastes the origin (hat/nat) to the Roll Up created above (for general use) ---
    `(R) Term Sum` = paste(`(R) Resolved origin`, `(R) Term Rollup - GSI Grouped`, sep=" "),
    

    
    
    # -------- Create 'TermCON' column: This is the TermCON file specific stock IDs to be used ---
    `(R) TermCON` = case_when(
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,                                                                                             # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                     # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      `(R) Term RR Roll Ups` %in% focal_a25ALL ~ `(R) Term Sum`,                                                                                 # Identify all of the focal Area 25 rivers above that get their own group throughout the term RR process                                            
      `(R) Resolved origin` == "Hatchery" & `(R) Term RR Roll Ups` %in% focal_a25XTRA ~ `(R) Term Sum`,                                          # NEW- for PLB Aug 2025. Pull out hatchery Leiner, Tahsis and Gold specifically
      `(R) Term RR Roll Ups` %notin% focal_a25ALL & statarea.origin==25 ~ paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                # Identify all systems not in focal_a25 above, but still in Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups` %notin% focal_a25ALL & statarea.origin==23 ~ paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                # Identify all systems not in focal_a25 above, and from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      `(R) Term RR Roll Ups` %notin% focal_a25ALL & statarea.origin %notin% c(23,25) ~ paste(`(R) Resolved origin`, "Other WCVI", sep=" ")),     # Identify all systems not in focal_a25, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
    
    

    # -------- Create TermNIT column: This is the TermNIT file specific stock IDs to be used ---
    `(R) TermNIT` = case_when(
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,                                                                                              # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                      # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      `(R) Term RR Roll Ups` %in% focal_a22 ~ `(R) Term Sum`,                                                                                     # Identify all of the focal Area 22 rivers above that get their own group throughout the term RR process
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin == 25 ~ paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                  # Identify all systems not in focal_a22, but are from Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin == 23 ~ paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                  # Identify all systems not in focal_a22, but are from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      `(R) Term RR Roll Ups` %notin% focal_a22 & statarea.origin %notin% c(23,25) ~ paste(`(R) Resolved origin`, "Other WCVI", sep=" ")),         # Identify all systems not in focal_a22, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
    
    
    
    # -------- Create TermArea23 column: This is the TermArea23 file specific stock IDs to be used ---
    `(R) TermArea23` = case_when(
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,                                                                                            # Base case unknown stock ID
      `(R) Term RR Roll Ups` == "NON-WCVI" ~ `(R) Term Sum`,                                                                                    # Identify all NON-WCVI stocks and carry through the "NON-WCVI" ID from '(R) Term Sum'
      `(R) Term RR Roll Ups` %in% focal_a23 ~ `(R) Term Sum`,                                                                                   # Identify all of the focal Area 23 rivers above that get their own group throughout the term RR process
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin == 25 ~ paste(`(R) Resolved origin`, "Other Area 25", sep=" "),                # Identify all systems not in focal_a23, but are from Area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin == 23 ~ paste(`(R) Resolved origin`, "Other Area 23", sep=" "),                # Identify all systems not in focal_a23, but are from Area 23 (using "statarea.origin" created above) and make them "Other Area 23"
      `(R) Term RR Roll Ups` %notin% focal_a23 & statarea.origin %notin% c(23,25) ~ paste(`(R) Resolved origin`, "Other WCVI", sep=" "))) %>%   # Identify all systems not in focal_a23, and also not assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" and simply give them "Other WCVI"
    

    # `(R) TERM WCVI NEW` = case_when(RESOLVED_STOCK_ROLLUP %in% c("SWVI", "NWVI") ~ paste0(`(R) Resolved origin`, " WCVI"),
    #                                 TRUE ~ "NON-WCVI")) %>%
  
  relocate("(R) Resolved origin", .after=`(R) Origin`) %>% 
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

  
  
#############################################################################################################################################################

#                                                                    JOIN TO TERM RUN GROUP LOOKUP TABLE 
# made by Piper-Lynn

# Join the compiled BDWR to the Rec sub-groups
crestBDWR_CNgrouped.recSubGroups <- left_join(crestBDWR_CNgrouped,
                                              termRun_RecSubGroups,
                                              by="SUBAREA")
  


#############################################################################################################################################################

#                                                                           EXPORT 


# ==================== CREATE EXCEL FILE ==================== 

# Create Workbook ---------------------------
R_OUT_CREST.Bio <- openxlsx::createWorkbook()

# Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.Bio, "CREST Biodata Compiled")



# Add data to tabs ---------------------------
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


# To Network: 
openxlsx::saveWorkbook(R_OUT_CREST.Bio, 
                       file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                                   min(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   "-",
                                   max(crestBDWR_CNgrouped.recSubGroups$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


write.csv(crestBDWR_CNgrouped.recSubGroups,
          file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R",
                      "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                      min(crestBDWR_CNgrouped.recSubGroups$YEAR),
                      "-",
                      max(crestBDWR_CNgrouped.recSubGroups$YEAR),
                      ".csv"), row.names=F)

















# Clean up for purposes of source() calls -------------------
remove(list=c("crestBDWR", "crestBDWR_CNgrouped", "streamAreas", "termRun_RecSubGroups", "focal_a22", "focal_a23", "focal_a25XTRA", "focal_a25ALL", "PBT_BYs", "R_OUT_CREST.Bio", 
              "stopwords", "%notin%"))




























