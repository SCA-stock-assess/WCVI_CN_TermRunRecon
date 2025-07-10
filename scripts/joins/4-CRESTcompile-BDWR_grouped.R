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
crestBDWR <- do.call("rbind", crestBDWR.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()


# Clean up ---------------------------
remove(crestBDWR.LL)



# ==================== LOAD STREAM AREA AUX FILE ==================== 
# NuSEDS ---------------------------
# This is for if we want roll up groups like "Other Area 23", "Other Area 25", etc.
# Should load pullNusedsData function and streamAreas dataframe: 
source(here::here("scripts", "misc-helpers", "CRESTcompile-streamLookups.R"))      
# saves as streamAreas


# ============== EXPORT the full compiled BDWR file ============== 
writexl::write_xlsx(crestBDWR, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/R_OUT - ",
                    min(crestBDWR$YEAR), "-", max(crestBDWR$YEAR),
                    "_Biological_Data_With_Results.xlsx"))



# ==================== LOAD TERM RUN REC SUBGROUP LOOKUP TABLE ==================== 
termRun_RecSubGroups <- readxl::read_excel(path=here::here("data", "lookups", "LOOKUP_CREST_PFMA-termRun-subgroup.xlsx"), 
                                           sheet="RRAreaLU")





#############################################################################################################################################################


#                                                                     CODE (OLD) CHINOOK TERM RUN GROUPS

# ============================= DEFINE HELPERS ============================= 

# Focal streams in each area to highlight ---------------------------
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later ---------------------------
stopwords <- c(" River", " Creek")

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
    # -------- Create updated RESOLVED AGE column to incorporate PBT
    `(R) Resolved total age` = case_when((is.na(RESOLVED_AGE) | RESOLVED_AGE==0) & PBT_BROOD_YEAR %in% PBT_BYs ~ YEAR-as.numeric(PBT_BROOD_YEAR),
                                         TRUE ~ RESOLVED_AGE),
    
    # -------- Create updated HATCHERY ORIGIN column to incorporate PBT and re-code for our terms
    `(R) Origin` = case_when(
      # If HATCHERY ORIGIN is a "Y", make it "Hatchery"
      HATCHERY_ORIGIN=="Y" ~ "Hatchery",
      # If PBT_BROOD_YEAR is a real year in the data, make it "Hatchery"
      PBT_BROOD_YEAR %in% PBT_BYs ~ "Hatchery",
      HATCHERY_ORIGIN!="Y" & PBT_BROOD_YEAR %notin% PBT_BYs ~ "Unknown",
      # PBT_BROOD_YEAR=="GSI 0000" & PBT_BROOD_YEAR=="Not Loaded" ~ "Unknown", 
      # If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural"
      ADIPOSE_FIN_CLIPPED=="N" & THERMALMARK=="Not Marked" ~ "Natural",
      # ************* need to add factor for if it's within the PBT baseline and it's NOT a PBT hit == natural***************
      # If it's none of these scenarios, make it "Unknown"
      TRUE ~ "Unknown"),
    
    # -------- Create updated Stock ID column to incorporate some missing otolith/CWT IDs 
    `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & 
                                              (!is.na(CWT_RESULT) & CWT_RESULT%notin%c("No Tag", "No Head", "Lost Tag") & !grepl("No Result", CWT_RESULT, ignore.case=T)) 
                                            ~ stringr::str_to_title(gsub("_", " ", CWT_RESULT)),
                                            TRUE ~ RESOLVED_STOCK_ROLLUP),
    `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & is.na(`(R) Resolved Stock Rollup`) & !is.na(OTO_STOCK) 
                                            ~ #sapply(OTO_STOCK, function(x) {
                                              #                         gsub(paste(c("H-", "S-", " R", " Cr"), collapse = "|"), , x)
                                              #                        }),
                                              stringr::str_to_title(OTO_STOCK),
                                            TRUE ~ `(R) Resolved Stock Rollup`)) %>% 
  
  mutate(
    # --- Add new CREST columns ---
    # -------- Create 'Term RR Roll Ups' column ---
    `(R) Term RR Roll Ups` = case_when(
      #2.0 Base case if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown"
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown", 
      #2.2 If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ROLLUP%notin%c("NWVI", "SWVI") ~ "NON-WCVI",
      #2.4 Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ORIGIN=="Tofino Hatchery" ~ "Tofino Hatchery (Bedwell?)",    
      #2.3 If it IS from NWVI or SWVI, this bit takes the stock ID from RESOLVED_STOCK_ORIGIN and removes 'creek' or 'river' so it just becomes uppercase BURMAN, CONUMA, etc.
      RESOLVED_STOCK_ROLLUP%in%c("NWVI", "SWVI") ~ toupper(gsub(paste0("\\b(",paste(stopwords, collapse="|"),")\\b"), "", RESOLVED_STOCK_ORIGIN))),
    
    
    # 3. Create 'TermSum' column ---
    `(R) Term Sum` = paste(`(R) Origin`, `(R) Term RR Roll Ups`, sep=" "),
    
    # 4. Create 'TermCON' column ---
    `(R) TermCON` = case_when(
      #4.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,
      #4.1 Identify all NON-WCVI stocks and carry through the "NON-WCVI" tag from '(R) Term Sum'
      `(R) Term RR Roll Ups`=="NON-WCVI" ~ `(R) Term Sum`,
      #4.2 Identify all of the focal rivers above that get their own group throughout the term RR process:
      `(R) Term RR Roll Ups`%in%focal_a25 ~ `(R) Term Sum`,
      #4.3 Identify all systems not in focal_a25 above, but still in area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `(R) Term RR Roll Ups`%notin%focal_a25 & statarea.origin==25 ~ paste(`(R) Origin`, "Other Area 25", sep=" "),
      #4.4 Same as above but for "Other Area 23"
      `(R) Term RR Roll Ups`%notin%focal_a25 & statarea.origin==23 ~ paste(`(R) Origin`, "Other Area 23", sep=" "),
      #4.5 Identify all systems NOT IN focal_a25, and also NOT assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" 
      `(R) Term RR Roll Ups`%notin%focal_a25 & statarea.origin%notin%c(23,25) ~ paste(`(R) Origin`, "Other WCVI", sep=" ")),
    
    # 5. Create TermNIT column ---
    `(R) TermNIT` = case_when(
      #5.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,
      #5.1 same as 4.1
      `(R) Term RR Roll Ups`=="NON-WCVI" ~ `(R) Term Sum`,
      #5.2 same as 4.2 but area 22
      `(R) Term RR Roll Ups`%in%focal_a22 ~ `(R) Term Sum`,
      #5.3 same as 4.3 but area 22
      `(R) Term RR Roll Ups`%notin%focal_a22 & statarea.origin==25 ~ paste(`(R) Origin`, "Other Area 25", sep=" "),
      #5.4 same as 4.4 but area 22
      `(R) Term RR Roll Ups`%notin%focal_a22 & statarea.origin==23 ~ paste(`(R) Origin`, "Other Area 23", sep=" "),
      #5.5 same as 4.5 but area 22
      `(R) Term RR Roll Ups`%notin%focal_a22 & statarea.origin%notin%c(23,25) ~ paste(`(R) Origin`, "Other WCVI", sep=" ")),
    
    # 6. Create TermArea23 column ---
    `(R) TermArea23` = case_when(
      #6.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `(R) Term Sum`,
      #6.1 same as 4.1
      `(R) Term RR Roll Ups`=="NON-WCVI" ~ `(R) Term Sum`,
      #6.2 same as 4.2
      `(R) Term RR Roll Ups`%in%focal_a23 ~ `(R) Term Sum`,
      #6.3 same as 4.3
      `(R) Term RR Roll Ups`%notin%focal_a23 & statarea.origin==25 ~ paste(`(R) Origin`, "Other Area 25", sep=" "),
      #6.4 same as 4.4  
      `(R) Term RR Roll Ups`%notin%focal_a23 & statarea.origin==23 ~ paste(`(R) Origin`, "Other Area 23", sep=" "),
      #6.5 same as 4.5
      `(R) Term RR Roll Ups`%notin%focal_a23 & statarea.origin%notin%c(23,25) ~ paste(`(R) Origin`, "Other WCVI", sep=" "))) %>%
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
remove(list=c("crestBDWR", "crestBDWR_CNgrouped", "streamAreas", "termRun_RecSubGroups", "focal_a22", "focal_a23", "focal_a25", "PBT_BYs", "R_OUT_CREST.Bio", 
              "stopwords", "%notin%"))




























