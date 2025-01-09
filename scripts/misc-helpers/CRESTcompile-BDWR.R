# crestCompile2 - out of TERM RUN 
# for BDWR compilation separate from term run (just here for record keeping for now)
# March 2024



# Load packages ---------------------------
#library(here)
library(tidyverse)
# library(readxl)
# library(writexl)



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
source(here::here("scripts", "misc-helpers", "CRESTcompile-streamAuxFile.R"))      
# saves as streamAreas


# ============== EXPORT the full compiled BDWR file ============== 
writexl::write_xlsx(crestBDWR, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/R_OUT - ",
                    min(crestBDWR$YEAR), "-", max(crestBDWR$YEAR),
                    "_Biological_Data_With_Results.xlsx"))

## temporary because VPN not working? 
writexl::write_xlsx(crestBDWR, 
                    path=paste0("C:/Users/DAVIDSONKA/Desktop/R_OUT - ",
                                min(crestBDWR$YEAR), "-", max(crestBDWR$YEAR),
                                "_Biological_Data_With_Results.xlsx"))



#############################################################################################################################################################

#                                                                           ADD VARIABLES
# 
# crestBDWRcompiled <- crestBDWR %>% 
#   mutate(`(R) Origin` = case_when(HATCHERY_ORIGIN=="Y" ~ "Hatchery",
#                                   THERMALMARK=="Not Marked" ~ "Natural (assumed)",
#                                   TRUE ~ "Unknown"),
#          `(R) RESOLVED ORIGIN-STOCK ID` = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1 <0.75 ~ paste0(`(R) Origin`, sep=" ", "Unknown (<75% GSI assignment)"),
#                                                     TRUE ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN)),
#          `(R) RESOLVED ORIGIN-REGION ID` = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1 <0.75 ~ paste0(`(R) Origin`, sep=" ", "Unknown (<75% GSI assignment)"),
#                                             TRUE ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP))) %>% 
#   print()
# 





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
  # --- Update existing CREST columns
  mutate(
    # 1. Create updated RESOLVED AGE column to incorporate PBT
    `(R) Resolved total age` = case_when((is.na(RESOLVED_AGE) | RESOLVED_AGE==0) & PBT_BROOD_YEAR %in% PBT_BYs ~ YEAR-as.numeric(PBT_BROOD_YEAR),
                                         TRUE ~ RESOLVED_AGE),
    
    # 2. Create updated HATCHERY ORIGIN column to incorporate PBT ---
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
    
    # 3. Fix Rollup to include missing IDs
    `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & 
                                              (!is.na(CWT_RESULT) & CWT_RESULT%notin%c("No Tag", "No Head", "Lost Tag") & !grepl("No Result", CWT_RESULT, ignore.case=T)) 
                                            ~ stringr::str_to_title(gsub("_", " ", CWT_RESULT)),
                                            TRUE ~ RESOLVED_STOCK_ROLLUP),
    `(R) Resolved Stock Rollup` = case_when(is.na(RESOLVED_STOCK_ROLLUP) & is.na(`(R) Resolved Stock Rollup`) & !is.na(OTO_STOCK) 
                                    ~ #sapply(OTO_STOCK, function(x) {
                                       #                         gsub(paste(c("H-", "S-", " R", " Cr"), collapse = "|"), , x)
                                        #                        }),
                                      stringr::str_to_title(OTO_STOCK),
                                    TRUE ~ `(R) Resolved Stock Rollup`),
    
    # 4. Assign WCVI/NON-WCVI (level 1) and identify "orphans"
    `(R) Term Run Group 1` = case_when(RESOLVED_STOCK_ROLLUP %in% c("SWVI", "NWVI") ~ paste(`(R) Origin`, sep=" ", "WCVI"),
                                       RESOLVED_STOCK_ROLLUP %notin% c("SWVI", "NWVI") & !is.na(RESOLVED_STOCK_ROLLUP) ~ paste(`(R) Origin`, sep=" ", "Non-WCVI"),
                                       is.na(RESOLVED_STOCK_ROLLUP) & !is.na(`(R) Resolved Stock Rollup`) ~ "Manual rollup required - use '(R) Resolved Stock Rollup' column ",
                                       is.na(`(R) Resolved Stock Rollup`) ~ paste(`(R) Origin`, sep=" ", "Unknown"),
                                       TRUE ~ "FLAG"),
    `(R) Term Run Group 2` = case_when(grepl(" WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP),
                                       grepl("Non-WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP),
                                       TRUE ~ paste(`(R) Term Run Group 1`)),
    `(R) Term Run Group 3` = case_when(!is.na(RESOLVED_STOCK_ORIGIN) ~ paste(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN),
                                       is.na(RESOLVED_STOCK_ORIGIN) & !is.na(`(R) Resolved Stock Rollup`) ~ paste0(`(R) Origin`, sep=" ", `(R) Resolved Stock Rollup`),
                                       is.na(RESOLVED_STOCK_ORIGIN) & is.na(`(R) Resolved Stock Rollup`) ~ paste(`(R) Origin`, sep=" ", "Unknown"), 
                                       TRUE ~ "FLAG"),
    `(R) Term Run Group 4` = case_when(grepl(" WCVI", `(R) Term Run Group 1`) ~ paste(`(R) Term Run Group 3`, " (WCVI)"),
                                       TRUE ~ `(R) Term Run Group 3`))
    
    

# **** here next: how to group? do we have to stick to old groupings?? 





#############################################################################################################################################################

#                                                                           EXPORT 


# ==================== CREATE EXCEL FILE ==================== 

# Create Workbook ---------------------------
R_OUT_CREST.Bio <- openxlsx::createWorkbook()

# Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.Bio, "CREST Biodata Compiled")



# Add data to tabs ---------------------------
openxlsx::writeData(R_OUT_CREST.Bio, sheet="CREST Biodata Compiled", x=crestBDWR_CNgrouped)





# ==================== EXPORT EXCEL FILE ==================== 

# To github ---------------------------
openxlsx::saveWorkbook(R_OUT_CREST.Bio,
                       file=paste0(here("outputs"),
                                   "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                                   min(crestBDWR_CNgrouped$YEAR),
                                   "-",
                                   max(crestBDWR_CNgrouped$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To Network: 
openxlsx::saveWorkbook(R_OUT_CREST.Bio, 
                       file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                                   min(crestBDWR_CNgrouped$YEAR),
                                   "-",
                                   max(crestBDWR_CNgrouped$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


write.csv(crestBDWR_CNgrouped, 
          file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R", 
                      "/R_OUT - Biological_Data_With_Results (WCVI GROUPED) ",
                      min(crestBDWR_CNgrouped$YEAR),
                      "-",
                      max(crestBDWR_CNgrouped$YEAR),
                      ".csv"), row.names=F)
































