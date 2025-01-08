# crestCompile2 - out of TERM RUN 
# for BDWR compilation separate from term run (just here for record keeping for now)
# March 2024



# Load packages ---------------------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)



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




#############################################################################################################################################################

#                                                                           ADD VARIABLES

crestBDWRcompiled <- crestBDWR %>% 
  mutate(`(R) Origin` = case_when(HATCHERY_ORIGIN=="Y" ~ "Hatchery",
                                  THERMALMARK=="Not Marked" ~ "Natural (assumed)",
                                  TRUE ~ "Unknown"),
         `(R) RESOLVED ORIGIN-STOCK ID` = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1 <0.75 ~ paste0(`(R) Origin`, sep=" ", "Unknown (<75% GSI assignment)"),
                                                    TRUE ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN)),
         `(R) RESOLVED ORIGIN-REGION ID` = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1 <0.75 ~ paste0(`(R) Origin`, sep=" ", "Unknown (<75% GSI assignment)"),
                                            TRUE ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP))) %>% 
  print()


#############################################################################################################################################################


#                                                                     CODE (OLD) CHINOOK TERM RUN GROUPS

# ============================= DEFINE HELPERS ============================= 

# Focal streams in each area to highlight ---------------------------
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later ---------------------------
stopwords <- c(" River", " Creek")




# ============================= CODE TERM RUN GROUPS =============================

crestBDWR_CNgrouped <-
  # Join CREST biodata and streamAreas aux file ----
left_join(crestBDWR, 
          streamAreas) %>% 
  filter(SPECIES==124) %>%
  mutate(
    # 1. Create 'Hat/Nat' column ---
    `(R) Origin` = case_when(
      #1.1 If HATCHERY ORIGIN is a "Y", make it "Hatchery"
      HATCHERY_ORIGIN=="Y" ~ "Hatchery",
      # If PBT_BROOD_YEAR is not 0 or blank, make it "Hatchery"
      PBT_BROOD_YEAR!=0 & !is.na(PBT_BROOD_YEAR) ~ "Hatchery",
      #1.2 If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural"
      ADIPOSE_FIN_CLIPPED=="N" & THERMALMARK=="Not Marked" ~ "Natural",
      # ************* need to add factor for if it's within the PBT baseline and it's NOT a PBT hit == natural***************
      #1.3 If it's neither of these scenarios, make it "Unknown"
      TRUE ~ "Unknown"))








writexl::write_xlsx(crestBDWR_grouped, "C:/Users/DAVIDSONKA/Desktop/crestBDWR_grouped test.xlsx")


#############################################################################################################################################################

#                                                                           EXPORT 


# ==================== CREATE EXCEL FILE ==================== 

# Create Workbook ---------------------------
R_OUT_CREST.Bio <- openxlsx::createWorkbook()

# Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.Bio, "CREST Biodata Compiled")



# Add data to tabs ---------------------------
openxlsx::writeData(R_OUT_CREST.Bio, sheet="CREST Biodata Compiled", x=crestBiocompiled)





# ==================== EXPORT EXCEL FILE ==================== 

# To github ---------------------------
openxlsx::saveWorkbook(R_OUT_CREST.Bio,
                       file=paste0(here("outputs"),
                                   "/R_OUT - Biological_Data_With_FOS ",
                                   min(crestBiocompiled$YEAR),
                                   "-",
                                   max(crestBiocompiled$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To Network: 
openxlsx::saveWorkbook(R_OUT_CREST.Bio, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - Biological_Data_With_FOS ",
                                   min(crestBiocompiled$YEAR),
                                   "-",
                                   max(crestBiocompiled$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


write.csv(crestBiocompiled, 
          file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R", 
                      "/R_OUT - Biological_Data_With_FOS ",
                      min(crestBiocompiled$YEAR),
                      "-",
                      max(crestBiocompiled$YEAR),
                      ".csv"), row.names=F)
































