
# Code WCVI Terminal Run Reconstruction stock ID groups in CREST Biodata output 

# Work flow is:
# 1.1.  
#       1.2.  


# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)


# Helpers -------------
"%notin%" <- Negate("%in%")
analysis_year <- 2022



################################################################################################################################################

#                                                                           I. CREST FILE LOAD


# Load files from Sharepoint -------------
crest.files <- lapply(list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                       "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/CREST/"), 
                                pattern = "_WCVI_Chinook_Run_Reconstruction_", full.names = T), 
                     readxl::read_excel,
                     guess_max=20000)
# Should be a Large List of multiple elements

# Change filenames in the List -------------
names(crest.files) <- list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                       "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/CREST/"), 
                                pattern = "_WCVI_Chinook_Run_Reconstruction_", full.names = F)


# Convert the Large List into a useable R dataframe ---------------------------
wcviCNcrest <- do.call("rbind", crest.files) %>%
  #filter(Spawning.Stock !="") %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()
remove(crest.files)


# Export to git and SP as a backup for future use (NOT NEEDED) ---------------------------
# # To git:
# writexl::write_xlsx(wcviCNcrest, path=paste0(here("outputs"), sep="/", "R_OUT - WCVI CN CREST BIODATA w RESULTS 2021-2022.xlsx"))
# 
# 
# # To SP: 
# writexl::write_xlsx(wcviCNcrest, path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
#                                                 "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/CREST/R_OUT - WCVI CN CREST BIODATA w RESULTS 2021-2022.xlsx"))
# 




#############################################################################################################################################################

#                                                                           I. BUILD STREAM AUX FILE


# Source NuSEDS query function ---------------------------
source(here("scripts","functions","getNusedsData.R"))


# Build stream-area aux file ---------------------------
# This is for applying the "area.origin" field so that we can group fish as being from "Other Area 23", "Other Area 25", etc.
streamAreas <- #left_join(
  # Load stream by area from NuSEDS query - a little slow.
  getNuSEDS(here("scripts","json","nuseds_stream-area.json"), password=NULL) %>% 
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



#############################################################################################################################################################

#                                                                           II. CODE TERM RUN GROUPS

# Define helper variables ---------------------------
# Focal streams in each area to highlight
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later
stopwords <- c(" River", " Creek")


# ======================== CODE TERM RUN GROUPS ========================
wcviCNcrest_coded <- 
  # Join CREST biodata and streamAreas aux file ---------------------------
left_join(wcviCNcrest, 
          streamAreas) %>% 
  
  mutate(
    #1. Create 'Hat/Nat' column ---------------------------
    `Hat/Nat (R)` = case_when(
      #1.1 If HATCHERY ORIGIN is a "Y", make it "Hatchery"
      HATCHERY_ORIGIN=="Y" ~ "Hatchery",
      #1.2 If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural"
      ADIPOSE_FIN_CLIPPED=="N" & THERMALMARK=="Not Marked" ~ "Natural",
      #1.3 If it's neither of these scenarios, make it "Unknown"
      TRUE ~ "Unknown"),
    
    #2. Create 'Term RR Roll Ups' column ---------------------------
    `Term RR Roll Ups (R)` = case_when(
      #2.0 Base case if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown"
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown", 
      #2.2 If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ROLLUP%notin%c("NWVI", "SWVI") ~ "NON-WCVI",
      #2.4 Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ORIGIN=="Tofino Hatchery" ~ "BEDWELL",
      #2.3 If it IS from NWVI or SWVI, this bit takes the stock ID from RESOLVED_STOCK_ORIGIN and removes 'creek' or 'river' so it just becomes uppercase BURMAN, CONUMA, etc.
      RESOLVED_STOCK_ROLLUP%in%c("NWVI", "SWVI") ~ toupper(gsub(paste0("\\b(",paste(stopwords, collapse="|"),")\\b"), "", RESOLVED_STOCK_ORIGIN))),
    
    
    #3. Create 'TermSum' column ---------------------------
    `Term Sum (R)` = paste(`Hat/Nat (R)`, `Term RR Roll Ups (R)`, sep=" "),
    
    #4. Create 'TermCON' column ---------------------------
    `TermCON (R)` = case_when(
      #4.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #4.1 Identify all NON-WCVI stocks and carry through the "NON-WCVI" tag from 'Term Sum (R)'
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #4.2 Identify all of the focal rivers above that get their own group throughout the term RR process:
      `Term RR Roll Ups (R)`%in%focal_a25 ~ `Term Sum (R)`,
      #4.3 Identify all systems not in focal_a25 above, but still in area 25 (using "statarea.origin" created above) and make them "Other Area 25"
      `Term RR Roll Ups (R)`%notin%focal_a25 & statarea.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #4.4 Same as above but for "Other Area 23"
      `Term RR Roll Ups (R)`%notin%focal_a25 & statarea.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #4.5 Identify all systems NOT IN focal_a25, and also NOT assigned to "NON-WCVI", "Other Area 25", or "Other Area 23" 
      `Term RR Roll Ups (R)`%notin%focal_a25 & statarea.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")),
    
    #5. Create TermNIT column ---------------------------
    `TermNIT (R)` = case_when(
      #5.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #5.1 same as 4.1
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #5.2 same as 4.2 but area 22
      `Term RR Roll Ups (R)`%in%focal_a22 ~ `Term Sum (R)`,
      #5.3 same as 4.3 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & statarea.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #5.4 same as 4.4 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & statarea.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #5.5 same as 4.5 but area 22
      `Term RR Roll Ups (R)`%notin%focal_a22 & statarea.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")),
    
    #6. Create TermArea23 column ---------------------------
    `TermArea23 (R)` = case_when(
      #6.0 Base case unknown stock ID
      is.na(RESOLVED_STOCK_ORIGIN) ~ `Term Sum (R)`,
      #6.1 same as 4.1
      `Term RR Roll Ups (R)`=="NON-WCVI" ~ `Term Sum (R)`,
      #6.2 same as 4.2
      `Term RR Roll Ups (R)`%in%focal_a23 ~ `Term Sum (R)`,
      #6.3 same as 4.3
      `Term RR Roll Ups (R)`%notin%focal_a23 & statarea.origin==25 ~ paste(`Hat/Nat (R)`, "Other Area 25", sep=" "),
      #6.4 same as 4.4  
      `Term RR Roll Ups (R)`%notin%focal_a23 & statarea.origin==23 ~ paste(`Hat/Nat (R)`, "Other Area 23", sep=" "),
      #6.5 same as 4.5
      `Term RR Roll Ups (R)`%notin%focal_a23 & statarea.origin%notin%c(23,25) ~ paste(`Hat/Nat (R)`, "Other WCVI", sep=" ")) ,
    
    # Create QC flag columns to see cases where Term labels don't match ---------------------------
    `qcFlag_TermCON=TermNIT?` = case_when(`TermCON (R)` == `TermNIT (R)` ~ "TRUE"),
    `qcFlag_TermCON=Term23?` = case_when(`TermCON (R)` == `TermArea23 (R)` ~ "TRUE"),
    `qcFlag_TermNIT=Term23?` = case_when(`TermNIT (R)` == `TermArea23 (R)` ~ "TRUE")
  ) %>%
  print()



#############################################################################################################################################################

#                                                                           VII. QC 


# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source EPRO files:",
                             "source NPAFC file:",
                             "CWT source:",
                             "assumptions made:", 
                             "",
                             "",
                             "sheet name:",
                             "WCVI CN CREST Biodata CODED",
                             "QC summary",
                             "qc1_otoNoSample",
                             "qc2_scaleNoAge",
                             "qc3_CWTnoID",
                             "qc4_CWTzero",
                             "qc5_WmanNoSample",
                             "qc6_CWTDNAdisagree",
                             "qc7_otoDNAdisagree",
                             "qc8_DNAuncert",
                             "qc9_PBTmaybe",
                             "qc10_susSUS",
                             "qc11_ageDisagree",
                             "qc12_nonstdSex"),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/2-EPRO_biodata_with_results.R", 
        "https://086gc.sharepoint.com/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=QSeYb8&cid=a94075b0%2D307f%2D43b2%2D96e6%2D02a093c68a9a&FolderCTID=0x01200009EB148EBFDA544E816AF000384149AC&id=%2Fsites%2FPAC%2DSCAStockAssessmentSTAD%2FShared%20Documents%2FWCVI%20STAD%2FTerminal%20CN%20Run%20Recon%2F2022%2FCommunal%20data%2FEPRO&viewid=931f98e0%2Da6b1%2D48c6%2D9fee%2D65ba9363ce0e",
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx",
        "http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice",  
        "Removed AK and Kamchatka marks from NPAFC file to avoid duplicates, assuming strays are only BC/SUS",
        "Ignored one case where RCH and Nanaimo hatchery applied the H5 same mark in 2018; assume it was a RCH mark that showed up in 2022 Burman broodstock",
        "",
        "sheet description:",
        "All EPRO facilities 2022 'All Adult Biosampling' reports for WCVI combined into 1 file and joined to 1. the NPAFC mark file to give otolith stock ID and 2. CWT releases for last 10 years to give CWT stock ID.",
        "Summary of QC flags and # of entries belonging to that flag.",
        "QC flags subsequent tabs. See QC Summary page for details."))


# QC flags ---------------------------
qc1_otoNoSample <- wcviCNcrest_coded %>% 
  filter(!is.na(OTOLITH_BOX) & !is.na(OTOLITH_SPECIMEN) & THERMALMARK=="No Sample")


qc2_scaleNoAge <- wcviCNcrest_coded %>% 
  filter(is.na(RESOLVED_AGE) & is.na(PART_AGE_CODE) & !is.na(SCALE_BOOK))


qc3_CWTnoID <- wcviCNcrest_coded %>% 
  filter(!is.na(CWT_HEAD_LABEL) & is.na(CWT_RESULT))


qc4_CWTzero <- wcviCNcrest_coded %>% 
  filter(CWT_HEAD_LABEL==0)


qc5_WmanNoSample <- crest_biodata %>% 
  filter(!is.na(SPECIMEN_REFERENCE_DNA_NO) & is.na(DNA_RESULTS_STOCK_1) | DNA_RESULTS_STOCK_1=="NO SAMPLE" ) 


qc6_CWTDNAdisagree <- crest_biodata %>% 
  filter(RESOLVED_STOCK_SOURCE=="CWT" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME)


qc7_otoDNAdisagree <- crest_biodata %>% 
  filter(RESOLVED_STOCK_SOURCE=="Otolith Stock" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME)


qc8_DNAuncert <- crest_biodata %>% 
  filter(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1<0.8)


qc9_PBTmaybe <- crest_biodata %>% 
  filter(is.na(HATCHERY_ORIGIN) & PROB_1==1 & is.na(DNA_STOCK_2))


qc10_susSUS <- crest_biodata %>% 
  filter(RESOLVED_STOCK_ORIGIN=="SUS (assumed)" & 
           !is.na(CWT_RESULT) & CWT_RESULT!="No Tag" & 
           !is.na(THERMALMARK) & !THERMALMARK%in%c("No Sample","Not Marked") &
           !is.na(DNA_RESULTS_STOCK_1) & DNA_RESULTS_STOCK_1!="NO SAMPLE") 


qc11_ageDisagree <- crest_biodata %>%
  filter((YEAR-CWT_BROOD_YEAR)!=RESOLVED_AGE) 


qc12_nonstdSex <- crest_biodata %>%
  filter(SEX %notin% c("M","F"))




## non-matching IDs


# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc1_otoNoSample",
                                         "qc2_scaleNoAge",
                                         "qc3_CWTnoID",
                                         "qc4_CWTzero",
                                         "qc5_WmanNoSample",
                                         "qc6_CWTDNAdisagree",
                                         "qc7_otoDNAdisagree",
                                         "qc8_DNAuncert",
                                         "qc9_PBTmaybe",
                                         "qc10_susSUS",
                                         "qc11_ageDisagree",
                                         "qc12_nonstdSex"
                                         ),
                         number_records = c(nrow(qc1_noOtoID),
                                            nrow(qc2_noOtoResults),
                                            nrow(qc3_noCWTID),
                                            nrow(qc4_noRslvdID),
                                            nrow(qc5_WmanNoSample),
                                            nrow(qc6_CWTDNAdisagree),
                                            nrow(qc7_otoDNAdisagree),
                                            nrow(qc8_DNAuncert),
                                            nrow(qc9_PBTmaybe),
                                            nrow(qc10_susSUS),
                                            nrow(qc11_ageDisagree),
                                            nrow(qc12_nonstdSex)
                                            ),
                         description = c("Otolith box/vial numbers exist but there is 'No Sample'",
                                         "Scale book and number but no age, and no explanation given (e.g., resorbed etc)",
                                         "A CWT head label was submitted but the stock ID result is blank",
                                         "CWT head label entered as '0'",
                                         "Whatman sheet/DNA tracking numbers exist but no GSI result (blank)",
                                         "Stock ID assigned by CWT but GSI (>=80%) disagrees",
                                         "Stock ID assigned by otolith thermal mark but GSI >=80% disagrees",
                                         "A stock ID assigned by DNA but <80% certainty",
                                         "Hatchery origin given as a blank, but some possibility for PBT (PROB=1.00). Note this should be approached with lots of caution and is stock-specific.",
                                         "SUS (assumed) that have ID methods available - A CWT head label was submitted but the stock ID result is blank",
                                         "Cases where the RESOLVED_AGE does not match the catch YEAR minus the CWT_BROOD_YEAR",
                                         "Sex designation does not fall as M or F - propose changing all other designations to 'unk'"
                                         )) %>% 
  print()


#############################################################################################################################################################

#                                                                           VIII. EXPORT 


# Export ---------------------------
# Create a blank workbook
R_OUT_EPRO.NPAFC <- openxlsx::createWorkbook()

# Add sheets to the workbook
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "readme")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "AllFacilities w RESULTS")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC summary")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "qc1 - No Oto stock ID")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "qc2 - No Oto result")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "qc3 - No CWT ID")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "qc4 - No Reslvd ID")

# Write data to the sheets
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="AllFacilities w RESULTS", x=wcviCNepro_w_Results2022)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="QC summary", x=qc_summary)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "qc1 - No Oto stock ID", x=qc1_noOtoID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "qc2 - No Oto result", x=qc2_noOtoResults)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "qc3 - No CWT ID", x=qc3_noCWTID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "qc4 - No Reslvd ID", x=qc4_noRslvdID)


# Export to git and SP ---------------------------
# To git:
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0(here("outputs"), 
                                   sep="/", 
                                   "R_OUT - All EPRO facilities master WITH RESULTS.xlsx"),
                       overwrite=T,
                       returnValue=T)


# To SP: 
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0("C:/Users", sep="/", 
                                   Sys.info()[6], 
                                   sep="/",
                                   "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/R_OUT - All EPRO facilities master WITH RESULTS.xlsx"),
                       overwrite=T,
                       returnValue=T)





# /END!




