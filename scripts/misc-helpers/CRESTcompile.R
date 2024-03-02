# CRESTcompile
# compile CREST biodata outputs for CN Run Recon  **assumes everything is from the "WCVI CHINOOK ONLY biodata..." report
# Then assign term rollup stock groups
# Feb 2024





# Load packages ---------------------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(saaWeb)    # for pullNusedsData in source() script to make stream aux file 



# Helpers ---------------------------
"%notin%" <- Negate("%in%")




#############################################################################################################################################################


# ==================== 1. LOAD CREST CHINOOK BIODATA BASE FILES (2021-present data) ==================== 


# Read CREST files as large list ---------------------------
# Load base files to compile
crestBio.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/1-Import-to-R", 
                                 pattern="*WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS*.*xlsx", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="WCVI_Chinook_Run_Rec", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBio.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/1-Import-to-R", 
                                 pattern="*WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS*.*xlsx", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
crestBio <- do.call("rbind", crestBio.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()


# Clean up ---------------------------
remove(crestBio.LL)




#############################################################################################################################################################

# ==================== 2. LOAD STREAM AUX FILE ==================== 
# This is for if we want roll up groups like "Other Area 23", "Other Area 25", etc.

# Should load pullNusedsData function and streamAreas dataframe: 
source(here("scripts", "misc-helpers", "CRESTcompile-streamAuxFile.R"))




#############################################################################################################################################################

# ==================== 3. CODE TERM RUN GROUPS ==================== 


# Define helper variables ---------------------------

# Focal streams in each area to highlight
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later
stopwords <- c(" River", " Creek")




# CODE TERM RUN GROUPS ---------------------------

crestBio_grouped <- 
  # Join CREST biodata and streamAreas aux file ----
left_join(crestBio, 
          streamAreas) %>% 
  
  mutate(
    # 1. Create 'Hat/Nat' column ---
    `(R) Origin` = case_when(
      #1.1 If HATCHERY ORIGIN is a "Y", make it "Hatchery"
      HATCHERY_ORIGIN=="Y" ~ "Hatchery",
      #1.2 If it's not clipped and the thermal mark indicates "Not Marked", make it "Natural"
      ADIPOSE_FIN_CLIPPED=="N" & THERMALMARK=="Not Marked" ~ "Natural",
      #1.3 If it's neither of these scenarios, make it "Unknown"
      TRUE ~ "Unknown"),
    
    # 2. Create 'Term RR Roll Ups' column ---
    `(R) Term RR Roll Ups` = case_when(
      #2.0 Base case if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown"
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown", 
      #2.2 If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ROLLUP%notin%c("NWVI", "SWVI") ~ "NON-WCVI",
      #2.4 Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ORIGIN=="Tofino Hatchery" ~ "BEDWELL",
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



#############################################################################################################################################################


# ==================== 3. QC & README ==================== 


# QC flags ---------------------------



# Otolith sample and age data (for stock ID) available but no result (possible oto processing error)
qc1_otoNoSample <- crestBio_grouped %>% 
  filter(!is.na(OTOLITH_BOX) & !is.na(OTOLITH_SPECIMEN) & !is.na(RESOLVED_AGE) & THERMALMARK!="Not Marked")

# Scale sample available but no result (possible scale processing error)
qc2_scaleNoAge <- crestBio_grouped %>% 
  filter(is.na(RESOLVED_AGE) & is.na(PART_AGE_CODE) & !is.na(SCALE_BOOK) & !is.na(SCALE_NO))

# DNA sample available but no DNA result (possible DNA processing error)
qc5_WmanNoSample <- crestBio_grouped %>% 
  filter(!is.na(SPECIMEN_REFERENCE_DNA_NO) & is.na(DNA_RESULTS_STOCK_1) | DNA_RESULTS_STOCK_1=="NO SAMPLE")

# CWT head lable available but no CWT result
qc3_CWTnoID <- crestBio_grouped %>% 
  filter(!is.na(CWT_HEAD_LABEL) & is.na(CWT_RESULT))

# Non-standard CWT head label
qc4_CWTzero <- crestBio_grouped %>% 
  filter(CWT_HEAD_LABEL==0)

# CWT-based stock ID disagrees with certain (>=80%) DNA stock ID
qc6_CWTDNAdisagree <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="CWT" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME) 

# Otolith-based stock ID disagrees with certain (>=80%) DNA stock ID
qc7_otoDNAdisagree <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="Otolith Stock" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME)

# DNA stock assignments that are below the recommended 80% threshold
qc8_DNAuncert <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1<0.8)


# Possible candidates for PBT assignment
qc9_PBTmaybe <- crestBio_grouped %>% 
  filter(is.na(HATCHERY_ORIGIN) & PROB_1==1 & is.na(DNA_STOCK_2))

# Suspicious Southern US assignment - i.e., a fish assumed to be Southern US with little to no evidence (given mass marking is now a thing for WCVI and not limited to S. US)
qc10_susSUS <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_ORIGIN=="SUS (assumed)" & 
           !is.na(CWT_RESULT) & CWT_RESULT!="No Tag" & 
           !is.na(THERMALMARK) & !THERMALMARK%in%c("No Sample","Not Marked") &
           !is.na(DNA_RESULTS_STOCK_1) & DNA_RESULTS_STOCK_1!="NO SAMPLE")

# CWT BY age disagrees with scale age 
qc11_ageDisagree <- crestBio_grouped %>% 
  filter((YEAR-CWT_BROOD_YEAR) != RESOLVED_AGE)

# A non-standard sex assignment
qc12_nonstdSex <- crestBio_grouped %>% 
  filter(SEX %notin% c("M","F"))




# *********** HERE NEXT DAY: 
## build in R code QC flags (e.g., stock ID available but not populated) -- if this is possible? 
# create QC summary report tab that shows error rate for each QC flag  (e.g., # bad records out of total n rows )
# create readme 
# (two versions of these below )







# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc0 - EBwR unCert Oto",
                                         "qc1_noID",
                                         "qc2_noResults",
                                         "qc3_noCWTID",
                                         "qc4_noRslvdID",
                                         "qc5_unRslvdID",
                                         "antijoin - PADS",
                                         "antijoin - Otos"),
                         number_records = c(nrow(qc0_EBwR_uncertOtoID),
                                            nrow(qc1_noOtoID),
                                            nrow(qc2_noOtoResults),
                                            nrow(qc3_noCWTID),
                                            nrow(qc4_noRslvdID),
                                            nrow(qc5_unRslvdID),
                                            nrow(antijoin_PADS),
                                            nrow(antijoin_OM)),
                         description = c("Esc biodata entries where there was no CWT and duplicate otolith hatch codes were applied within one Brood Year resulting in >1 stock ID options, OR where unable to resolve to Stock level and are left making assumptions based on Facility. These records are still retained in the full biodata file as well, and assumptions are made based on likelihood or facility. These are indicated in the (R) OTOLITH ID METHOD column.",
                                         "Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
                                         "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
                                         "There is a CWT available but no Stock ID.",
                                         "There is a CWT or an NPAFC ID but no Resolved stock ID.",
                                         "Otolith stock ID does not match CWT stock ID.",
                                         "All WCVI CN PADS results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'WCVI Creel Survey' assumed already in CREST. Purpose here is to make sure there are no missing scales expected (i.e., samples not entered in base esc biodata file).",
                                         "All WCVI otolith results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'Sport' assumed already in CREST. Purpose here is to make sure there are no missing otoliths expected (i.e., samples not entered in base esc biodata file).")) %>% 
  print()


# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source escapement file:",
                             "source PADS file:",
                             "source Oto Manager file:",
                             "source NPAFC file:",
                             "!PLACEHOLDER! CWT source:",
                             "",
                             "sheet name:",
                             "Esc biodata w RESULTS",
                             "QC summary",
                             "qc0 - EBwR unCert Oto",
                             "!NPAFC_dupl!",
                             "qc1 - No stock ID",
                             "qc2 - No Oto result",
                             "qc3 - No CWT ID",
                             "qc4 - No Reslvd ID",
                             "qc5 - Unreslvd ID",
                             "antijoin - PADS unmatched",
                             "antijoin - OM unmatched"
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/1-esc_biodata_with_results.R", 
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/2015-2023_WCVI_Escapement-FSC_BioData.xlsx",
        "via direct R query to http://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/AgeBatchList",
        "For 2022, query from OtoManager online stored in: https://086gc.sharepoint.com/:x:/r/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/WCVI%20STAD/Terminal%20CN%20Run%20Recon/2022/Communal%20data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx?d=w398c15dd3c9b4ceb84d3083a215e9c6a&csf=1&web=1&e=NAxyjd",
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx",
        "!NOT IN YET!: http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice",  
        "",
        "sheet description:",
        "WCVI Chinook escapement biodata joined to PADS scale age results, OtoManager thermal mark results, NPAFC mark file to give otolith stock ID, and CWT recoveries. Currently does NOT include DNA results.",
        "Summary of QC flags and # of entries belonging to that flag.",
        "QC flag 0 tab. Only the Esc biodata w RESULTS ('EBwR') entries that correspond to NPAFC BY-hatchcode duplicates. See QC summary for details.",
        "All duplicate BY-hatchcodes documented by the NPAFC. To inform decisions around QC Flag 0.",
        "QC flag 1 tab. See QC summary for details.",
        "QC flag 2 tab. See QC summary for details.",
        "QC flag 3 tab. See QC summary for details.",
        "QC flag 4 tab. See QC summary for details.",
        "QC flag 5 tab. See QC summary for details.",
        "PADS Antijoin tab. See QC summary for details.",
        "OtoManager Antijoin tab. See QC summary for details."))















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
                                         "qc12_nonstdSex",
                                         "",
                                         "total CREST records:"),
                         number_records = c(nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc1_otoNoSample==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc2_scaleNoAge==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc3_CWTnoID==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc4_CWTzero==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc5_WmanNoSample==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc6_CWTDNAdisagree==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc7_otoDNAdisagree==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc8_DNAuncert==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc9_PBTmaybe==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc10_susSUS==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc11_ageDisagree==1,]),
                                            nrow(wcviCNcrest_coded_qc[wcviCNcrest_coded_qc$qc12_nonstdSex==1,]),
                                            "",
                                            nrow(wcviCNcrest_coded)),
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
                                         "Sex designation does not fall as M or F - propose changing all other designations to 'unk'",
                                         "",
                                         paste0("for ", paste(unique(wcviCNcrest$YEAR), collapse = " ") ))) %>% 
  print()




# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source CREST files:",
                             "assumptions made:", 
                             "",
                             "",
                             "sheet name:",
                             "WCVI CN CREST Biodata CODED",
                             "QC summary"),
                     `2` = c(as.character(Sys.Date()), 
                             "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/CREST_termRR_groups.R", 
                             "download from CREST stored on https://086gc.sharepoint.com/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=QSeYb8&cid=a94075b0%2D307f%2D43b2%2D96e6%2D02a093c68a9a&FolderCTID=0x01200009EB148EBFDA544E816AF000384149AC&id=%2Fsites%2FPAC%2DSCAStockAssessmentSTAD%2FShared%20Documents%2FWCVI%20STAD%2FTerminal%20CN%20Run%20Recon%2F2022%2FCommunal%20data%2FCREST&viewid=931f98e0%2Da6b1%2D48c6%2D9fee%2D65ba9363ce0e",
                             "Removed Salmon River from Fraser watershed in stream aux file because duplicate river on VI.",
                             "Changed the terminology and made some manual adjustments to Robertson, Toquart/Toquaht, Big Q, Tranquil and Omega Pacific in stream aux file.",
                             "",
                             "sheet description:",
                             "CREST Biodata, chinook only 2021-2022 for terminal run reconstruction. Joined two years of files together and assigned TermRR roll-up groupings. Produced a number of QC flag columns at the end.",
                             "Summary of QC flag columns and # of entries belonging to that flag."
                             #rep("", 11)
                     ))




#############################################################################################################################################################

#                                                                           VIII. EXPORT 


# Export ---------------------------
# Create a blank workbook
R_OUT_CREST.CODED <- openxlsx::createWorkbook()

# Add sheets to the workbook
openxlsx::addWorksheet(R_OUT_CREST.CODED, "readme")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "WCVI CN CREST Biodata CODED")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC summary")


# Write data to the sheets
openxlsx::writeData(R_OUT_CREST.CODED, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="WCVI CN CREST Biodata CODED", x=wcviCNcrest_coded_qc)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC summary", x=qc_summary)




# Export to git and SP ---------------------------
# To git:
#openxlsx::saveWorkbook(R_OUT_CREST.CODED, 
#                       file=paste0(here("outputs"), 
#                                   sep="/", 
#                                   "R_OUT - WCVI CN CREST Biodata CODED.xlsx"),
#                       overwrite=T,
#                       returnValue=T)


# To SP: 
openxlsx::saveWorkbook(R_OUT_CREST.CODED, 
                       file=paste0("C:/Users", sep="/", 
                                   Sys.info()[6], 
                                   sep="/",
                                   "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/CREST/R_OUT - WCVI CN CREST Biodata CODED.xlsx"),
                       overwrite=T,
                       returnValue=T)





# /END!










