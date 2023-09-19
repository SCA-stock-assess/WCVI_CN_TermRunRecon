
# WCVI Chinook escapement biodata WITH results for Terminal run recon

# Work flow is:
# 1.     Load escapement biodata file off of DFO network  (Step I)
# 2.1.   Load age data via function in source R script (direct query to MPRIS PADS database online)  (Step II)
#   2.2. Export to git/SP for records (used for other run reconstruction purposes)
# 3.1.   Join Escapement biodata to PADS results (Step III)
#   3.2. Calculate brood year
#   3.3. Determine anti-joins (samples that did not connect to escapement biodata records; "bad records/orphans")
# 4.     Load Otolith data from Sharepoint (queried previously to OtoManager)  (Step IV)
# 5.1.   Join Esc+PADS file to OtoManager results  (Step V)
#   5.2. Determine anti-joins (samples that did not connect to escapement biodata records; "bad records/orphans")
# 6.     Load NPAFC mark master file  (Step VI)
# 7.1.   Join ESc+PADS+OtoMgr to NPAFC mark master file  (Step VII)
#   7.2. Create final resolved stock ID 
# 8.     Run QC report(s)  (Step VIII)
# 9.     Export to git and Sharepoint for subsequent use in run reconstructions (Step X)


# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)


# Helpers ----------------
"%notin%" <- Negate("%in%")
analysis_year <- 2022



#############################################################################################################################################################

#                                                                           I. ESCAPEMENT BIODATA LOAD 


# 1. Examine escapement biodata files available ----------------
list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/"), 
           recursive=F, pattern="^[^~]*.xlsx") 

# 2. Select the most recent one. This is manual because the naming convention sucks ----------------
esc_biodata_recent_filename <- list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement"),
                                          recursive=F, pattern="^[^~]*.xlsx")[4]

#3. Read in the file and reformat (slow) ----------------
wcviCNesc2022 <- cbind(
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="A1:A10000", guess_max=10000),
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="F1:AX10000", guess_max=10000),
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CC1:CE10000", guess_max=10000),
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CI1:CM10000", guess_max=10000),
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CP1:CP10000", guess_max=10000),
  readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/", esc_biodata_recent_filename),
             sheet="Biodata 2015-2022", range="CY1:CZ10000", guess_max=10000)) %>%
  filter(Year %in% analysis_year, Species=="Chinook") %>%
  mutate(`(R) OTOLITH BOX NUM`=`Otolith Box #`,
         `(R) OTOLITH VIAL NUM` = `Otolith Specimen #`,
         `(R) OTOLITH LAB NUM` = `Otolith Lab Number`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`Otolith Lab Number`) & !is.na(`Otolith Box #`) & !is.na(`Otolith Specimen #`) ~ 
                                                paste0(`Otolith Lab Number`,sep="-",`Otolith Box #`,sep="-",`Otolith Specimen #`)),
         `(R) SCALE BOOK NUM` = case_when(`Scale Book #` %in% c(15983:15987,15989) ~ paste0("22SC ", `Scale Book #` ),
                                          `Scale Book #` %in% c(17376,17377) ~ paste0("22SP", `Scale Book #` ),
                                          nchar(`Scale Book #`)==4 ~ paste0("0",`Scale Book #`),
                                          TRUE ~ `Scale Book #`),
         `(R) SCALE CELL NUM` = case_when(grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="11" ~ "2",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="21" ~ "3",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="31" ~ "4",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="41" ~ "5",
                                          TRUE ~ `Scale #`),
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & `Scale #`%in%c(1:51) ~ paste0(`(R) SCALE BOOK NUM`,sep="-",`(R) SCALE CELL NUM`)),
         # `(R) FIELD SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) FIELD SCALE BOOK NUM`) ~ paste0(`(R) FIELD SCALE BOOK NUM`,"-",`(R) SCALE CELL NUM`),
         #                                                TRUE ~ NA),
         # `(R) RESOVLED SCALE BOOK-CELL CONCAT` = case_when(is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) SCALE BOOK-CELL CONCAT`,
         #                                                   !is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) FIELD SCALE BOOK-CELL CONCAT`,
         #                                                   TRUE~"FLAG"),
         `(R) SAMPLE YEAR` = Year,
         `(R) HEAD LABEL` = `CWT Head Label #`,
         RrowID = seq(1:nrow(.))) %>%
  drop_na(Year) %>%    # remove entries without year specified
  mutate_at(c("(R) SCALE BOOK NUM", "(R) HEAD LABEL"), as.character) %>%
  print()



#############################################################################################################################################################

#                                                                           II. AGE DATA LOAD 


# Load source script function (saves as 'allPADS') ----------------
source(here("scripts","functions","getAgeDataMRP.R"))


# Clean PADS data ----------------
wcviCNPADS2022 <- allPADS %>% 
  filter(RecoveryYear %in% analysis_year, Area%in%c(20:27, 121:127), Species=="Chinook") %>% 
  filter(!grepl("Georgia Str|Sooke", ProjectName)) %>%
  setNames(paste0('PADS_', names(.))) %>% 
  mutate(`(R) SCALE BOOK NUM` = case_when(is.na(PADS_FieldContainerId) ~ PADS_ContainerId,
                                          !is.na(PADS_FieldContainerId) ~ PADS_FieldContainerId,
                                          TRUE ~ "FLAG"),
         `(R) SCALE CELL NUM` = PADS_FishNumber,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) SCALE BOOK NUM`) & !is.na(`(R) SCALE CELL NUM`) ~ 
                                                    paste0(`(R) SCALE BOOK NUM`,sep="-",`(R) SCALE CELL NUM`))) %>%
  mutate_at("(R) SCALE CELL NUM", as.character) %>%
  print()


# Export to git and SP as a backup for future use ---------------------------
# To git:
writexl::write_xlsx(wcviCNPADS2022, here("outputs", "R_OUT - PADS WCVI Chinook 2022.xlsx"))


# To SP: 
writexl::write_xlsx(wcviCNPADS2022, path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                                "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/BiodataResults/R_OUT - PADS WCVI Chinook 2022.xlsx"))



#############################################################################################################################################################

#                                                                           III. JOIN ESCAPEMENT BIODATA to PADS, CALC BY


# ======================== JOIN ESCAPEMENT BIODATA to PADS, calc BY ========================  
intersect(colnames(wcviCNesc2022), colnames(wcviCNPADS2022))

esc_biodata_PADS <- left_join(wcviCNesc2022,
                              wcviCNPADS2022 %>% 
                                filter(PADS_ProjectName != "WCVI Creel Survey" & !grepl("Fisher", PADS_ProjectName))) %>%
  mutate(`(R) RESOLVED TOTAL AGE` = case_when(!is.na(PADS_GrAge) & !grepl("M|F", PADS_GrAge) ~ as.numeric(paste0(substr(PADS_GrAge,1,1))),
                                              `PADS_GrAge`=="2M" ~ 2,
                                              `PADS_GrAge`=="3M" ~ 3,
                                              `PADS_GrAge`=="4M" ~ 4,
                                              `PADS_GrAge`=="5M" ~ 5,
                                              `PADS_GrAge`=="6M" ~ 6),
         `(R) BROOD YEAR` = analysis_year-`(R) RESOLVED TOTAL AGE`) %>% 
  print()


# ANTI JOINS: Scale samples that didn't make it in to the escapement biodata basefile ---------------------------
esc_scaleIDs <- esc_biodata_PADS %>%
  filter(!is.na(`(R) SCALE BOOK-CELL CONCAT`)) %>% 
  pull(`(R) SCALE BOOK-CELL CONCAT`)

antijoin_PADS <- wcviCNPADS2022 %>% 
  filter(`(R) SCALE BOOK-CELL CONCAT` %notin% esc_scaleIDs & PADS_ProjectName!="WCVI Creel Survey") %>% 
  print()



#############################################################################################################################################################

#                                                                           IV. OTOLITH DATA LOAD 

wcviCNOtos2022 <- readxl::read_excel(path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
                                                 "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx"),
                                     sheet="RcvySpecAge", skip=1, guess_max=20000) %>%
  setNames(paste0('OM_', names(.))) %>% 
  mutate(`(R) OTOLITH BOX NUM` = `OM_BOX CODE`,
         `(R) OTOLITH VIAL NUM` = `OM_CELL NUMBER`,
         `(R) OTOLITH LAB NUM` = `OM_LAB NUMBER`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`OM_LAB NUMBER`) & !is.na(`OM_BOX CODE`) & !is.na(`OM_CELL NUMBER`) ~ 
                                                paste0(`OM_LAB NUMBER`,sep="-",`OM_BOX CODE`,sep="-",`OM_CELL NUMBER`))) %>%
  mutate_at("(R) OTOLITH VIAL NUM", as.character) %>%
  rename(`(R) HATCHCODE` = `OM_HATCH CODE`) %>%
  print()



#############################################################################################################################################################

#                                                                           V. JOIN ESC+PADS to OTOMGR 


# ======================== JOIN ESCAPEMENT BIODATA+PADS to OTOMGR ========================  
intersect(colnames(esc_biodata_PADS), colnames(wcviCNOtos2022))


esc_biodata_PADS_oto <- left_join(esc_biodata_PADS,
                                  wcviCNOtos2022)


# ANTI JOINS: Oto samples that didn't make it in to the escapement biodata basefile ---------------------------
esc_otoIDs <- esc_biodata_PADS_oto %>%
  filter(!is.na(`(R) OTOLITH LBV CONCAT`)) %>% 
  pull(`(R) OTOLITH LBV CONCAT`)

antijoin_OM <- wcviCNOtos2022 %>% 
  filter(`(R) OTOLITH LBV CONCAT` %notin% esc_otoIDs & OM_SOURCE != "Sport") %>% 
  print()



#############################################################################################################################################################

#                                                                           VI. LOAD NPAFC

NPAFC <- readxl::read_excel(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All CN Marks",   #ignore temp files, eg "~All CN Marks...,
                                            full.names = TRUE), 
                            sheet="AllNPAFC CNReleasestoJun8,2022") %>% 
  setNames(paste0('NPAFC_', names(.))) %>% 
  rename(`(R) HATCHCODE` = NPAFC_HATCH_CODE,
         `(R) BROOD YEAR` = NPAFC_BROOD_YEAR) %>% 
  mutate(NPAFC_FACILITY = case_when(is.na(NPAFC_FACILITY) ~ NPAFC_AGENCY,
                                    TRUE ~ NPAFC_FACILITY),
         NPAFC_STOCK = case_when(is.na(NPAFC_STOCK) ~ NPAFC_FACILITY,
                                 TRUE ~ NPAFC_STOCK)) %>%
  # **ASSUPMTIONS**: 
  # 1. Alaska and Kamchatka marks should essentially not exist, so remove them to avoid duplicate marks 
  filter(NPAFC_STATE_PROVINCE %in% c("BRITISH COLUMBIA", "IDAHO", "WASHINGTON", "OREGON"),
         # 2. Remove the one case where RCH and Nanaimo Hatchery used the same mark in 2018 and assume it was a RCH fish
         !grepl("NANAIMO", NPAFC_FACILITY) | `(R) BROOD YEAR`!=2018 | `(R) HATCHCODE`!="H5") %>%
  select(`(R) BROOD YEAR`, NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, `(R) HATCHCODE`) %>% 
  print()



#############################################################################################################################################################

#                                                                           VII. JOIN BIODATA+PADS+OTO to NPAFC

# ======================== JOIN ESCAPEMENT BIODATA+PADS+OTOMGR to NPAFC ========================  
intersect(colnames(esc_biodata_PADS_oto), colnames(NPAFC))

esc_biodata_w_RESULTS <- left_join(esc_biodata_PADS_oto,
                                   NPAFC) %>% 
  mutate(`(R) HATCHERY ORIGIN` = case_when(`OM_READ STATUS` == "Not Marked" ~ "Natural",
                                           `AD Clipped?` == "Y" ~ "Hatchery"),
         # update resolved stock ID when more stock ID available 
         `(R) RESOLVED STOCK` = NPAFC_STOCK) %>% 
  print()



#############################################################################################################################################################

#                                                                           VIII. QC and README


# QC flags ---------------------------
qc1_noOtoID <- esc_biodata_w_RESULTS %>%
  filter(!is.na(`(R) BROOD YEAR`) & !is.na(`(R) HATCHCODE`) & `(R) HATCHCODE` %notin% c("Destroyed", "Not Marked", "No Sample") & is.na(NPAFC_STOCK)) %>%
  print()

qc2_noOtoResults <- esc_biodata_w_RESULTS %>%
  filter(!is.na(`(R) OTOLITH LBV CONCAT`) & !is.na(`(R) BROOD YEAR`) & is.na(`(R) HATCHCODE`) & `OM_READ STATUS`!="Not Marked") %>%
  print()

# PLACEHOLDER: NO CWT DATA YET
 qc3_noCWTID <- data.frame(val="Empty")   #esc_biodata_w_RESULTS %>%
   #filter(!is.na(`(R) TAGCODE`) & `(R) TAGCODE`!="No Tag" & is.na(`MRP_Stock Site Name`)) %>% 
   filter()

qc4_noRslvdID <- esc_biodata_w_RESULTS %>% 
  filter(is.na(`(R) RESOLVED STOCK`) & !is.na(NPAFC_STOCK)) %>% 
  print()


# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc1_noID",
                                         "qc2_noResults",
                                         "qc3_noCWTID",
                                         "qc4_noRslvdID"),
                         number_records = c(nrow(qc1_noOtoID),
                                            nrow(qc2_noOtoResults),
                                            nrow(qc3_noCWTID),
                                            nrow(qc4_noRslvdID)),
                         description = c("Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
                                         "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
                                         "There is a CWT available but no Stock ID.",
                                         "There is a CWT or an NPAFC ID but no Resolved stock ID.")) %>% 
  print()


# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source escapement file:",
                             "source PADS file:",
                             "source Oto Manager file:",
                             "source NPAFC file:",
                             "!PLACEHOLDER! CWT source:",
                             "assumptions made:", 
                             "",
                             "",
                             "sheet name:",
                             "Esc biodata w RESULTS",
                             "QC summary",
                             "qc1 - No stock ID",
                             "qc2 - No Oto result",
                             "qc3 - No CWT ID",
                             "qc4 - No Reslvd ID",
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
        "Removed AK and Kamchatka marks from NPAFC file to avoid duplicates, assuming strays are only BC/SUS",
        "Ignored one case where RCH and Nanaimo hatchery applied the H5 same mark in 2018; assume it was a RCH mark that showed up in 2022 Burman broodstock",
        "",
        "sheet description:",
        "2022 WCVI Chinook escapement biodata joined to PADS scale age results, OtoManager thermal mark results, and the NPAFC mark file to give otolith stock ID. Currently does NOT include CWT Head Tag or DNA results.",
        "Summary of QC flags and # of entries belonging to that flag.",
        "QC flag 1 tab. See QC summary for details.",
        "QC flag 2 tab. See QC summary for details.",
        "QC flag 3 tab. See QC summary for details.",
        "QC flag 4 tab. See QC summary for details.",
        "All WCVI CN 2022 PADS results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., FOS. ASSUMPTION: Removed 'WCVI Creel Survey' assumed already in CREST.",
        "All WCVI CN 2022 otolith results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., FOS. ASSUMPTION: Removed 'Sport' assumed already in CREST."))



#############################################################################################################################################################

#                                                                           X. EXPORT 

# Export ---------------------------
# Create a blank workbook
R_OUT_ESC.RES <- openxlsx::createWorkbook()

# Add sheets to the workbook
openxlsx::addWorksheet(R_OUT_ESC.RES, "readme")
openxlsx::addWorksheet(R_OUT_ESC.RES, "Esc biodata w RESULTS")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC summary")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc1 - No Oto stock ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc2 - No Oto result")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc3 - No CWT ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc4 - No Reslvd ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - PADS unmatched")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - OM unmatched")

# Write data to the sheets
openxlsx::writeData(R_OUT_ESC.RES, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_ESC.RES, sheet="Esc biodata w RESULTS", x=esc_biodata_w_RESULTS)
openxlsx::writeData(R_OUT_ESC.RES, sheet="QC summary", x=qc_summary)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "qc1 - No Oto stock ID", x=qc1_noOtoID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "qc2 - No Oto result", x=qc2_noOtoResults)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "qc3 - No CWT ID", x=qc3_noCWTID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "qc4 - No Reslvd ID", x=qc4_noRslvdID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - PADS unmatched", x=antijoin_PADS)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - OM unmatched", x=antijoin_OM)

# Export to git and SP ---------------------------
# To git:
openxlsx::saveWorkbook(R_OUT_ESC.RES, 
                       file=paste0(here("outputs"), 
                                   sep="/", 
                                   "R_OUT - Escapement biodata WITH RESULTS.xlsx"),
                       overwrite=T,
                       returnValue=T)


# To SP: 
openxlsx::saveWorkbook(R_OUT_ESC.RES, 
                       file=paste0("C:/Users", sep="/", 
                                   Sys.info()[6], 
                                   sep="/",
                                   "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/BiodataResults/R_OUT - Escapement biodata WITH RESULTS.xlsx"),
                       overwrite=T,
                       returnValue=T)










# /END!











# NON JOIN SCALES
# NON JOIN OTOS