
# Join EPRO to NPAFC mark master file to assign stock IDs to otolith hatch codes (missing step in EPRO as of Sept 2023)

# Work flow is:
# 1.1. Download all facility files 'All Adult Biosampling' reports from EPRO: https://epro-stage.azure.cloud.dfo-mpo.gc.ca/EProWeb/#home
#       1.2. Store EPRO files on Term Run working SharePoint site: https://086gc.sharepoint.com/:f:/r/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/WCVI%20STAD/Terminal%20CN%20Run%20Recon/2022/Communal%20data/EPRO?csf=1&web=1&e=LggAxf
# 2.1. Load EPRO files into R from SharePoint (Step I) 
#       2.2. Export to git and SharePoint for records
# 3.   Load NPAFC mark master file from SCD_Stad network drive (Step II): dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks....xlsx
# 4.   Join EPRO to NPAFC mark master file (Step III)
# 5.   Load CWT release tag codes from last 10 years (Step IV)
# 6.   Join EPRO+NPAFC file to CWT tag codes (Step V)
# 7.   Assign final stock ID (Step VI)
# 5.   Run QC report(s) (Step VII)
# 6.   Export to git and Sharepoint for subsequent use in run reconstructions (Step VIII)


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

#                                                                           I. EPRO FILES LOAD 


# Load files from Sharepoint -------------

# Save directory name for where files are located (change if necessary)
epro_dir <- paste("C:/Users", Sys.info()[6], "OneDrive - DFO-MPO/WCVI STAD/Terminal CN Run Recon/2022/Communal data/EPRO", sep="/")

# Load files as a list of tibbles
epro.files <- list.files(epro_dir, pattern = "All_Adult_Biosampling_", full.names = T) |> 
  purrr::set_names(
    list.files(epro_dir,pattern = "All_Adult_Biosampling_",full.names = F)) |> 
  map(~readxl::read_excel(path = .x, trim_ws=T), id = "path" )
# Should be a Large List of at least 7 elements: Burman, Conuma, Gold, Nahmint, Nitinat, Robertson, Sarita




# Convert the Large List into a useable R dataframe ---------------------------
wcviCNepro2022 <- epro.files %>%
  map(~ mutate(.x, across(everything(), as.character) # Convert all columns to character for rbind compatibility
    )
  ) %>% 
  list_rbind(names_to = "file_source") %>% 
  filter(`Spawning Stock` != "") %>%
  mutate(
    across(everything(), parse_guess), # Automatically determine column classes based on values
    `(R) OTOLITH BOX NUM` = `Bag No`,
    `(R) OTOLITH VIAL NUM` = `Vial No`,
    `(R) OTOLITH BOX-VIAL CONCAT` = case_when(
      !is.na(`Bag No`) & !is.na(`Vial No`) ~ paste0(`Bag No`,sep="-",`Vial No`)
    ),
    `(R) SCALE BOOK NUM` = `Book No`,
    `(R) SCALE CELL NUM` = `Scale Sample No`,
    `(R) SCALE BOOK-CELL CONCAT` = case_when(
      !is.na(`Book No`) & !is.na(`Scale Sample No`) ~ paste0(`Book No`,sep="-",`Scale Sample No`)
    ),
    `(R) TAGCODE` = `CWT Tag Code`,
    `(R) HATCHCODE` = `Hatch Code`,
    `(R) RESOLVED TOTAL AGE` = case_when(!is.na(`CWT Age (yrs)`) ~ as.numeric(`CWT Age (yrs)`), # Prefer CWT ages where available
                                         !is.na(`Total Age (yrs)`) ~ as.numeric(`Total Age (yrs)`), # Some entries for ttl age are "TRUE"(??)
                                         `Scale Part Age`=="1M" ~ 2,
                                         `Scale Part Age`=="2M" ~ 3,
                                         `Scale Part Age`=="3M" ~ 4,
                                         `Scale Part Age`=="4M" ~ 5,
                                         `Scale Part Age`=="5M" ~ 6,
                                         `Scale Part Age`=="6M" ~ 7,
                                         T ~ NA_real_),
    `(R) BROOD YEAR` = analysis_year - `(R) RESOLVED TOTAL AGE`,
    UEID = paste0("2022", "-", seq(1:nrow(.)))) 

# Remove the list object
rm(epro.files)


# Export to git and SP as a backup for future use---------------------------
# To git:
# writexl::write_xlsx(wcviCNepro2022, path=paste0(here("outputs"), sep="/", "R_OUT - All EPRO facilities master.xlsx"))


# Export to SharePoint: 
writexl::write_xlsx(
  wcviCNepro2022, 
  path = paste0(
    epro_dir, 
    "/R_OUT - All EPRO facilities master.xlsx"
    )
  )



################################################################################################################################################

#                                                                           II. NPAFC LOAD


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
                                 TRUE ~ NPAFC_STOCK),
         `(R) BYHID` = case_when(!is.na(`(R) BROOD YEAR`) & !is.na(`(R) HATCHCODE`) ~ paste0(`(R) BROOD YEAR`, " - ", `(R) HATCHCODE`),
                                 TRUE ~ NA)) %>%
  # This filter line below initially removed some marks to avoid duplicates, but the work-around below now addresses this more systematically. Below is just
  #     kept for reference for now
  #filter(NPAFC_STATE_PROVINCE %in% c("BRITISH COLUMBIA", "IDAHO", "WASHINGTON", "OREGON"),
  # 2. Remove the one case where RCH and Nanaimo Hatchery used the same mark in 2018 and assume it was a RCH fish
  #!grepl("NANAIMO", NPAFC_FACILITY) | `(R) BROOD YEAR`!=2018 | `(R) HATCHCODE`!="H5"
  #) %>%
  select(`(R) BROOD YEAR`, NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_REGION, `(R) BYHID`, NPAFC_NUMBER_RELEASED) %>% 
  distinct(`(R) BROOD YEAR`, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_FACILITY, NPAFC_STOCK, .keep_all=T) %>% 
  #group_by(`(R) BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`) %>% 
  mutate(NPAFC_wcvi_prob = case_when(NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("NWVI","SWVI") ~ "A",
                                     NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("LWFR","TOMM", "TOMF") ~ "B",
                                     NPAFC_STATE_PROVINCE%in%c("IDAHO","OREGON","WASHINGTON") ~ "B",
                                     NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("GSVI","JNST") ~ "C",
                                     NPAFC_STATE_PROVINCE=="ALASKA" ~ "D",
                                     NPAFC_STATE_PROVINCE=="KAMCHATKA" ~ "E")) %>%
  #mutate(NPAFC_wcvi_prob = reorder(NPAFC_wcvi_prob, prob_orders))  %>% 
  arrange(`(R) BYHID`, NPAFC_wcvi_prob) %>%
  mutate(group = case_when(!is.na(`(R) BYHID`) ~ with(., ave(seq_along(`(R) BYHID`), `(R) BYHID`, FUN = seq_along)),
                           TRUE ~ 1),
         NPAFC_wcvi_prob = case_when(NPAFC_wcvi_prob=="A" ~ "HIGH",
                                     NPAFC_wcvi_prob=="B" ~ "MED-HIGH",
                                     NPAFC_wcvi_prob=="C" ~ "MED",
                                     NPAFC_wcvi_prob=="D" ~ "LOW",
                                     NPAFC_wcvi_prob=="E" ~ "V LOW")) %>%
  group_by(`(R) BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`) %>% 
  pivot_wider(names_from = group,
              values_from= c(NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, NPAFC_STATE_PROVINCE, NPAFC_REGION, NPAFC_wcvi_prob, NPAFC_NUMBER_RELEASED)) %>%
  select(`(R) BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  print()



#############################################################################################################################################################

#                                                                           III. JOIN EPRO + NPAFC


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(wcviCNepro2022), colnames(NPAFC))

wcviCNepro_w_NPAFC2022 <- left_join(wcviCNepro2022 ,
                                    NPAFC,
                                    by=c("(R) BROOD YEAR", "(R) HATCHCODE"),
                                    relationship="many-to-one")



#############################################################################################################################################################

#                                                                           IV. CWT LOAD


# Load function to query MRPIS CWT releases --------------------------- 
source(here("scripts","functions","getCWTData.R"))


# Query MRP Releases using json query doc --------------------------- 
CWT_rel_10yr <- getCWTData(paste0(here("scripts","json"), "/CWT_Releases_CN_last10yrs.json"), password=NULL) %>% 
  select(`Tagcode`,
         `Release Agency Code`,
         `Country Code`,
         `Stock Site Prov/State Code`,
         `Brood Year`,
         `Release Year`,
         `Stock Site Name`,
         `Stock PSC Basin Code`,
         `Stock PSC Region Code`,
         `Hatchery Site Name`,
         `Release Site Name`,
         `Release PSC Basin Code`,
         `Release PSC Region Code`) %>% 
  setNames(paste0('MRP_', names(.))) %>% 
  rename(`(R) TAGCODE` = MRP_Tagcode) %>% 
  print()

# If you get an error message about "atomic vectors" try restarting R and/or your computer. It's related to connectivity access and just means R isn't 
# talking to the DFO network properly



#############################################################################################################################################################

#                                                                           V. JOIN EPRO+NPAFC + CWT


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(wcviCNepro_w_NPAFC2022), colnames(CWT_rel_10yr))

wcviCNepro_w_NPAFC.MRP2022 <- left_join(wcviCNepro_w_NPAFC2022 ,
                                        CWT_rel_10yr,
                                        by="(R) TAGCODE",
                                        relationship="many-to-one")


#############################################################################################################################################################

#                                                                           VI. ASSIGN FINAL STOCK ID and ORIGIN


# Temp read due to database blocks
wcviCNepro_w_NPAFC.MRP2022_TEMP <- readxl::read_excel(paste0("C:/Users", sep="/", Sys.info()['login'], sep="/",
                                                             "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/R_OUT - All EPRO facilities master WITH RESULTS.xlsx"),
                                                      sheet="AllFacilities w RESULTS")



wcviCNepro_w_Results2022 <- wcviCNepro_w_NPAFC.MRP2022_TEMP %>%
  mutate(#`(R) ORIGIN` = case_when(`(R) HATCHCODE`=="Not Marked" & (`(R) TAGCODE`=="No tag" | is.na(`(R) TAGCODE`)) & `External Marks`!="Clipped"  ~ "Natural",
  #                                 
  #                                 `External Marks`=="Clipped" |
  #                                 (`(R) HATCHCODE`%notin%c("Destroyed","No Sample", "Not Marked") & !is.na(`(R) HATCHCODE`)) | 
  #                                   (`(R) TAGCODE`!="No tag" & !is.na(`(R) TAGCODE`)) ~ "Hatchery",
  # 
  #                                 `External Marks`!="Clipped" & (is.na(`(R) HATCHCODE`) | `(R) HATCHCODE`%in%c("Destroyed","No Sample")) &
  #                                   is.na(`(R) TAGCODE`) ~ "Unknown",
  #                                 
  #                                 TRUE ~ "FLAG"),
         
         
         `(R) ORIGIN` = case_when(`External Marks`=="Clipped" ~ "Hatchery",
                                  !is.na(`CWT Tag Code`) | `CWT Tag Code` != "No tag" ~ "Hatchery",
                                  `Hatch Code` == "Marked" ~ "Hatchery",
                                  `Hatch Code` == "Not Marked" ~ "Natural",
                                  TRUE ~ "Unknown"),
         
         
         `(R) CWT STOCK ID` = case_when(!is.na(`MRP_Stock Site Name`) ~ 
                                          gsub(" Cr", "", 
                                               gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
                                               ignore.case=F),
                                        TRUE ~ NA),
         
         
         `(R) OTOLITH ID METHOD` = case_when(!is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ "To stock (certain)",
                                             !is.na(NPAFC_STOCK_1) & !is.na(NPAFC_STOCK_2) ~ 
                                               "Duplicate BY-hatchcode at >1 facility, assumed stock ID (moderately certain ID)",
                                             is.na(NPAFC_STOCK_1) & !is.na(OM_FACILITY) ~ "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)",
                                             TRUE~NA),
         
         
         
         `(R) STOCK ID` = case_when(!is.na(`MRP_Stock Site Name`) ~ gsub(" Cr", "", 
                                                                         gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
                                                                         ignore.case=F),
                                    
                                    (is.na(`MRP_Stock Site Name`) | `MRP_Stock Site Name`=="No Tag") & !is.na(NPAFC_STOCK) ~ 
                                      gsub(" R", "",
                                           gsub(" Cr", "",  
                                                str_to_title(
                                                  str_sub(NPAFC_STOCK,3,100)), 
                                                ignore.case = F), 
                                           ignore.case=F),
                                    
                                    TRUE ~ "Unknown"),

         
         `(R) STOCK ID METHOD` = case_when(!is.na(`MRP_Stock Site Name`) ~ "CWT",
                                           (is.na(`MRP_Stock Site Name`) | `MRP_Stock Site Name`=="No Tag") & !is.na(NPAFC_STOCK) ~ "Otolith",
                                           TRUE ~ NA),
         
         `(R) RESOLVED STOCK-ORIGIN` = paste0(`(R) ORIGIN`, sep=" ", `(R) STOCK ID`)) %>% 
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
                              "AllFacilities w RESULTS",
                              "QC summary",
                              "qc1 - No stock ID",
                              "qc2 - No Oto result",
                             "qc3 - No CWT ID",
                             "qc4 - No Reslvd ID"
                              ),
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
                             "QC flag 1 tab. See QC summary for details.",
                             "QC flag 2 tab. See QC summary for details.",
                             "QC flag 3 tab. See QC summary for details.",
                             "QC flag 4 tab. See QC summary for details."))


# QC flags ---------------------------
qc1_noOtoID <- wcviCNepro_w_Results2022 %>%
  filter(!is.na(`(R) BROOD YEAR`) & !is.na(`(R) HATCHCODE`) & `(R) HATCHCODE` %notin% c("Destroyed", "Not Marked", "No Sample") & is.na(NPAFC_STOCK)) %>%
  print()


qc2_noOtoResults <- wcviCNepro_w_Results2022 %>%
  filter(!is.na(`(R) OTOLITH BOX-VIAL CONCAT`) & !is.na(`(R) BROOD YEAR`) & is.na(`(R) HATCHCODE`)) %>%
  print()


qc3_noCWTID <- wcviCNepro_w_Results2022 %>% 
  filter(!is.na(`(R) TAGCODE`) & `(R) TAGCODE`!="No Tag" & is.na(`MRP_Stock Site Name`)) %>% 
  filter()


qc4_noRslvdID <- wcviCNepro_w_Results2022 %>% 
  filter(`(R) STOCK ID`=="Unknown" & !is.na(NPAFC_STOCK) & !is.na(`MRP_Stock Site Name`)) %>% 
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
openxlsx::saveWorkbook(
  R_OUT_EPRO.NPAFC, 
  file=paste0(
    epro_dir, 
    "/R_OUT - All EPRO facilities master WITH RESULTS.xlsx"
  ),
  overwrite=T,
  returnValue=T
)





# /END!





