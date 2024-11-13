# CRESTcompile
# compile CREST biodata outputs for CN Run Recon  **assumes everything is from the "WCVI CHINOOK ONLY biodata..." report
# Then assign term rollup stock groups
# Feb 2024



## A note about this: as of NOv 2024 it has become apparent the WCVI CREST query is not appropriate anymore. Too many bugs not being maintained. From now on,
##  use BDWR query.  b


# ============================= SET UP ============================= 
# Load high-use packages ---------------------------
library(tidyverse)
#library(saaWeb)     

# Helpers ---------------------------
"%notin%" <- Negate("%in%")
options(scipen=9999)



# ============================= LOAD DATA =============================

# Read CREST files as large list ---------------------------
# Load base files to compile
crestBio.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                 pattern="^\\d{4}_Biological_Data_With_Results_.*\\.xlsx$", 
                                 full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="Biological_Data_With", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBio.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/1-Import-to-R", 
                                 pattern="^\\d{4}_Biological_Data_With_Results_.*\\.xlsx$", 
                                 full.names=F)


# Convert the Large List into a useable R dataframe:
crestBio <- do.call("rbind", crestBio.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()


# Clean up:
#remove(crestBio.LL)


# Load Stream aux foile ---------------------------
# This is for if we want roll up groups like "Other Area 23", "Other Area 25", etc.
# Should load pullNusedsData function and streamAreas dataframe: 
source(here::here("scripts", "misc-helpers", "CRESTcompile-streamAuxFile.R"))      
# saves as streamAreas




#############################################################################################################################################################

#                                                                     CODE TERM RUN GROUPS

# ============================= DEFINE HELPERS ============================= 

# Focal streams in each area to highlight ---------------------------
focal_a22 <- c("CONUMA", "NITINAT", "ROBERTSON", "SAN JUAN")
focal_a23 <- c("CONUMA", "NITIANT", "ROBERTSON")
focal_a25 <- c("BEDWELL", "BURMAN", "CONUMA", "KAOUK", "MARBLE", "NITINAT", "ROBERTSON", "SAN JUAN")

# Used to remove the river/creek suffix later ---------------------------
stopwords <- c(" River", " Creek")




# ============================= CODE TERM RUN GROUPS =============================

crestBio_grouped <- 
  # Join CREST biodata and streamAreas aux file ----
left_join(crestBio, 
          streamAreas) %>% 
  
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
      TRUE ~ "Unknown"),
    
    # 2. Create 'Term RR Roll Ups' column ---
    `(R) Term RR Roll Ups` = case_when(
      #2.0 Base case if is.na(RESOLVED_STOCK_ORIGIN) make it "Unknown"
      is.na(RESOLVED_STOCK_ORIGIN) ~ "Unknown", 
      #2.2 If it is NOT from NWVI or SWVI, it gets "NON-WCVI"
      RESOLVED_STOCK_ROLLUP%notin%c("NWVI", "SWVI") ~ "NON-WCVI",
      #2.4 Special case: Change "Tofino Hatchery" to "Bedwell"
      RESOLVED_STOCK_ORIGIN=="Tofino Hatchery" ~ "BEDWELL",   # hatchery bedwell? 
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
  
  # PROPOSED NEW GROUPINGS: Ignore all the rollups and just print the stock ID for level1, and roll up to watershed for level2 (not "NON-WCVI")
  mutate(`(R) TERM GROUP01` = case_when(is.na(RESOLVED_STOCK_ORIGIN) ~ paste0(`(R) Origin`, " Unknown"),
                                        !is.na(RESOLVED_STOCK_ORIGIN) & RESOLVED_STOCK_SOURCE=="DNA" & PROB_1 < 0.75 ~ paste0(`(R) Origin`, sep=" ", "Unknown (DNA did not resolve)"),
                                        TRUE ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN)),
         `(R) TERM GROUP02` = case_when(RESOLVED_STOCK_ROLLUP %in% c("SWVI", "NWVI") ~ `(R) TERM GROUP01`,
                                        RESOLVED_STOCK_ROLLUP %notin% c("SWVI", "NWVI") & !is.na(RESOLVED_STOCK_ROLLUP) ~ paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP),
                                        RESOLVED_STOCK_ROLLUP %notin% c("SWVI", "NWVI") & is.na(RESOLVED_STOCK_ROLLUP) ~ paste0(`(R) Origin`, " Unknown"),
                                        grepl("ECVI", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "ECVI",
                                        grepl("Bolshaya", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "Russian (uncertain)",
                                        grepl("Alaska", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "Alaska",
                                        grepl("Homathko|Mainland Inlets", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "Mainland Inlets",
                                        grepl("SALMON_RIVER_JNST", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "NEVI",
                                        grepl("SERPENTINE", RESOLVED_STOCK_ROLLUP, ignore.case=T) ~ "S Mainland",
                                        is.na(RESOLVED_STOCK_ROLLUP) & !is.na(RESOLVED_STOCK_ORIGIN) & grepl("Grays", RESOLVED_STOCK_ORIGIN) ~ "Coastal Washington",
                                        is.na(RESOLVED_STOCK_ROLLUP) & !is.na(RESOLVED_STOCK_ORIGIN) & grepl("Oregon", RESOLVED_STOCK_ORIGIN) ~ "Coastal Oregon",
                                        is.na(RESOLVED_STOCK_ROLLUP) & !is.na(RESOLVED_STOCK_ORIGIN) & grepl("Puget|Hood", RESOLVED_STOCK_ORIGIN) ~ "Puget Sound",
                                        is.na(RESOLVED_STOCK_ROLLUP) & !is.na(RESOLVED_STOCK_ORIGIN) & grepl("Western Vancouver Island", RESOLVED_STOCK_ORIGIN) ~ "SWVI",
                                        TRUE ~ "FLAG"),
         `(R) TERM GROUP03` = case_when(RESOLVED_STOCK_ROLLUP %in% c("SWVI", "NWVI") ~ paste0(`(R) Origin`, " WCVI"),
                                        TRUE ~ paste0(`(R) Origin`, " NON-WCVI"))) %>%
  print()



#############################################################################################################################################################


# ==================== 3. QC & README ==================== 


# QC flags ---------------------------

# Otolith sample and age data (for stock ID) available but no result (possible oto processing error)
qc_otoNoSample <- crestBio_grouped %>% 
  filter(!is.na(OTOLITH_BOX) & !is.na(OTOLITH_SPECIMEN) & !is.na(RESOLVED_AGE) & THERMALMARK!="Not Marked")

# Scale sample available but no result (possible scale processing error)
qc_scaleNoAge <- crestBio_grouped %>% 
  filter(is.na(RESOLVED_AGE) & is.na(PART_AGE_CODE) & !is.na(SCALE_BOOK) & !is.na(SCALE_NO))

# DNA sample available but no DNA result (possible DNA processing error)
qc_WmanNoSample <- crestBio_grouped %>% 
  filter(!is.na(SPECIMEN_REFERENCE_DNA_NO) & is.na(DNA_RESULTS_STOCK_1) | DNA_RESULTS_STOCK_1=="NO SAMPLE")

# CWT head label available but no CWT result
qc_CWTnoID <- crestBio_grouped %>% 
  filter(!is.na(CWT_HEAD_LABEL) & is.na(CWT_RESULT))

# Non-standard CWT head label
qc_CWTzero <- crestBio_grouped %>% 
  filter(CWT_HEAD_LABEL==0)

# CWT-based stock ID disagrees with certain (>=80%) DNA stock ID
qc_CWTDNAdisagree <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="CWT" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME) 

# Otolith-based stock ID disagrees with certain (>=80%) DNA stock ID
qc_otoDNAdisagree <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="Otolith Stock" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME)

# DNA stock assignments that are below the recommended 80% threshold
qc_DNAuncert <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1<0.8)


# Possible candidates for PBT assignment
qc_PBTmaybe <- crestBio_grouped %>% 
  filter(is.na(HATCHERY_ORIGIN) & PROB_1==1 & is.na(DNA_STOCK_2))

# Suspicious Southern US assignment - i.e., a fish assumed to be Southern US with little to no evidence (given mass marking is now a thing for WCVI and not limited to S. US)
qc_susSUS <- crestBio_grouped %>% 
  filter(RESOLVED_STOCK_ORIGIN=="SUS (assumed)" & 
           !is.na(CWT_RESULT) & CWT_RESULT!="No Tag" & 
           !is.na(THERMALMARK) & !THERMALMARK%in%c("No Sample","Not Marked") &
           !is.na(DNA_RESULTS_STOCK_1) & DNA_RESULTS_STOCK_1!="NO SAMPLE")

# CWT BY age disagrees with scale age 
qc_ageDisagree <- crestBio_grouped %>% 
  filter((YEAR-CWT_BROOD_YEAR) != RESOLVED_AGE)

# A non-standard sex assignment
qc_nonstdSex <- crestBio_grouped %>% 
  filter(SEX %notin% c("M","F"))




# QC summary report ---------------------------
qc_summary <- data.frame(`QC flag/tab name` = c("qc_otoNoSample",
                                         "qc_scaleNoAge",
                                         "qc_WmanNoSample",
                                         "qc_CWTnoID",
                                         "qc_CWTzero",
                                         "qc_CWTDNAdisagree",
                                         "qc_otoDNAdisagree",
                                         "qc_DNAuncert",
                                         "qc_PBTmaybe",
                                         "qc_susSUS",
                                         "qc_ageDisagree",
                                         "qc_nonstdSex",
                                         "",
                                         "total CREST records:"),
                         `number of records` = c(nrow(qc_otoNoSample),
                                            nrow(qc_scaleNoAge),
                                            nrow(qc_WmanNoSample),
                                            nrow(qc_CWTnoID),
                                            nrow(qc_CWTzero),
                                            nrow(qc_CWTDNAdisagree),
                                            nrow(qc_otoDNAdisagree),
                                            nrow(qc_DNAuncert),
                                            nrow(qc_PBTmaybe),
                                            nrow(qc_susSUS),
                                            nrow(qc_ageDisagree),
                                            nrow(qc_nonstdSex),
                                            "",
                                            nrow(crestBio_grouped)),
                         description = c("Otolith box/vial numbers exist but there is 'No Sample'",
                                         "Scale book and number but no age, and no explanation given (e.g., resorbed etc)",
                                         "A CWT head label was submitted but the stock ID result is blank",
                                         "CWT head label entered as '0' or other non-standard format.",
                                         "Whatman sheet/DNA tracking numbers exist but no GSI result (blank)",
                                         "Stock ID assigned by CWT but GSI (>=80% certainty) disagrees",
                                         "Stock ID assigned by otolith thermal mark but GSI (>=80% certainty) disagrees",
                                         "A stock ID assigned by DNA but with <80% certainty (not recommended)",
                                         "Hatchery origin given as a blank, but some possibility for PBT assignment (PROB=1.00). Note this should be approached with lots of caution and is stock-specific.",
                                         "'SUS (assumed)' that have ID methods available - A CWT head label was submitted but the stock ID result is blank",
                                         "Cases where the scale age does not match the CWT brood year age (calculated as catch year-CWT_BROOD_YEAR)",
                                         "Sex designation is non-standard, i.e., is not M or F - propose changing all other designations to 'unk'?",
                                         "",
                                         paste0("for ", paste(unique(crestBio_grouped$YEAR), collapse = " ") ))) %>% 
  print()




# Create readme tab ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source CREST files:",
                             "R export CREST file location:",
                             "assumptions made:", 
                             "",
                             "",
                             "Tab name:",
                             "WCVI CN CREST Biodata CODED",
                             "QC summary",
                             "QC-..."),
                     `2` = c(as.character(Sys.Date()), 
                             "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/CRESTcompile.R", 
                             "SCD_Stad/WCVI/CHINOOK?WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/1-Import-to-R",
                             "SCD_Stad/WCVI/CHINOOK?WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R",
                             "Removed Salmon River from Fraser watershed in stream aux file because duplicate river on VI.",
                             "Changed the terminology and made some manual adjustments to Robertson, Toquart/Toquaht, Big Q, Tranquil and Omega Pacific in stream aux file.",
                             "",
                             "Tab description:",
                             "CREST Biodata from 'WCVI Chinook biodata results with FOS' report, 2021-recent year for terminal run reconstruction. Joined years of files together and assigned TermRR roll-up groupings. Requires manual tump of area/year files from CREST.",
                             "Summary of QC flag columns and # of entries belonging to that flag.",
                             "QC flag tabs. See QC summary for details."
                     ))



#############################################################################################################################################################

#                                                                           EXPORT 


# ==================== CREATE EXCEL FILE ==================== 

# Create Workbook ---------------------------
R_OUT_CREST.CODED <- openxlsx::createWorkbook()

# Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.CODED, "readme")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "WCVI CN CREST Biodata CODED")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC Report")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Oto sample no result")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Scale sample no result")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Whatman sample no result")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- CWT no result")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- CWT non-standard")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- CWT-DNA stock ID disagree")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Oto-DNA stock ID disagree")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Uncertain DNA used")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- PBT possible")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- S.US suspicious")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Scale-CWT age disagree")
openxlsx::addWorksheet(R_OUT_CREST.CODED, "QC- Non-standard sex ID")


# Add data to tabs ---------------------------
openxlsx::writeData(R_OUT_CREST.CODED, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="WCVI CN CREST Biodata CODED", x=crestBio_grouped)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC Report", x=qc_summary)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Oto sample no result", x=qc_otoNoSample)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Scale sample no result", x=qc_scaleNoAge)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Whatman sample no result", x=qc_WmanNoSample)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- CWT no result", x=qc_CWTnoID)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- CWT non-standard", x=qc_CWTzero)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- CWT-DNA stock ID disagree", x=qc_CWTDNAdisagree)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Oto-DNA stock ID disagree", x=qc_otoDNAdisagree)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Uncertain DNA used", x=qc_DNAuncert)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- PBT possible", x=qc_PBTmaybe)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- S.US suspicious", x=qc_susSUS)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Scale-CWT age disagree", x=qc_ageDisagree)
openxlsx::writeData(R_OUT_CREST.CODED, sheet="QC- Non-standard sex ID", x=qc_nonstdSex)




# ==================== EXPORT EXCEL FILE ==================== 

# To github ---------------------------
openxlsx::saveWorkbook(R_OUT_CREST.CODED,
                      file=paste0(here::here("outputs"),
                                  "/R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS ",
                                  min(crestBio_grouped$YEAR),
                                  "-",
                                  max(crestBio_grouped$YEAR),
                                  ".xlsx"),
                      overwrite=T,
                      returnValue=T)


# To Network: 
openxlsx::saveWorkbook(R_OUT_CREST.CODED, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS ",
                                   min(crestBio_grouped$YEAR),
                                   "-",
                                   max(crestBio_grouped$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To Desktop: 
openxlsx::saveWorkbook(R_OUT_CREST.CODED, 
                       file=paste0("C:/Users/DAVIDSONKA/Desktop", 
                                   "/R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS ",
                                   min(crestBio_grouped$YEAR),
                                   "-",
                                   max(crestBio_grouped$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# /END!



