
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
library(tidyverse)
#library(saaWeb)   # remotes::install_git("https://github.com/Pacific-salmon-assess/saaWeb") 



# Helpers ----------------
"%notin%" <- Negate("%in%")
analysis_year <- 2024
options(scipen = 9999)


#############################################################################################################################################################

#                                                                           I. ESCAPEMENT BIODATA LOAD 


# 1. Examine escapement biodata files available ----------------  **delete soon if updated file call below works 
# list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement/"), 
#            recursive=F, pattern="^[^~]*.xlsx") 

# 2. Select the most recent one. This is manual because the naming convention sucks ----------------  **delete soon if updated file call below works 
# esc_biodata_recent_filename <- list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement"),
#                                           recursive=F, pattern="^[^~]*_WCVI_Escapement-FSC_BioData*.xlsx")   # <<<< add a file index value if needed e.g., [1]

#3. Read in the file and reformat (slow) ----------------
wcviCNescBiodat <- readxl::read_excel(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement/",
                                                      pattern = "^\\d{4}-\\d{4}_WCVI_Escapement-FSC_BioData.xlsx$",    
                                                      full.names = TRUE), 
                                      sheet=grep("Biodata 2015-", 
                                                 readxl::excel_sheets(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement/",
                                                                                      pattern = "^\\d{4}-\\d{4}_WCVI_Escapement-FSC_BioData.xlsx$",    
                                                                                      full.names = TRUE)),
                                                 ignore.case=T, value=T),
                                      guess_max=10000) %>% 
  select(Year, `Sample Month`:Species, `Fishery / River`:Gear, Sex, `POF Length (mm)`:`Egg Retention`, Comments) %>%
  mutate(`(R) OTOLITH LBV CONCAT` = case_when(!is.na(`Otolith Lab Number`) & !is.na(`Otolith Box #`) & !is.na(`Otolith Specimen #`) ~ 
                                                paste0(`Otolith Lab Number`,sep="-",`Otolith Box #`,sep="-",`Otolith Specimen #`)),
         `(R) SCALE BOOK NUM` = case_when(`Scale Book #` %in% c(15983:15987,15989) ~ paste0("22SC ", `Scale Book #` ),
                                          `Scale Book #` %in% c(17376,17377) ~ paste0("22SP", `Scale Book #` ),
                                          nchar(`Scale Book #`)==4 ~ paste0("0",`Scale Book #`),
                                          TRUE ~ `Scale Book #`),
         `(R) SCALE CELL NUM` = case_when(grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="11" ~ "2",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="21" ~ "3",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="31" ~ "4",
                                          grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #`=="41" ~ "5",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("1-41", `Scale #`) ~ "1",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("2-42", `Scale #`) ~ "2",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("3-43", `Scale #`) ~ "3",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("4-44", `Scale #`) ~ "4",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("5-45", `Scale #`) ~ "5",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("6-46|6--46", `Scale #`) ~ "6",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("7-47", `Scale #`) ~ "7",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("8-48", `Scale #`) ~ "8",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("9-49", `Scale #`) ~ "9",
                                          is.na(`Scale Format (5 down, 2 across, etc)`) & grepl("10-50", `Scale #`) ~ "10",
                                          TRUE ~ `Scale #`),
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & !is.na(`Scale #`) ~ paste0(`(R) SCALE BOOK NUM`,sep="-",`(R) SCALE CELL NUM`)),
         # `(R) FIELD SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) FIELD SCALE BOOK NUM`) ~ paste0(`(R) FIELD SCALE BOOK NUM`,"-",`(R) SCALE CELL NUM`),
         #                                                TRUE ~ NA),
         # `(R) RESOVLED SCALE BOOK-CELL CONCAT` = case_when(is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) SCALE BOOK-CELL CONCAT`,
         #                                                   !is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) FIELD SCALE BOOK-CELL CONCAT`,
         #                                                   TRUE~"FLAG"),
         RrowID = seq(1:nrow(.)),
         Sex = case_when(Sex=="Female" ~ "F",
                         Sex=="Male" ~ "M",
                         TRUE ~ Sex),
         `Fishery / River` = case_when(`Fishery / River`=="Moheya River" ~ "Moyeha River",
                                       TRUE ~ `Fishery / River`),
         `(R) DNA NUM` = case_when(!is.na(`Specimen Reference DNA #...43`) ~ `Specimen Reference DNA #...43`,
                                   is.na(`Specimen Reference DNA #...43`) & (!grepl("Whatman",`DNA Container Type 1`) & 
                                                                               !is.na(`DNA Container Type 1`) & 
                                                                               `DNA Container Type 1`%notin%c("FIN", "vial")) ~  `DNA Container Type 1`,
                                   TRUE ~ NA)) %>%
  rename(`(R) OTOLITH BOX NUM`=`Otolith Box #`,
         `(R) OTOLITH VIAL NUM` = `Otolith Specimen #`,
         `(R) OTOLITH LAB NUM` = `Otolith Lab Number`,
         `(R) SAMPLE YEAR` = Year,
         `(R) HEAD LABEL` = `CWT Head Label #`) %>%
  drop_na(`(R) SAMPLE YEAR`) %>%    # remove entries without year specified
  mutate_at(c("(R) SCALE BOOK NUM", "(R) HEAD LABEL", "(R) SAMPLE YEAR"), as.character) %>%
  print()


#############################################################################################################################################################

#                                                                           II. CWT HEAD RECOVERIES WITH RESULTS



# ======================== Load head recovery data ========================  

# Option 1:     Run CWT compile/join scripts --------------------------- (*slow*)
# Do this if you need to add a new year, otherwise move to Option2  
#  source(here::here("scripts","joins","1-CWT_recoveries_with_results")) 
  # saves as CN_headRcvyResults


# Option 2:    Load already saved/exported head recoveries with results --------------------------- (not quite as slow)
# Do this if you are just loading already compiled head recoveries (i.e., you already ran "1-CWT_recoveries_with_results")
CN_headRcvyResults <- readxl::read_excel(path=list.files(path = here::here("outputs"),
                                                         pattern = "^R_OUT - MRPHeadRecoveries_CHINOOK_WITH RESULTS_",   
                                                         full.names = T),
                                         sheet="Sheet1",
                                         trim_ws=T)


# !! MAY GET:    Error: Std:: bad_alloc()  
# It's a memory issue. Either try quitting R session and re-starting, or may have to close programs/restart computer. 
# If really in a pickle, convert the xlsx to a csv usually works, but can ruin column names/formatting


#############################################################################################################################################################

#                                                                           III. JOIN BIODATA to CWT HEAD RECOVERIES WITH RESULTS 


# ======================== JOIN ESCAPEMENT BIODATA+PADS+OTO+NPAFC to HEADS ========================  
intersect(colnames(wcviCNescBiodat), colnames(CN_headRcvyResults))

esc_biodata_headsCWT <- left_join(wcviCNescBiodat,
                               CN_headRcvyResults %>% 
                                 mutate_at("(R) SAMPLE YEAR", as.character),
                               na_matches="never") %>% 
  mutate(`(R) TAGCODE` = MRP_TagCode) %>%
  print()



#############################################################################################################################################################


# << PROBABLY RETIRE THIS SECTION - HAS BEEN STREAMLINED INTO 1-CWT_recoveries_with_results >>


# LOAD CWT TAGCODE IDs


# 
# # 0. RUN CWT RELEASE CODE DUMP ONCE PER UPDATE (i.e., should only need to run line below a couple times a year)
# # source(here::here("scripts", "functions", "pullChinookCWTReleases.R"))
# 
# # 1. Load pre-dumped tagcode releases (dumped in Step 0 above) - DO NOT NEED TO DO IF YOU DO STEP 0
# cn_relTagCodes <- readxl::read_excel(path=list.files(path = here::here("outputs"),
#                                                        pattern = "^R_OUT - Chinook CWT release tagcodes BY",   # use ^ to ignore temp files, eg "~R_OUT - ALL...,
#                                                        full.names = TRUE), 
#                                        sheet="Sheet1")  
# 
# 
# 
# #############################################################################################################################################################
# 
# #                                                                           V. JOIN BIODATA+HEADS to CWT TAGCODE ID
# 
# 
# # ======================== JOIN ESCAPEMENT BIODATA+HEADS to TAGCODE ID ========================  
# intersect(colnames(esc_biodata_heads), colnames(cn_relTagCodes))
# 
# esc_biodata_headsCWT <- left_join(esc_biodata_heads,
#                                   cn_relTagCodes,
#                                   by="(R) TAGCODE") %>%     #Needed or else links on comments field too 
#   mutate(`(R) BROOD YEAR: CWT` = `MRP_Brood Year`,
#          `(R) TOTAL AGE: CWT` = as.numeric(`(R) SAMPLE YEAR`) - `MRP_Brood Year`,
#          `(R) STOCK ID: CWT` = case_when(!is.na(`MRP_Stock Site Name`) ~ 
#                                            gsub(" Cr", "", 
#                                                 gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
#                                                 ignore.case=F),
#                                          TRUE ~ NA)) %>%
#   print()
# 


#############################################################################################################################################################

#                                                                           VI. SCALE AGE DATA LOAD 


# ======================== Load age data ========================  

# Option 1: Run helper script to pull/compile scale data (saves as 'SC_age_data') --------------------------- (*slow*)
# Do this if you need to add a new year. Otherwise, do option 2. 
#  source(here::here("scripts", "functions", "pullChinookAgeData.R"))


# Option 2: Load already saved exported age master file --------------------------- (faster)
# Do this if you are just loading already compiled data
SC_allAgesMaster <- readxl::read_excel(path=list.files(path = here::here("outputs"),
                                                  pattern = "^R_OUT - ALL South Coast Chinook Age results",   # use ^ to ignore temp files, eg "~R_OUT - ALL...,
                                                  full.names = TRUE), 
                                  sheet="Sheet1")  %>% 
  mutate_at("(R) SAMPLE YEAR", as.character)




#############################################################################################################################################################

#                                                                           III. JOIN ESCAPEMENT BIODATA+HEADS+CWT IDs to AGES, CALC BY



# ======================== JOIN ESCAPEMENT BIODATA+HEADS+CWTs to PADS, calc BY ========================  
intersect(colnames(esc_biodata_headsCWT), colnames(SC_allAgesMaster))

# IF you get a many-to-many error, you have duplicate scalebooks that were handed out to different regions. 
# Will need to further filter down the SC_age_data file
esc_biodata_headsCWT_PADS <- left_join(esc_biodata_headsCWT,
                                       SC_allAgesMaster,
                                       na_matches="never") %>%
  mutate(`(R) TOTAL AGE: SCALE` = case_when(!is.na(PADS_GrAge) & !grepl("M|F", PADS_GrAge) ~ as.numeric(paste0(substr(PADS_GrAge,1,1))),
                                             `PADS_GrAge`=="1M" ~ 2,
                                             `PADS_GrAge`=="2M" ~ 3,
                                             `PADS_GrAge`=="3M" ~ 4,
                                             `PADS_GrAge`=="4M" ~ 5,
                                             `PADS_GrAge`=="5M" ~ 6,
                                             `PADS_GrAge`=="6M" ~ 7),
         `(R) BROOD YEAR: SCALE` = as.numeric(`(R) SAMPLE YEAR`) - `(R) TOTAL AGE: SCALE`) %>%
         #`(R) RESOLVED BROOD YEAR` = case_when(!is.na(`(R) TOTAL AGE: CWT`) ~ as.numeric(`(R) SAMPLE YEAR`) - `(R) TOTAL AGE: CWT`,
          #                                     is.na(`(R) TOTAL AGE: CWT`) ~ as.numeric(`(R) SAMPLE YEAR`) - `(R) TOTAL AGE: SCALE`,
           #                                    TRUE ~ NA)) %>% 
  arrange(`(R) SAMPLE YEAR`) %>%
  relocate(c(`(R) BROOD YEAR: CWT`, `(R) TOTAL AGE: CWT`, `(R) STOCK ID: CWT`), .before=`(R) TOTAL AGE: SCALE`) %>%
  print()



# ANTI JOINS: Scale samples that didn't make it in to the escapement biodata basefile ---------------------------
# 1. Extract "successful" scale book-cell values from the joined file (e.g., scale books with attached non-NA scale ages)
available_age_results <- esc_biodata_headsCWT_PADS %>%
  filter(!is.na(`(R) SCALE BOOK-CELL CONCAT`) & !is.na(`(R) TOTAL AGE: SCALE`)) %>% 
  pull(`(R) SCALE BOOK-CELL CONCAT`)

# 2. Filter - remove the successful results from the join from the age data dump and save only the non-successful join (orphans)
antijoin_PADS <- SC_allAgesMaster %>% 
  filter(`(R) SCALE BOOK-CELL CONCAT` %notin% available_age_results & !grepl("creel|test", PADS_ProjectName, ignore.case=T)) %>% 
  print()



#############################################################################################################################################################

#                                                                           IV. OTOLITH DATA LOAD 

# <<< For each new year: >>> 
  # 1. manually download newest year's Recovery Specimens file: http://devios-intra.dfo-mpo.gc.ca/Otolith/Reports/ [reference/recovery specimens]
  # 2. Store in SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ [reference/recovery] / Import
    # FOLLOW NAMING CONVENTION >:(
  # 3. Run Option 1 source() line once/year to add new year. Otherwise, run option 2.



# ======================== Load otolith data ========================  

# Option 1: Run helper script to pull/compile otolith data (saves as 'wcviOtos') --------------------------- (*slow*)
# Do this if you need to add a new year. Otherwise, do option 2. 
#  source(here::here("scripts","misc-helpers","OtoCompile.R")) 


# Option 2: Load already saved exported otolith master file --------------------------- (faster)
# Do this if you are just loading already compiled data
wcviOtos <- readxl::read_excel(path=list.files(path = here::here("outputs"),
                                               pattern = "^R_OUT - OtoManager_AllSpecies_Area20-27andOffshore_",   # use ^ to ignore temp files, eg "~R_OUT - ALL...,
                                               full.names = TRUE), 
                               sheet="Sheet1")  %>% 
  mutate_at("(R) SAMPLE YEAR", as.character) %>% 
  mutate(OM_FACILITY = case_when(OM_FACILITY=="H-ROBERTSON CR" ~ "H-ROBERTSON CREEK H",
                                 OM_FACILITY=="H-CONUMA R " ~ "H-CONUMA RIVER H ",
                                 grepl("GWA'NI", OM_FACILITY) ~ "H-GAW'NI H",
                                 grepl("INCH CR", OM_FACILITY) ~ "H-INCH CREEK H",
                                 grepl("NANAIMO", OM_FACILITY) ~ "H-NANAIMO RIVER H",
                                 grepl("NITINAT", OM_FACILITY) ~ "H-NITINAT RIVER H",
                                 grepl("QUINSAM", OM_FACILITY) ~ "H-QUINSAM RIVER H",
                                 grepl("WALLACE", OM_FACILITY) ~ "WALLACE RIVER HATCHERY"))





# Extract lab numbers for QC ---------------------------
write.csv(wcviOtos %>% 
            filter(grepl("scapement", OM_SOURCE)) %>%
            group_by(`(R) SAMPLE YEAR`, OM_SPECIES, `OM_RCVY LOCATIONS`, `(R) OTOLITH LAB NUM`) %>%
            summarize(n()),
          "C:/Users/DAVIDSONKA/Desktop/oto lab nums.csv", row.names=F)



#############################################################################################################################################################

#                                                                           V. JOIN ESC BIODATA+HEADS+CWT IDs+PADS to OTOMGR 


# ======================== JOIN ESC+HEADS+CWT IDs+PADS to OTOMGR ========================  
intersect(colnames(esc_biodata_headsCWT_PADS), colnames(wcviOtos))


esc_biodata_headsCWT_PADS_oto <- left_join(esc_biodata_headsCWT_PADS,
                                           wcviOtos %>%
                                             filter(OM_SPECIES=="Chinook") %>% 
                                             mutate_at("(R) SAMPLE YEAR", as.character),
                                           #by=c("(R) OTOLITH BOX NUM", "(R) OTOLITH VIAL NUM", "(R) SAMPLE YEAR", "(R) OTOLITH LBV CONCAT"),
                                           na_matches="never") %>% 
  mutate(`(R) BYHID` = case_when(!is.na(`(R) BROOD YEAR: SCALE`) & !is.na(`(R) HATCHCODE`) ~ paste0(`(R) BROOD YEAR: SCALE`, " - ", `(R) HATCHCODE`),
                                 TRUE ~ NA),
         `(R) oto join BY` = `(R) BROOD YEAR: SCALE`) %>% 
  print()



# ANTI JOINS: Oto samples that didn't make it in to the escapement biodata basefile ---------------------------
# 1. Extract "successful" otolith lab-box-vial values from the joined file (e.g., vials with attached non-NA BY-hatch code IDs)
availabe_oto_results <- esc_biodata_headsCWT_PADS_oto %>%
  filter(!is.na(`(R) OTOLITH LBV CONCAT`) & !is.na(`(R) BYHID`)) %>% 
  pull(`(R) OTOLITH LBV CONCAT`)

# 2. Filter - remove the successful results in the join from the oto data dump and save only the non-successful join results (orphans) 
antijoin_OM <- wcviOtos %>% 
  filter(`(R) OTOLITH LBV CONCAT` %notin% availabe_oto_results & OM_SOURCE != "Sport") %>% 
  print()



#############################################################################################################################################################

#                                                                           VI. LOAD NPAFC


prob_orders <- factor(c("V LOW", "LOW", "MED", "HIGH"), levels=c("V LOW", "LOW", "MED", "HIGH"), ordered=T)


# Load NPAFC CN mark master file ---------------------------

NPAFC <- readxl::read_excel(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All CN Marks",   #ignore temp files, eg "~All CN Marks...,
                                            full.names = TRUE), 
                            sheet="AC087805 (1)") %>% 
  setNames(paste0('NPAFC_', names(.))) %>% 
  rename(`(R) HATCHCODE` = NPAFC_HATCH_CODE,
         `(R) oto join BY` = NPAFC_BROOD_YEAR) %>% 
  mutate(NPAFC_FACILITY = case_when(is.na(NPAFC_FACILITY) ~ NPAFC_AGENCY,
                                    TRUE ~ NPAFC_FACILITY),
         NPAFC_STOCK = case_when(is.na(NPAFC_STOCK) ~ NPAFC_FACILITY,
                                 TRUE ~ NPAFC_STOCK),
         `(R) BYHID` = case_when(!is.na(`(R) oto join BY`) & !is.na(`(R) HATCHCODE`) ~ paste0(`(R) oto join BY`, " - ", `(R) HATCHCODE`),
                                TRUE ~ NA)) %>%
  # This filter line below initially removed some marks to avoid duplicates, but the work-around below now addresses this more systematically. Below is just
  #     kept for reference for now
  #filter(NPAFC_STATE_PROVINCE %in% c("BRITISH COLUMBIA", "IDAHO", "WASHINGTON", "OREGON"),
         # 2. Remove the one case where RCH and Nanaimo Hatchery used the same mark in 2018 and assume it was a RCH fish
         #!grepl("NANAIMO", NPAFC_FACILITY) | `(R) BROOD YEAR`!=2018 | `(R) HATCHCODE`!="H5"
         #) %>%
  select(`(R) oto join BY`, NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_REGION, `(R) BYHID`, NPAFC_NUMBER_RELEASED) %>% 
  distinct(`(R) oto join BY`, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_FACILITY, NPAFC_STOCK, .keep_all=T) %>% 
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
  group_by(`(R) oto join BY`, `(R) HATCHCODE`, `(R) BYHID`) %>% 
  pivot_wider(names_from = group,
              values_from= c(NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, NPAFC_STATE_PROVINCE, NPAFC_REGION, NPAFC_wcvi_prob, NPAFC_NUMBER_RELEASED)) %>%
  select(`(R) oto join BY`, `(R) HATCHCODE`, `(R) BYHID`, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  print()



# Cases where multiple stocks within a BY received the same hatchcode ---------------------------
NPAFC_dupl <- NPAFC %>% 
  filter(!is.na(NPAFC_FACILITY_2)) %>% 
  pull(`(R) BYHID`)





#############################################################################################################################################################

#                                                                           VII. JOIN ESC BIODATA+HEADS+CWT IDs+PADS+OTOMGR to NPAFC


# # QC flag: duplicate BY-hatchcodes in our esc biodata ------------------------
 esc_biodata_PADS_otoDUPS <- esc_biodata_headsCWT_PADS_oto %>% 
   filter(`(R) BYHID` %in% NPAFC_dupl) %>% 
   print()


# ======================== JOIN ESCAPEMENT BIODATA+PADS+OTOMGR to NPAFC ========================  
intersect(colnames(esc_biodata_headsCWT_PADS_oto), colnames(NPAFC))

# Joining to NPAFC FIX: 
# Originally, there were cases where the same BY received the same hatchcode, so we need a way to join on this. 
# Solution: Now that the NPAFC file has been re-organized to identify multiple stock IDs within a BY-HID (e.g., NPAFC_STOCK_1, NPAFC_STOCK_2, etc.), don't need to worry about complex joins/renaming. R will do it all. 
esc_biodata_headsCWT_PADS_otoNPAFC <- left_join(esc_biodata_headsCWT_PADS_oto,
                                                NPAFC,
                                                na_matches="never") %>% 
  relocate(c(`(R) BROOD YEAR: CWT`, `(R) TOTAL AGE: CWT`, `(R) STOCK ID: CWT`, `(R) BROOD YEAR: SCALE`, `(R) TOTAL AGE: SCALE`), 
           .after=last_col()) %>%
  select(-`(R) oto join BY`) %>%
  mutate(# Before identifying otolith ID, determine the certainty of the ID (accounts for any duplication of hatchcodes within a BY)
    `(R) OTOLITH ID METHOD` = case_when(!is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ "To stock (certain)",
                                        !is.na(NPAFC_STOCK_1) & !is.na(NPAFC_STOCK_2) ~ 
                                          "Duplicate BY-hatchcode at >1 facility, assumed stock ID (moderately certain ID)",
                                        is.na(NPAFC_STOCK_1) & !is.na(OM_FACILITY) ~ "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)",
                                        TRUE~NA),
    
    
    # Identify otolith stock ID - Note at this point it is irrelevant if a CWT exists because we want to test later whether CWT ID and Otolith ID agree
    `(R) STOCK ID: OTOLITH` = case_when(
      # 5 a) Single otolith stock choice (certain ID)
      !is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ 
        gsub(" R", "", 
             gsub("River", "R",
                  gsub(" Cr", "",  
                       gsub("S-", "",
                            stringr::str_to_title(
                              stringr::str_sub(NPAFC_STOCK_1,1,100)), 
                            ignore.case = F), 
                       ignore.case = F), 
                  ignore.case=F),   
             ignore.case=F),
      
      # 5 b) Multiple HIGH probability otolith matches, flag for manual ID: 
      (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
        (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2=="HIGH" | NPAFC_wcvi_prob_3=="HIGH" | NPAFC_wcvi_prob_4=="HIGH") ~  
        "!! manual decision needed, refer to release sizes!!",
      
      # 5 c) Multiple MEDIUM probability otolith matches, flag for manual ID: 
      (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
        (NPAFC_wcvi_prob_1=="MED" & NPAFC_wcvi_prob_2=="MED" | NPAFC_wcvi_prob_3=="MED" | NPAFC_wcvi_prob_4=="MED") ~  
        "!! manual decision needed, refer to release sizes!!",
      
      # 5 d) Multiple otolith matches but Stock1 is HIGH probability and the rest are NOT, therefore choose Otolith stock 1: 
      (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
        (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2!="HIGH" | NPAFC_wcvi_prob_3!="HIGH" | NPAFC_wcvi_prob_4!="HIGH") ~  
        gsub(" R", "",
             gsub(" Cr", "",  
                  stringr::str_to_title(
                    stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                  ignore.case = F), 
             ignore.case=F),
      
      # 5 e) Multiple otolith matches but Stock1 is med-high probability and the rest are not, still choose Otolith stock 1: 
      (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
        (NPAFC_wcvi_prob_1=="MED-HIGH" & NPAFC_wcvi_prob_2!="MED-HIGH" | NPAFC_wcvi_prob_3!="MED-HIGH" | NPAFC_wcvi_prob_4!="MED-HIGH") ~  
        gsub(" R", "",
             gsub(" Cr", "",  
                  stringr::str_to_title(
                    stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                  ignore.case = F), 
             ignore.case=F),
      
      # 5 f) LOW probability otoliths, just choose stock1:
      (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
        (NPAFC_wcvi_prob_1%in%c("MED","LOW","V LOW") & NPAFC_wcvi_prob_2%in%c("LOW", "V LOW") | NPAFC_wcvi_prob_3%in%c("LOW", "V LOW")  | NPAFC_wcvi_prob_4%in%c("LOW", "V LOW") ) ~  
        gsub(" R", "",
             gsub(" Cr", "",  
                  stringr::str_to_title(
                    stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                  ignore.case = F), 
             ignore.case=F),
      
      # 5 g) Otolith ID method is uncertain and requires using the Oto Manager Facility (due to duplicate BY-hatch codes)
      `(R) OTOLITH ID METHOD` == "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)" ~
        gsub("H-", "",
             gsub(" R", "",
                  gsub(" River H", "",
                       gsub(" Creek H", "",
                            stringr::str_to_title(OM_FACILITY),
                            ignore.case = F),
                       ignore.case = F),
                  ignore.case=F),
             ignore.case=F),
      
      
      TRUE ~ NA)) %>%
    #//end "(R) STOCK ID: OTOLITH")
  print()



#############################################################################################################################################################

#                                                                           XIII. LOAD PBT DATA

# ======================== Load PBT results ========================  
PBT_results <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/2023-09-14 Chinook_Brood_2013-2021_PBT_results.xlsx",
                                 sheet="Sheet1", guess_max=10000) %>% 
  setNames(paste0('MGL_', names(.))) %>% 
  mutate(`(R) DNA NUM` = MGL_oFish,
         `(R) SAMPLE YEAR` = MGL_oYear,
         MGL_Brood_Collection = str_to_title(gsub("_", " ", MGL_Brood_Collection, ignore.case = T)),
         `Fishery / River` = MGL_Brood_Collection) %>% 
  mutate_at(c("(R) DNA NUM", "(R) SAMPLE YEAR"), as.character) %>%
  filter(grepl("Bedwell|Burman|Conuma|Cowichan|Cypre|Gold|Kennedy|Leiner|Campbell|Qualicum|Marble|Nahmint|Nanaimo|Nimpkish|Nitinat|
               Oyster|Phillips|Puntledge|Quinsam|Robertson|Salmon River Jnst|San Juan|Sarita|Sooke|Tahsis|Thornton|Toquart|Tranquil", 
               MGL_Brood_Collection, ignore.case=T)) %>%
  print()


# ======================== Load PBT inventory ========================  
# Load tag rate file -------------------------   

#*** Don't use network version for now. Slowww, had to make 1 manual update to SJ for BY 2018
 #SC_PBT_tagrate <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/bch_v4.2b_2013-2022_brood-counts_tagging_rates_2024-06-05.xlsx",
  #                                      sheet="ch_supplementary_file_to-check", guess_max=10000) 

# Load from github csv for now -------------------------
# Extract reliable PBT baseline BYs
PBT_tagrate <- read.csv(here::here("data", "bch_v4.2b_2013-2022_brood-counts_tagging_rates_2024-06-05.csv")) %>%  
  filter(grepl("Tag_Rate", collection_extract)) %>%
  pivot_longer(cols=c(ALOUETTE_RIVER:YUKON_RIVER.WHITEHORSE), names_to = "System", values_to = "Tag_Rate") %>% 
  rename(`(R) SAMPLE YEAR` = collection_extract) %>% 
  mutate(`(R) SAMPLE YEAR` = str_sub(`(R) SAMPLE YEAR`, start=1, end=4)) %>%
  mutate(Tag_Rate = case_when(grepl(">1", Tag_Rate) ~ "1",
                              grepl("\\*", Tag_Rate) ~ "1",
                              TRUE ~ Tag_Rate)) %>%
  mutate_at("Tag_Rate", as.numeric) %>%
  filter(Tag_Rate > 0.70) %>%
  left_join(.,
            read.csv(here::here("data", "bch_v4.2b_2013-2022_brood-counts_tagging_rates_2024-06-05.csv")) %>%  
              filter(grepl("ProvState_extract", collection_extract)) %>% 
              pivot_longer(cols=c(ALOUETTE_RIVER:YUKON_RIVER.WHITEHORSE), names_to = "System", values_to = "Province") %>%
              select(-collection_extract),
            by="System") %>%
  relocate("Province", .before=Tag_Rate) %>% 
  mutate(System = str_to_title(gsub(x=gsub(x=System, pattern="_", replacement=" "), pattern="\\.", replacement=" ")),
         `(R) SYSTEM-YEAR` = paste0(System, sep= "-", `(R) SAMPLE YEAR`))
  

  # 
# # Define function to find rolling 5 year window of full PBT results ------------------------- (not needed)
# # TY chatGPT :)
# findFirstFullPBTBY <- function(x, other_col) {
#   x <- ifelse(is.na(other_col), NA, x)
#   roll_max <- rollapply(x, width=5, FUN=max, align="right", fill=NA)
#   return(roll_max)
# }


# # Export -------------------------:
writexl::write_xlsx(PBT_tagrate,
                    "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Reliable PBT (over 0.7) BYs by stock - draft working.xlsx")



#############################################################################################################################################################

#                                                                           XIV. JOIN BIODATA+PADS+OTO+NPAFC+HEADS+CWT ID to PBT RESULTS


# ======================== JOIN ESCAPEMENT BIODATA+PADS+OTO+NPAFC+HEADS+CWT ID to PBT!! ========================  
intersect(colnames(PBT_results), colnames(esc_biodata_headsCWT_PADS_otoNPAFC))

esc_biodata_headsCWT_PADS_otoNPAFC_PBT <- left_join(esc_biodata_headsCWT_PADS_otoNPAFC,
                                                    PBT_results,
                                                    by=c("(R) SAMPLE YEAR", "(R) DNA NUM", "Fishery / River")) %>% 
  mutate(`(R) TOTAL AGE: PBT` = MGL_Offspring_Age,
         `(R) BROOD YEAR: PBT` = as.numeric(`(R) SAMPLE YEAR`) - MGL_Offspring_Age,
         `(R) STOCK ID: PBT` = case_when(!is.na(MGL_Parental_Collection) ~ str_to_title(gsub(" Creek", "",
                                                                                             gsub(" River", "",
                                                                                                  gsub("_", 
                                                                                                       " ", 
                                                                                                       MGL_Parental_Collection, 
                                                                                                       ignore.case = T),
                                                                                                  ignore.case=T),
                                                                                             ignore.case=T)),
                                         TRUE ~ NA)) %>% 
  relocate(c(`(R) BROOD YEAR: CWT`, `(R) TOTAL AGE: CWT`, `(R) STOCK ID: CWT`, `(R) BROOD YEAR: SCALE`, `(R) TOTAL AGE: SCALE`, `(R) OTOLITH ID METHOD`,
             `(R) STOCK ID: OTOLITH`), .before=`(R) TOTAL AGE: PBT`) %>% 
  print()




#############################################################################################################################################################

#                                                                           XIII. ASSIGN FINAL AGE and STOCK ID


esc_biodata_w_RESULTS <- esc_biodata_headsCWT_PADS_otoNPAFC_PBT %>% 
  mutate(# AGE METHOD: CWT > PBT > scales
         `(R) RESOLVED TOTAL AGE METHOD` = case_when(!is.na(`(R) TOTAL AGE: CWT`) ~ "CWT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & !is.na(`(R) TOTAL AGE: PBT`) ~ "PBT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & is.na(`(R) TOTAL AGE: PBT`) & !is.na(`(R) TOTAL AGE: SCALE`) ~ "Scale",
                                                     TRUE ~ NA),
         # AGE APPLIED: CWT > PBT > scales
         `(R) RESOLVED TOTAL AGE` = case_when(`(R) RESOLVED TOTAL AGE METHOD`=="CWT" ~ `(R) TOTAL AGE: CWT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="PBT" ~ `(R) TOTAL AGE: PBT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="Scale" ~ `(R) TOTAL AGE: SCALE`,
                                              TRUE ~ NA),
         
         # BROOD YEAR: CWT > PBT > scales
         `(R) RESOLVED BROOD YEAR` = case_when(`(R) RESOLVED TOTAL AGE METHOD`=="CWT" ~ `(R) BROOD YEAR: CWT`,
                                                     `(R) RESOLVED TOTAL AGE METHOD`=="PBT" ~ `(R) BROOD YEAR: PBT`,
                                                     `(R) RESOLVED TOTAL AGE METHOD`=="Scale" ~ `(R) BROOD YEAR: SCALE`,
                                                     TRUE ~ NA),
         
         # Assign a System-Year designation to help with identifying natural-origin PBT results for stocks with those BYs completed (used below)
         `(R) SYSTEM-YEAR` = paste0(`Fishery / River`, sep="-", `(R) RESOLVED BROOD YEAR`),

         
         # Identify hatchery/natural origin - not using stock ID columns to avoid any issues with assigning stock ID. 
         `(R) ORIGIN` = case_when(`AD Clipped?` == "Y" ~ "Hatchery",
                                  `OM_READ STATUS` == "Marked" ~ "Hatchery",
                                  !is.na(MGL_Parental_Collection) ~ "Hatchery", 
                                  #This line below is probably redundant as all CWT fish will be ad-clipped
                                  !is.na(`(R) TAGCODE`) ~ "Hatchery",
                                  `OM_READ STATUS` == "Not Marked" ~ "Natural (assumed)",
                                  # This line below looks up any system-year that doesn't have a PBT hit in the tag rate baseline. If the system-year was PBT tagged, but there is no PBT hit, it assigns "natural"
                                  is.na(MGL_Parental_Collection) & `(R) SYSTEM-YEAR` %in% PBT_tagrate$`(R) SYSTEM-YEAR`  ~ "Natural (PBT)",
                                  TRUE ~ "Unknown"),
         
         # 2. Identify CWT Stock ID
         # `(R) CWT STOCK ID` = case_when(!is.na(`MRP_Stock Site Name`) ~ 
         #                                  gsub(" Cr", "", 
         #                                       gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
         #                                       ignore.case=F),
         #                                TRUE ~ NA),
         
         # 3. Identify PBT Stock ID
         # `(R) PBT STOCK ID` = case_when(!is.na(MGL_Parental_Collection) ~ str_to_title(gsub(" Creek", "",
         #                                                                                    gsub(" River", "",
         #                                                                                         gsub("_", 
         #                                                                                              " ", 
         #                                                                                              MGL_Parental_Collection, 
         #                                                                                              ignore.case = T),
         #                                                                                         ignore.case=T),
         #                                                                                    ignore.case=T)),
         #                                TRUE ~ NA),
         
         # 4. Before identifying otolith ID, determine the certainty of the ID (accounts for any duplication of hatchcodes within a BY)
         # `(R) OTOLITH ID METHOD` = case_when(!is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ "To stock (certain)",
         #                                     !is.na(NPAFC_STOCK_1) & !is.na(NPAFC_STOCK_2) ~ 
         #                                       "Duplicate BY-hatchcode at >1 facility, assumed stock ID (moderately certain ID)",
         #                                     is.na(NPAFC_STOCK_1) & !is.na(OM_FACILITY) ~ "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)",
         #                                     TRUE~NA),
         
         
         # 5. Identify otolith stock ID - Note at this point it is irrelevant if a CWT exists because we want to test later whether CWT ID and Otolith ID agree
         # `(R) OTOLITH STOCK ID` = case_when(
         #   # 5 a) Single otolith stock choice (certain ID)
         #   !is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ 
         #     gsub(" R", "", 
         #          gsub("River", "R",
         #               gsub(" Cr", "",  
         #                    gsub("S-", "",
         #                         stringr::str_to_title(
         #                           stringr::str_sub(NPAFC_STOCK_1,1,100)), 
         #                         ignore.case = F), 
         #                    ignore.case = F), 
         #               ignore.case=F),   
         #          ignore.case=F),
         #   
         #   # 5 b) Multiple HIGH probability otolith matches, flag for manual ID: 
         #   (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
         #     (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2=="HIGH" | NPAFC_wcvi_prob_3=="HIGH" | NPAFC_wcvi_prob_4=="HIGH") ~  
         #     "!! manual decision needed, refer to release sizes!!",
         #   
         #   # 5 c) Multiple MEDIUM probability otolith matches, flag for manual ID: 
         #   (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
         #     (NPAFC_wcvi_prob_1=="MED" & NPAFC_wcvi_prob_2=="MED" | NPAFC_wcvi_prob_3=="MED" | NPAFC_wcvi_prob_4=="MED") ~  
         #     "!! manual decision needed, refer to release sizes!!",
         #   
         #   # 5 d) Multiple otolith matches but Stock1 is HIGH probability and the rest are NOT, therefore choose Otolith stock 1: 
         #   (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
         #     (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2!="HIGH" | NPAFC_wcvi_prob_3!="HIGH" | NPAFC_wcvi_prob_4!="HIGH") ~  
         #     gsub(" R", "",
         #          gsub(" Cr", "",  
         #               stringr::str_to_title(
         #                 stringr::str_sub(NPAFC_STOCK_1,3,100)), 
         #               ignore.case = F), 
         #          ignore.case=F),
         #   
         #   # 5 e) Multiple otolith matches but Stock1 is med-high probability and the rest are not, still choose Otolith stock 1: 
         #   (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
         #     (NPAFC_wcvi_prob_1=="MED-HIGH" & NPAFC_wcvi_prob_2!="MED-HIGH" | NPAFC_wcvi_prob_3!="MED-HIGH" | NPAFC_wcvi_prob_4!="MED-HIGH") ~  
         #     gsub(" R", "",
         #          gsub(" Cr", "",  
         #               stringr::str_to_title(
         #                 stringr::str_sub(NPAFC_STOCK_1,3,100)), 
         #               ignore.case = F), 
         #          ignore.case=F),
         #   
         #   # 5 f) LOW probability otoliths, just choose stock1:
         #   (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3) | !is.na(NPAFC_STOCK_4)) & 
         #     (NPAFC_wcvi_prob_1%in%c("MED","LOW","V LOW") & NPAFC_wcvi_prob_2%in%c("LOW", "V LOW") | NPAFC_wcvi_prob_3%in%c("LOW", "V LOW")  | NPAFC_wcvi_prob_4%in%c("LOW", "V LOW") ) ~  
         #     gsub(" R", "",
         #          gsub(" Cr", "",  
         #               stringr::str_to_title(
         #                 stringr::str_sub(NPAFC_STOCK_1,3,100)), 
         #               ignore.case = F), 
         #          ignore.case=F),
         #   
         #   # 5 g) Otolith ID method is uncertain and requires using the Oto Manager Facility (due to duplicate BY-hatch codes)
         #   `(R) OTOLITH ID METHOD` == "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)" ~
         #     gsub("H-", "",
         #          gsub(" R", "",
         #               gsub(" River H", "",
         #                    gsub(" Creek H", "",
         #                         stringr::str_to_title(OM_FACILITY),
         #                         ignore.case = F),
         #                    ignore.case = F),
         #               ignore.case=F),
         #          ignore.case=F),
         #   
         #   
         #   TRUE ~ NA),
         #//end 5. "(R) OTOLITH STOCK ID"
         
         
         # 6. Identify where may be able to apply the Fishery/River location as finer scale than Otolith facility for uncertain oto ID cases (e.g., Sooke / Nitinat)
         `(R) OTO STOCK != FISHERY/RIVER` = case_when(`(R) OTOLITH ID METHOD` == "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)" &
                                                        `(R) STOCK ID: OTOLITH` != gsub(" River", "",
                                                                                       gsub(" Creek", "", `Fishery / River`,
                                                                                            ignore.case = F),
                                                                                       ignore.case = F) ~
                                                        "Flag: Consider substituting Fishery/River as stock ID.",
                                                      TRUE ~ NA
                                                       )
         ) %>%
  mutate(
    
    # 7. Identify the method used to determine the final stock ID: CWT > PBT > Otolith > No PBT 
    `(R) RESOLVED STOCK ID METHOD` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ "CWT",
                                               is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) ~ "PBT",
                                               is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) & !is.na(`(R) OTOLITH ID METHOD`) ~ paste0("Otolith", sep=" - ", `(R) OTOLITH ID METHOD`),
                                               `(R) ORIGIN`=="Natural (assumed)" ~ "Otolith (no mark)",
                                               `(R) ORIGIN`=="Natural (PBT)" ~ "PBT (no match)",
                                               TRUE ~ NA),
    
    
    # 8. Assign the final stock ID: CWT > PBT > Otolith (with varying levels of oto certainty) > Natural IDs
    `(R) RESOLVED STOCK ID` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ `(R) STOCK ID: CWT`,
                                        is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) ~ `(R) STOCK ID: PBT`,
                                        is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) & !is.na(`(R) STOCK ID: OTOLITH`) ~ `(R) STOCK ID: OTOLITH`,
                                        `(R) ORIGIN`=="Natural (assumed)" ~ paste0(stringr::str_to_title(gsub(" River", "",
                                                                                                              gsub(" Creek", "",
                                                                                                                   `Fishery / River`,
                                                                                                                   ignore.case=F),
                                                                                                              ignore.case=F)), 
                                                                                   " (assumed)"),
                                        `(R) ORIGIN`=="Natural (PBT)" ~ paste0(stringr::str_to_title(gsub(" River", "",
                                                                                                    gsub(" Creek", "",
                                                                                                         `Fishery / River`,
                                                                                                         ignore.case=F),
                                                                                                    ignore.case=F)),
                                                                               " (assumed)"),
                                        TRUE ~ "Unknown"),
    
    
    # 9. Combine the origin and ID into the final grouping level for the Term Run files 
    `(R) RESOLVED STOCK-ORIGIN` = paste0(`(R) ORIGIN`, sep=" ", `(R) RESOLVED STOCK ID`),
    
    
    # 10. Create flag for cases where CWT and Otolith IDs disagree
    `(R) STOCK ID FLAG: CWT-OTO` = case_when(`(R) STOCK ID: CWT` != `(R) STOCK ID: OTOLITH` ~ "FLAG: CWT/Otolith stock ID disagree",
                                             TRUE ~ NA),
    `(R) STOCK ID FLAG: CWT-PBT` = case_when(`(R) STOCK ID: CWT` != `(R) STOCK ID: PBT` ~ "FLAG: CWT/PBT stock ID disagree",
                                             TRUE ~ NA),
    `(R) STOCK ID FLAG: PBT-OTO` = case_when(`(R) STOCK ID: OTOLITH` != `(R) STOCK ID: PBT` ~ "FLAG: PBT/Otolith stock ID disagree",
                                             TRUE ~ NA),
    
    
    # 11. Create flag for cases where PBT, CWT and/or scale age(s) disagree
    `(R) AGE FLAG: CWT-SCALE` = case_when(`(R) TOTAL AGE: CWT` != `(R) TOTAL AGE: SCALE` ~ "FLAG: CWT/scale ages disagree",
                                          TRUE ~ NA),
    `(R) AGE FLAG: CWT-PBT` = case_when(`(R) TOTAL AGE: CWT` != `(R) TOTAL AGE: PBT` ~ "FLAG: CWT/PBT ages disagree",
                                        TRUE ~ NA),
    `(R) AGE FLAG: PBT-SCALE` = case_when(`(R) TOTAL AGE: SCALE` != `(R) TOTAL AGE: PBT` ~ "FLAG: PBT/scale ages disagree",
                                          TRUE ~ NA)
  ) %>%
  print()






  
  

#############################################################################################################################################################

#                                                                           VIII. QC REPORT and README


# ======================== Extract PBT parental records ========================  
PBT_parents <- esc_biodata_w_RESULTS %>% 
  filter(`(R) DNA NUM` %in% c(PBT_results[!is.na(PBT_results$MGL_mFish),]$MGL_mFish, 
                              PBT_results[!is.na(PBT_results$MGL_dFish),]$MGL_dFish))


# QC flags ---------------------------

# NPAFC duplicates - key for assigning final stock IDs for orphan otolith samples with duplicate BY-hatchcodes (already defined above)
NPAFC_dupl.df <- NPAFC %>% 
  filter(!is.na(NPAFC_STOCK_2)) %>%
  print()

# Entries in the escapement biodata that have >1 NPAFC stock ID option due to duplicated BY-hatchcode applications or only assumed via facility
qc_EBwR_uncertOtoID <- esc_biodata_w_RESULTS %>% 
  filter(grepl("assumed", `(R) OTOLITH ID METHOD`)) %>% 
  print()

# Otolith hatch code and BY available but no NPAFC stock ID (Oto read errors)
qc_noOtoID <- esc_biodata_w_RESULTS %>%
  filter(!is.na(`(R) RESOLVED BROOD YEAR`) & !is.na(`(R) HATCHCODE`) & `(R) HATCHCODE` %notin% c("Destroyed", "Not Marked", "No Sample") & is.na(NPAFC_STOCK_1)) %>%
  print()

# Otolith sample available but no result (Oto processing error)
qc_noOtoResults <- esc_biodata_w_RESULTS %>%
  filter(!is.na(`(R) OTOLITH LBV CONCAT`) & !is.na(`(R) RESOLVED BROOD YEAR`) & is.na(`(R) HATCHCODE`) & `OM_READ STATUS`!="Not Marked") %>%
  print()

# E-Label collected but missing CWT ID results (CWT processing error)
 qc_noCWTID <- esc_biodata_w_RESULTS %>%
   filter(!is.na(`(R) HEAD LABEL`) & is.na(`MRP_TagCode`)) %>% 
   print()

# CWT or Otolith stock ID available but not populated in final Stock ID column (R code error)
qc_noRslvdID <- esc_biodata_w_RESULTS %>% 
  filter(`(R) RESOLVED STOCK ID`=="Unknown" & (!is.na(NPAFC_STOCK_1) | !is.na(`MRP_Stock Site Name`))) %>% 
  print()

# Stock IDs contradict (stock ID error?)
qc_unRslvdID <- esc_biodata_w_RESULTS %>% 
  filter(if_any(c(`(R) STOCK ID FLAG: CWT-OTO`:`(R) STOCK ID FLAG: PBT-OTO`), ~ grepl("FLAG", .x))) %>%
  print()

# Ages contradict (aging error?)
qc_unRslvdAge <- esc_biodata_w_RESULTS %>% 
  filter(if_any(c(`(R) AGE FLAG: CWT-SCALE`:`(R) AGE FLAG: PBT-SCALE`), ~ grepl("FLAG", .x))) %>%
  print()



# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc0 - EBwR unCert Oto",
                                         "qc_noOtoID",
                                         "qc_noResults",
                                         "qc_noCWTID",
                                         "qc_noRslvdID",
                                         "qc_unRslvdID",
                                         "qc_unRslvdAge",
                                         "antijoin - PADS",
                                         "antijoin - Otos"),
                         number_records = c(nrow(qc_EBwR_uncertOtoID),
                                            nrow(qc_noOtoID),
                                            nrow(qc_noOtoResults),
                                            nrow(qc_noCWTID),
                                            nrow(qc_noRslvdID),
                                            nrow(qc_unRslvdID),
                                            nrow(qc_unRslvdAge),
                                            nrow(antijoin_PADS),
                                            nrow(antijoin_OM)),
                         description = c("Esc biodata entries where there was no CWT and duplicate otolith hatch codes were applied within one Brood Year resulting in >1 stock ID options, OR where unable to resolve to Stock level and are left making assumptions based on Facility. These records are still retained in the full biodata file as well, and assumptions are made based on likelihood or facility. These are indicated in the (R) OTOLITH ID METHOD column.",
                                         "Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
                                         "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
                                         "There is a CWT available but no Stock ID.",
                                         "There is a CWT or an NPAFC ID but no Resolved stock ID.",
                                         "Otolith, CWT and/or PBT stock ID(s) do not match.",
                                         "Scale, CWT and/or PBT age(s) do not match.",
                                         "All WCVI CN PADS results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'WCVI Creel Survey' assumed already in CREST. Purpose here is to make sure there are no missing scales expected (i.e., samples not entered in base esc biodata file).",
                                         "All WCVI otolith results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'Sport' assumed already in CREST. Purpose here is to make sure there are no missing otoliths expected (i.e., samples not entered in base esc biodata file).")) %>% 
  print()


# ======================== Create readme ========================
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
                             "PBT parent biodata w RESULTS",
                             "QC Report",
                             "qc0 - EBwR unCert Oto",
                             "!NPAFC_dupl!",
                             "QC...",
                             "antijoin - PADS unmatched",
                             "antijoin - OM unmatched"
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/1-esc_biodata_with_results.R", 
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement/2015-2023_WCVI_Escapement-FSC_BioData.xlsx",
        "via direct R query to http://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/AgeBatchList",
        "For 2022, query from OtoManager online stored in: https://086gc.sharepoint.com/:x:/r/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/WCVI%20STAD/Terminal%20CN%20Run%20Recon/2022/Communal%20data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx?d=w398c15dd3c9b4ceb84d3083a215e9c6a&csf=1&web=1&e=NAxyjd",
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx",
        "!NOT IN YET!: http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice",  
        "",
        "sheet description:",
        "WCVI Chinook escapement biodata joined to PADS scale age results, OtoManager thermal mark results, NPAFC mark file to give otolith stock ID, CWT recoveries, and PBT up to 2021 return year. Currently does NOT include any GSI results.",
        "Subset of full database, filtered by Whatman IDs of parents contributing to returning fish. I.e., if an adult fish sampled for PBT during broodstock had a hit to a parent in the baseline, here their parents are pulled out of the overall database. This would show parents that contributed to adult recruits. These are parent fish identified through PBT only.",
        "Summary of QC flags and # of entries belonging to that flag.",
        "QC flag 0 tab. Only the Esc biodata w RESULTS ('EBwR') entries that correspond to NPAFC BY-hatchcode duplicates. See QC summary for details.",
        "All duplicate BY-hatchcodes documented by the NPAFC. To inform decisions around QC Flag 0.",
        "QC flag tabs. See QC summary report for details.",
        "PADS Antijoin tab. See QC summary for details.",
        "OtoManager Antijoin tab. See QC summary for details."))



#############################################################################################################################################################

#                                                                           X. EXPORT 


# ==================== Create the Excel file ====================
# Create workbook --------------------
R_OUT_ESC.RES <- openxlsx::createWorkbook()

# Add sheets to the workbook --------------------
openxlsx::addWorksheet(R_OUT_ESC.RES, "readme")
openxlsx::addWorksheet(R_OUT_ESC.RES, "Esc biodata w RESULTS")
openxlsx::addWorksheet(R_OUT_ESC.RES, "Esc biodat w RES - PBT parents")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC summary")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc0 - EBwR unCert Oto")
openxlsx::addWorksheet(R_OUT_ESC.RES, "!NPAFC_dupl!")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Oto stock ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Oto result")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No CWT ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Reslvd ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- Unreslvd ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- Unreslvd age")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - PADS unmatched")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - OM unmatched")

# Write data to the sheets --------------------
openxlsx::writeData(R_OUT_ESC.RES, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_ESC.RES, sheet="Esc biodata w RESULTS", x=esc_biodata_w_RESULTS)
openxlsx::writeData(R_OUT_ESC.RES, sheet="Esc biodat w RES - PBT parents", x=PBT_parents)
openxlsx::writeData(R_OUT_ESC.RES, sheet="QC summary", x=qc_summary)
openxlsx::writeData(R_OUT_ESC.RES, sheet="qc0 - EBwR unCert Oto", x=qc_EBwR_uncertOtoID)
openxlsx::writeData(R_OUT_ESC.RES, sheet="!NPAFC_dupl!", x=NPAFC_dupl.df)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Oto stock ID", x=qc_noOtoID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Oto result", x=qc_noOtoResults)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No CWT ID", x=qc_noCWTID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Reslvd ID", x=qc_noRslvdID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- Unreslvd ID", x=qc_unRslvdID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- Unreslvd age", x=qc_unRslvdAge)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - PADS unmatched", x=antijoin_PADS)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - OM unmatched", x=antijoin_OM)





# ==================== EXPORT ====================

# To github repo --------------------
openxlsx::saveWorkbook(R_OUT_ESC.RES,
                       file=paste0(here::here("outputs"),
                                   "/R_OUT - WCVI_Escapement-FSC_BioData_",
                                   min(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "-",
                                   max(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "_WithResults_",
                                   Sys.Date(),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To SharePoint working Term Run folder -------------------- 
# openxlsx::saveWorkbook(R_OUT_ESC.RES, 
#                        file=paste0("C:/Users/", 
#                                    Sys.info()[6], 
#                                    "/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2023/Communal data/Escapement/R_OUT - WCVI_Escapement-FSC_BioData_",
#                                    min(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
#                                    "-",
#                                    max(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
#                                    "_WithResults.xlsx"),
#                        overwrite=T,
#                        returnValue=T)



# To DFO Network drive --------------------
# For run reconstruction: 
openxlsx::saveWorkbook(R_OUT_ESC.RES, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/",  
                                   "R_OUT - WCVI_Escapement-FSC_BioData_",
                                   min(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "-",
                                   max(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "_WithResults_",
                                   Sys.Date(),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To biodata folder:
openxlsx::saveWorkbook(R_OUT_ESC.RES, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/SC_BioData_Management/2-Escapement/",  
                                   "R_OUT - WCVI_Escapement-FSC_BioData_",
                                   min(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "-",
                                   max(as.numeric(esc_biodata_w_RESULTS$`(R) SAMPLE YEAR`)),
                                   "_WithResults_",
                                   Sys.Date(),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)



# /END!







