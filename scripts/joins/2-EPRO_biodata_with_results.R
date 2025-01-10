
# Join EPRO to NPAFC mark master file to assign stock IDs to otolith hatch codes (missing step in EPRO as of Sept 2023)

# Work flow is:
# 1.1. Download all facility files 'All Adult Biosampling' reports from EPRO: https://epro-stage.azure.cloud.dfo-mpo.gc.ca/EProWeb/#home
#       1.2. Store EPRO files on Network drive location
# 2.   Load EPRO files into R from Network drive (Step I) 
# 3.   Load NPAFC mark master file from SCD_Stad network drive (Step II): dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks....xlsx
# 4.   Join EPRO to NPAFC mark master file (Step III)
# 5.   Load CWT release tag codes from last 10 years (Step IV)
# 6.   Join EPRO+NPAFC file to CWT tag codes (Step V)
# 7.   Assign final stock ID (Step VI)
# 5.   Run QC report(s) (Step VII)
# 6.   Export to git and Sharepoint for subsequent use in run reconstructions (Step VIII)

################################################################################################################################################
################################################################################################################################################

# BEFORE YOU START: 
# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.



################################################################################################################################################
################################################################################################################################################

# Now should be able to highlight and run all! 


# Load packages ----------------
library(tidyverse)


# Helpers -------------
"%notin%" <- Negate("%in%")
analysis_year <- 2024




################################################################################################################################################

#                                                                           I. COMBINE EPRO FACILITY FILES 


# Load source() EPRO compile code ---------------------------
source(here::here("scripts", "misc-helpers", "EPROcompile.R"))




################################################################################################################################################

#                                                                           II. NPAFC LOAD


NPAFC <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All CN Marks",   #ignore temp files, eg "~All CN Marks...,
                                            full.names = TRUE), 
                            sheet="AC087805 (1)") %>% 
  setNames(paste0('NPAFC_', names(.))) %>% 
  rename(`(R) HATCHCODE` = NPAFC_HATCH_CODE,
         `(R) RESOLVED BROOD YEAR` = NPAFC_BROOD_YEAR) %>% 
  mutate(NPAFC_FACILITY = case_when(is.na(NPAFC_FACILITY) ~ NPAFC_AGENCY,
                                    TRUE ~ NPAFC_FACILITY),
         NPAFC_STOCK = case_when(is.na(NPAFC_STOCK) ~ NPAFC_FACILITY,
                                 TRUE ~ NPAFC_STOCK),
         `(R) BYHID` = case_when(!is.na( `(R) RESOLVED BROOD YEAR`) & !is.na(`(R) HATCHCODE`) ~ paste0( `(R) RESOLVED BROOD YEAR`, " - ", `(R) HATCHCODE`),
                                 TRUE ~ NA)) %>%
  # This filter line below initially removed some marks to avoid duplicates, but the work-around below now addresses this more systematically. Below is just
  #     kept for reference for now
  #filter(NPAFC_STATE_PROVINCE %in% c("BRITISH COLUMBIA", "IDAHO", "WASHINGTON", "OREGON"),
  # 2. Remove the one case where RCH and Nanaimo Hatchery used the same mark in 2018 and assume it was a RCH fish
  #!grepl("NANAIMO", NPAFC_FACILITY) | `(R) BROOD YEAR`!=2018 | `(R) HATCHCODE`!="H5"
  #) %>%
  select( `(R) RESOLVED BROOD YEAR`, NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_REGION, `(R) BYHID`, NPAFC_NUMBER_RELEASED) %>% 
  distinct( `(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_FACILITY, NPAFC_STOCK, .keep_all=T) %>% 
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
  group_by( `(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`) %>% 
  pivot_wider(names_from = group,
              values_from= c(NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, NPAFC_STATE_PROVINCE, NPAFC_REGION, NPAFC_wcvi_prob, NPAFC_NUMBER_RELEASED)) %>%
  select( `(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  print()



#############################################################################################################################################################

#                                                                           III. JOIN EPRO + NPAFC


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(wcviEPRO), colnames(NPAFC))

wcviCNepro_w_NPAFC <- left_join(wcviEPRO %>%
                                  filter(Species=="Chinook"),
                                    NPAFC,
                                    by=c("(R) RESOLVED BROOD YEAR", "(R) HATCHCODE"),
                                    relationship="many-to-one")



#############################################################################################################################################################

#                                                                           IV. CWT LOAD


# Option 1: Load function to query MRPIS CWT releases --------------------------- 
  # Run this if it hasn't been refreshed in a while (**VERY SLOW**)
# source(here("scripts","functions","pullChinookCWTReleases.R"))
# saves as CN_relTagCodes


# Option 2: Load CWT data export from source() above directly --------------------------- (much quicker)
CN_relTagCodes <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/",
                                                     pattern = "^R_OUT - Chinook CWT release tagcodes",    
                                                     full.names = T), 
                                     sheet="Sheet1") %>% 
  print()



# If you get an error message about "atomic vectors"  
  # try restarting R and/or your computer. It's can be related to connectivity access and just means R isn't talking to the DFO network properly

# If you get a message about: Error in openSaaWebConnection(extractor_usage_url, user_name, password) : Error when setting up connection to web server: 401
  # you may not have the config file in your working directory, or the json query doc name is wrong. 



#############################################################################################################################################################

#                                                                           V. JOIN EPRO+NPAFC + CWT


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(wcviCNepro_w_NPAFC), colnames(CN_relTagCodes))

wcviCNepro_w_NPAFC.MRP <- left_join(wcviCNepro_w_NPAFC ,
                                    CN_relTagCodes,
                                    by="(R) TAGCODE",
                                    relationship="many-to-one")


#############################################################################################################################################################

#                                                                           VI. LOAD PBT DATA

# ======================== Load PBT results ========================  
SC_PBT_SEP <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/2023-09-14 Chinook_Brood_2013-2021_PBT_results.xlsx",
                                 sheet="Sheet1", guess_max=10000) %>% 
  setNames(paste0('MGL_', names(.))) %>% 
  mutate(`(R) DNA NUM` = MGL_oFish,
         `(R) RETURN YEAR` = MGL_oYear,
         MGL_Brood_Collection = str_to_title(gsub("_", " ", MGL_Brood_Collection, ignore.case = T))) %>% 
  mutate_at(c("(R) DNA NUM", "(R) RETURN YEAR"), as.character) %>%
  filter(grepl("Bedwell|Burman|Conuma|Cowichan|Cypre|Gold|Kennedy|Leiner|Campbell|Qualicum|Marble|Nahmint|Nanaimo|Nimpkish|Nitinat|
               Oyster|Phillips|Puntledge|Quinsam|Robertson|Salmon River Jnst|San Juan|Sarita|Sooke|Tahsis|Thornton|Toquart|Tranquil", 
               MGL_Brood_Collection, ignore.case=T)) %>%
  print()


# ======================== Load PBT inventory ========================  
# Run PBT source code -------------------------   
source(here("scripts", "misc-helpers", "CalcReliablePBT.R"))
# saves as SC_PBTreliable



#############################################################################################################################################################

#                                                                           VII. JOIN JOIN EPRO+NPAFC+CWT to PBT


# Join EPRO master file to PBT ---------------------------
intersect(colnames(wcviCNepro_w_NPAFC.MRP), colnames(SC_PBT_SEP))

wcviCNepro_w_NPAFC.MRP.PBT <- left_join(wcviCNepro_w_NPAFC.MRP %>%
                                          mutate_at("(R) DNA NUM", as.character) %>% 
                                          mutate_at("(R) RETURN YEAR", as.character),
                                        SC_PBT_SEP ,
                                    by=c("(R) DNA NUM", "(R) RETURN YEAR")) %>%
  mutate(`(R) TOTAL AGE: PBT` = MGL_Offspring_Age) %>%
  print()



#############################################################################################################################################################

#                                                                           VII. ASSIGN FINAL STOCK ID and ORIGIN



wcviCNepro_w_Results <- wcviCNepro_w_NPAFC.MRP.PBT %>%
  mutate(
         # AGE ID: 
         `(R) RESOLVED TOTAL AGE METHOD` = case_when(!is.na(`(R) TOTAL AGE: CWT`) ~ "CWT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & !is.na(`(R) TOTAL AGE: PBT`) ~ "PBT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & is.na(`(R) TOTAL AGE: PBT`) & !is.na(`(R) TOTAL AGE: SCALE`) ~ "Scale",
                                                     TRUE ~ NA),
         `(R) RESOLVED TOTAL AGE` = case_when(`(R) RESOLVED TOTAL AGE METHOD`=="CWT" ~ `(R) TOTAL AGE: CWT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="PBT" ~ `(R) TOTAL AGE: PBT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="Scale" ~ `(R) TOTAL AGE: SCALE`,
                                              TRUE ~ NA),
         
         `(R) RESOLVED FINAL BROOD YEAR` = as.numeric(`(R) RETURN YEAR`) - `(R) RESOLVED TOTAL AGE`,
    
    
         # 1. Identify hatchery/natural origin
         `(R) ORIGIN` = case_when((Otolith.Hatch.Code %in% c("Destroyed", "No Sample") | is.na(Otolith.Hatch.Code)) & (is.na(Cwt.Tag.Code) | Cwt.Tag.Code =="No tag") & (External.Marks=="Unclipped") ~ "Unknown",
                                  External.Marks=="Clipped" ~ "Hatchery",
                                  !is.na(Cwt.Tag.Code) | Cwt.Tag.Code != "No tag" ~ "Hatchery",
                                  Otolith.Hatch.Code %notin% c("Destroyed", "No Sample", "Not Marked") ~ "Hatchery", 
                                  !is.na(MGL_Parental_Collection) ~ "Hatchery", 
                                  Otolith.Hatch.Code == "Not Marked" ~ "Natural",
                                  # If the system name is present in the Reliable PBT records, and the return year is >= the first full PBT baseline year for that system, then call it natural
                                  # ******* THIS ISNT WORKING  -- FIX NEXT DAY! 
                                  str_sub(gsub(" Cr", "",
                                                       gsub(" R", "",
                                                            gsub(" Fall Chinook", "", Spawning.Stock.Name)
                                                            )
                                                       ),
                                                  start=6, end=20) %in% SC_PBTreliable$MGL_Brood_Collection & 
                                     `(R) RETURN YEAR` >= SC_PBTreliable$firstFullBY & 
                                    is.na(MGL_Parental_Collection) ~ "Natural (PBT flag)",
                                  TRUE ~ "Unknown"),
         
         
         
         # 2. Identify CWT Stock ID 
         `(R) CWT STOCK ID` = case_when(!is.na(`MRP_Stock Site Name`) ~ 
                                          gsub(" Cr", "", 
                                               gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
                                               ignore.case=F),
                                        TRUE ~ NA),
        
    
        # 3. Identify PBT Stock ID
        `(R) PBT STOCK ID` = case_when(!is.na(MGL_Parental_Collection) ~ str_to_title(gsub(" Creek", "",
                                                                                           gsub(" River", "",
                                                                                                gsub("_", 
                                                                                                     " ", 
                                                                                                     MGL_Parental_Collection, 
                                                                                                     ignore.case = T),
                                                                                                ignore.case=T),
                                                                                           ignore.case=T)),
                                       TRUE ~ NA),
         
         
         # 4. Before identifying otolith ID, determine the certainty of the ID (accounts for any duplication of hatchcodes within a BY)
         `(R) OTOLITH ID METHOD` = case_when(!is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ "To stock (certain)",
                                             !is.na(NPAFC_STOCK_1) & !is.na(NPAFC_STOCK_2) ~ 
                                               "Duplicate BY-hatchcode at >1 facility, assumed stock ID (moderately certain ID)",
                                             #is.na(NPAFC_STOCK_1) & !is.na(OM_FACILITY) ~ "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)",    # NOT RELEVANT FOR EPRO output 
                                             TRUE~NA),
         
         
         # 5. Identify otolit stock ID - Note at this point it is irrelevant if a CWT exists because we want to test later whether CWT ID and Otolith ID agree
         `(R) OTOLITH STOCK ID` = case_when(
           # 5 a) NSingle otolith stock choice (certain ID)
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
             
             # 5 e) Multiple otolith matches but Stock1 is med-high probability and the rest are not, choose Otolith stock 1: 
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
           #//end 5. 
           TRUE ~ NA)) %>% 
  mutate(
    # 6. Identify the method used to determine the final stock ID: CWT > PBT > Otolith
    `(R) RESOLVED STOCK ID METHOD` = case_when(!is.na(`(R) CWT STOCK ID`) ~ "CWT",
                                               is.na(`(R) CWT STOCK ID`) & !is.na(`(R) PBT STOCK ID`) ~ "PBT",
                                               is.na(`(R) CWT STOCK ID`) & is.na(`(R) PBT STOCK ID`) & !is.na(`(R) OTOLITH ID METHOD`) ~ paste0("Otolith", sep=" - ", `(R) OTOLITH ID METHOD`),
                                               TRUE ~ NA),
    
    # 7. Assign the final stock ID: CWT > PBT > Otolith (with varying levels of oto certainty)
    `(R) RESOLVED STOCK ID` = case_when(!is.na(`(R) CWT STOCK ID`) ~ `(R) CWT STOCK ID`,
                                        is.na(`(R) CWT STOCK ID`) & !is.na(`(R) PBT STOCK ID`) ~ `(R) PBT STOCK ID`,
                                        is.na(`(R) CWT STOCK ID`) & is.na(`(R) PBT STOCK ID`) & !is.na(`(R) OTOLITH STOCK ID`) ~ `(R) OTOLITH STOCK ID`,
                                        #is.na(`(R) CWT STOCK ID`) & is.na(`(R) OTOLITH STOCK ID`) & !is.na(`(R) OTOLITH FACILITY ID`) ~ `(R) OTOLITH FACILITY ID`,            # irrelevant for EPRO output
                                        `(R) ORIGIN`=="Natural" ~ paste0(stringr::str_to_title(str_sub(gsub(pattern=" R Fall Chinook", replacement="", Spawning.Stock.Name), 6, -1)), 
                                                                         " (assumed)"), 
                                        TRUE ~ "Unknown"),
    
    # 8. Combine the origin and ID into the final grouping level for the Term Run files 
    `(R) RESOLVED STOCK-ORIGIN` = paste0(`(R) ORIGIN`, sep=" ", `(R) RESOLVED STOCK ID`),
    
    # 9. Create flag for cases where PBT, CWT and/or Otolith ID(s) disagree
    `(R) STOCK ID FLAG: CWT-OTO` = case_when(`(R) CWT STOCK ID` != `(R) OTOLITH STOCK ID` ~ "FLAG: CWT/Otolith stock ID disagree",
                                             TRUE ~ NA),
    `(R) STOCK ID FLAG: CWT-PBT` = case_when(`(R) CWT STOCK ID` != `(R) PBT STOCK ID` ~ "FLAG: CWT/PBT stock ID disagree",
                                             TRUE ~ NA),
    `(R) STOCK ID FLAG: PBT-OTO` = case_when(`(R) OTOLITH STOCK ID` != `(R) PBT STOCK ID` ~ "FLAG: PBT/Otolith stock ID disagree",
                                             TRUE ~ NA),
    
    
    # 10. Create flag for cases where PBT, CWT and/or scale age(s) disagree
    `(R) AGE FLAG: CWT-SCALE` = case_when(`(R) TOTAL AGE: CWT` != `(R) TOTAL AGE: SCALE` ~ "FLAG: CWT/scale ages disagree",
                                             TRUE ~ NA),
    `(R) AGE FLAG: CWT-PBT` = case_when(`(R) TOTAL AGE: CWT` != `(R) TOTAL AGE: PBT` ~ "FLAG: CWT/PBT ages disagree",
                                             TRUE ~ NA),
    `(R) AGE FLAG: PBT-SCALE` = case_when(`(R) TOTAL AGE: SCALE` != `(R) TOTAL AGE: PBT` ~ "FLAG: PBT/scale ages disagree",
                                             TRUE ~ NA)
    ) %>% 
  print()



#############################################################################################################################################################

#                                                                           VIII. QC and readme

# ======================== Extract PBT parental records ========================  
  # --> Can't do this yet as EPRO file doesn't go back far enough (EPRO records only back to ~2021 at best)
# PBT_parents <- esc_biodata_PADS_otoNPAFC_headsCWT %>% 
#   filter(`(R) DNA NUM` %in% c(SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_mFish),]$MGL_mFish, 
#                               SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_dFish),]$MGL_dFish))


# QC flags ---------------------------
# There is brood year data and a useable hatch code but the Otolith stock ID didn't populate (e.g., R code errors)
qc_noOtoID <- wcviCNepro_w_Results %>%
  filter(!is.na(`(R) RESOLVED BROOD YEAR`) & !is.na(`(R) HATCHCODE`) & `(R) HATCHCODE` %notin% c("Destroyed", "Not Marked", "No Sample") & is.na(NPAFC_STOCK_1)) %>%
  print()

# There is an otolith sample that was taken and a useable brood year, but no otolith result (e.g., sample processing errors)
qc_noOtoResults <- wcviCNepro_w_Results %>%
  filter(!is.na(`(R) OTOLITH BOX-VIAL CONCAT`) & !is.na(`(R) RESOLVED BROOD YEAR`) & is.na(`(R) HATCHCODE`)) %>%
  print()

# There is a useable CWT but no stock ID (e.g., R code errors)
qc_noCWTID <- wcviCNepro_w_Results %>% 
  filter(!is.na(Cwt.Tag.Code) & Cwt.Tag.Code%notin%c("No Tag","Lost Tag","No Head") & Sample.Status=="Tag Read Ok" & is.na(`MRP_Stock Site Name`)) %>% 
  filter()

# Stock ID is unknown but there is a useable otolith and/or CWT stock ID available (e.g., code errors)
qc_noRslvdID <- wcviCNepro_w_Results %>% 
  filter(`(R) RESOLVED STOCK ID`=="Unknown" & !is.na(NPAFC_STOCK_1) & !is.na(`MRP_Stock Site Name`)) %>% 
  print()

# Stock ID(s) disagree (e.g., processing error)
qc_unRslvdID <- wcviCNepro_w_Results %>% 
  filter(across(c(`(R) STOCK ID FLAG: CWT-OTO`:`(R) STOCK ID FLAG: PBT-OTO`), ~ grepl("FLAG", .x))) %>%
  print()

# Scale, CWT and/or PBT age(s) disagree
qc_unRslvdAge <- wcviCNepro_w_Results %>% 
  filter(across(c(`(R) AGE FLAG: CWT-SCALE`:`(R) AGE FLAG: PBT-SCALE`), ~ grepl("FLAG", .x))) %>%
  print()


# QC summary Report ---------------------------
qc_summary <- data.frame(qc_flagName = c("QC- No Oto ID",
                                         "QC- Oto sample no result",
                                         "QC- No CWT ID",
                                         "QC- No resolved ID",
                                         "QC- Stock IDs disagree",
                                         "QC- Ages disagree",
                                         "",
                                         "total EPRO records:"),
                         number_records = c(nrow(qc_noOtoID),
                                            nrow(qc_noOtoResults),
                                            nrow(qc_noCWTID),
                                            nrow(qc_noRslvdID),
                                            nrow(qc_unRslvdID),
                                            nrow(qc_unRslvdAge),
                                            "",
                                            nrow(wcviCNepro_w_Results)),
                         description = c("Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
                                         "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
                                         "There is CWT info available but no Stock ID. Note sometimes this is due to exceptionally young age classes (eg, Jimmies) being sampled, or more rarely species ID issues (e.g., tag code 186168). Use https://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/TagSearch to search individual tag #s if concerned about results.",
                                         "There is a CWT or an NPAFC ID but no Resolved stock ID.",
                                         "CWT, PBT and/or Otolith stock ID(s) disagree.",
                                         "CWT, PBT and/or scale age(s) disagree.",
                                         "",
                                         paste0("for ", paste(unique(wcviCNepro_w_Results$`(R) RETURN YEAR`), collapse = " ") ))) %>% 
  print()



# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source EPRO files:",
                             "source NPAFC file:",
                             "CWT tag code source:",
                             "assumptions made/notes:", 
                             "",
                             "TAB NAME",
                             "AllFacilities w RESULTS",
                             "QC Report",
                             "QC-..."
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/2-EPRO_biodata_with_results.R , https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/EPROcompile.R", 
        "SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RETURN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R (saved from online EPRO output)",
        "SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to {MOST RECENT DATE}.xlsx",
        "http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice", 
        "2021 CURRENTLY INCOMPLETE - EPRO STILL UPDATING HISTORICAL YEARS",
        "",
        "TAB DESCRIPTION",
        "All EPRO facilities 2022 'All Adult Biosampling' reports for WCVI combined into 1 file and joined to 1. the NPAFC mark file to give otolith stock ID and 2. CWT releases for last 10 years to give CWT stock ID.",
        "Summary of QC flags, # of entries belonging to that flag and descriptions.",
        "QC flag tabs. See QC summary report for details."))


#############################################################################################################################################################

#                                                                           X. EXPORT 


# ================== Create excel file ==================

# Create empty workbook ---------------------------
R_OUT_EPRO.NPAFC <- openxlsx::createWorkbook()

# Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "readme")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "AllFacilities w RESULTS")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC summary")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No Oto stock ID")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Oto sample no result")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No CWT ID")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No Resolved ID")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Stock IDs disagree")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Ages disagree")

# Write data to tabs ---------------------------
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="AllFacilities w RESULTS", x=wcviCNepro_w_Results)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="QC summary", x=qc_summary)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No Oto stock ID", x=qc_noOtoID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Oto sample no result", x=qc_noOtoResults)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No CWT ID", x=qc_noCWTID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No Resolved ID", x=qc_noRslvdID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Stock IDs disagree", x=qc_unRslvdID)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Ages disagree", x=qc_unRslvdAge)



# ================== Export ================== 
# To github repo ---------------------------
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0(here("outputs"), 
                                   "/R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS ",
                                   min(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   "-",
                                   max(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To SharePoint ---------------------------
  # Discontinue sharepoint export, too confusing with different syncing to different local computers - commit to git and DFO network 
    # openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
    #                        file=paste0(epro_dir, 
    #                                    "/R_OUT - All EPRO facilities master WITH RESULTS ",
    #                                    analysis_year,
    #                                    ".xlsx"),
    #                        overwrite=T,
    #                        returnValue=T)

# To DFO Network drive ---------------------------
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
                                   "/R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS ",
                                   min(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   "-",
                                   max(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)








# /END!





