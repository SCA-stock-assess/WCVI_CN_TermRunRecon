
# Join EPRO to NPAFC mark master file to assign stock IDs to otolith hatch codes (missing step in EPRO as of Sept 2023)

# Work flow is:
# 1.1. Download all WCVI facility files 'All Adult Biosampling' reports from EPRO: https://epro-stage.azure.cloud.dfo-mpo.gc.ca/EProWeb/#home
#       1.2. Store EPRO files on Network drive location: "Y:\WCVI\CHINOOK\WCVI_TERMINAL_RUN\Annual_data_summaries_for_RunRecons\EPROcompile_base-files\1-Import-to-R"
# 2.   Load EPRO files into R from Network drive  
# 3.   Load NPAFC mark master file from SCD_Stad network drive: SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/
# 4.   Join EPRO to NPAFC mark master file  
# 5.   Load CWT release tag codes from last 10 years (direct saaWeb query, or load of previously dumped data from "Y:\WCVI\CHINOOK\WCVI_TERMINAL_RUN\Annual_data_summaries_for_RunRecons\R_OUT - Chinook CWT release tagcodes BY 2004-2024.xlsx")
# 6.   Join EPRO+NPAFC file to CWT tag codes  
# 7.   Assign final stock/origin/age 
# 5.   Run QC report(s)  - removed for now
# 6.   Export to git and network for subsequent use in run reconstructions 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== BEFORE YOU START: ====================
## Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects from Environment including hidden objects.
gc()                            # free up memory and report the memory usage.

# ***** MANUAL UPATE: 
analysis_year <- 2025


## Load packages & helpers ----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== COMBINE EPRO FACILITY FILES =====================

## Load source() EPRO compile code ---------------------------
# This code references a background script, "EPROcompile.R" which essentially just reads in the individual
# All Adult Biosampling files for each Facility, renames some columns, and compiles them into 1 flat file for 
# subsequent manipulation across facilities and years. 
# The EPROcompile.R script should not require any updates. 

source(here::here("scripts", "misc-helpers", "EPROcompile.R"))
# --> saves in Environment as wcviEPRO. Note that it is filtered down to just Chinook at a later stage. 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== NPAFC LOAD =====================

# We have to load our marks file because EPRO only joins results to hatch code, not to stock ID, which is not super helpful

# *** This list.files() part can be tricky - it will always take the top file "[1]" which was necessary to code in because sometimes
# multiple files are added with new dates. You may need to update the [1] if it isn't the right file in future
NPAFC <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All Canadian Marks in NPAFC Database",    
                                            full.names = TRUE)[1]) %>% 
  filter(SPECIES=="CHINOOK") %>%
  setNames(paste0('NPAFC_', names(.))) %>% 
  rename(`(R) HATCHCODE` = NPAFC_HATCH_CODE,
         `(R) RESOLVED BROOD YEAR` = NPAFC_BROOD_YEAR) %>% 
  mutate(NPAFC_FACILITY = case_when(is.na(NPAFC_FACILITY) ~ NPAFC_AGENCY,
                                    TRUE ~ NPAFC_FACILITY),
         NPAFC_STOCK = case_when(is.na(NPAFC_STOCK) ~ NPAFC_FACILITY,
                                 TRUE ~ NPAFC_STOCK),
         `(R) BYHID` = case_when(!is.na( `(R) RESOLVED BROOD YEAR`) & !is.na(`(R) HATCHCODE`) ~ paste0( `(R) RESOLVED BROOD YEAR`, " - ", `(R) HATCHCODE`),
                                 TRUE ~ NA)) %>%
  select( `(R) RESOLVED BROOD YEAR`, NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_REGION, `(R) BYHID`, NPAFC_NUMBER_RELEASED) %>% 
  distinct( `(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, NPAFC_STATE_PROVINCE, NPAFC_FACILITY, NPAFC_STOCK, .keep_all=T) %>% 
  # This section below is for the few cases where there may be read issues or duplicate marks. It basically assigns the likelihood of a given mark to being from a broad region. This came up for example in the San Juan where we had a bunch of Russian fish that PBT later confirmed were SJ (poor mark application)
  mutate(NPAFC_wcvi_prob = case_when(NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("NWVI","SWVI") ~ "A",
                                     NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("LWFR","TOMM", "TOMF") ~ "B",
                                     NPAFC_STATE_PROVINCE%in%c("IDAHO","OREGON","WASHINGTON") ~ "B",
                                     NPAFC_STATE_PROVINCE=="BRITISH COLUMBIA" & NPAFC_REGION%in%c("GSVI","JNST") ~ "C",
                                     NPAFC_STATE_PROVINCE=="ALASKA" ~ "D",
                                     NPAFC_STATE_PROVINCE=="KAMCHATKA" ~ "E")) %>%
  arrange(`(R) BYHID`, NPAFC_wcvi_prob) %>%
  mutate(group = case_when(!is.na(`(R) BYHID`) ~ with(., ave(seq_along(`(R) BYHID`), `(R) BYHID`, FUN = seq_along)),
                           TRUE ~ 1),
         NPAFC_wcvi_prob = case_when(NPAFC_wcvi_prob=="A" ~ "HIGH",
                                     NPAFC_wcvi_prob=="B" ~ "MED-HIGH",
                                     NPAFC_wcvi_prob=="C" ~ "MED",
                                     NPAFC_wcvi_prob=="D" ~ "LOW",
                                     NPAFC_wcvi_prob=="E" ~ "V LOW")) %>%
  group_by(`(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`) %>% 
  pivot_wider(names_from = group,
              values_from= c(NPAFC_FACILITY, NPAFC_RELEASE_YEAR, NPAFC_STOCK, NPAFC_STATE_PROVINCE, NPAFC_REGION, NPAFC_wcvi_prob, NPAFC_NUMBER_RELEASED)) %>%
  select( `(R) RESOLVED BROOD YEAR`, `(R) HATCHCODE`, `(R) BYHID`, contains("1"), contains("2"), contains("3"), contains("4")) %>% 
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== JOIN EPRO + NPAFC =====================
# This step joins the NPAFC mark records to the EPRO file based on hatchcode and BY

## Join wcviEPRO to NPAFC master mark file ---------------------------
wcviCNepro_w_NPAFC <- left_join(wcviEPRO %>%
                                  filter(Species=="Chinook"),
                                    NPAFC,
                                    by=c("(R) RESOLVED BROOD YEAR", "(R) HATCHCODE"),
                                    relationship="many-to-one",
                                na_matches = "never")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== CWT LOAD =====================
# Like thermal marks, EPRO only joins results to tag codes, not stock IDs. So again, we need to be able to link tag codes to stock IDs.

## Option 1: Load function to query MRPIS CWT releases --------------------------- 
# Run this if it hasn't been refreshed in a while - maybe do once/yr (**VERY SLOW**)
  # source(here::here("scripts","functions","pullChinookCWTReleases.R"))
# Note: This relies on the saaWeb package and a saaWeb.config file. the config file should be in the repository and work fine, but if you run into errors Nick Komick is the person to ask. 
# --> saves as CN_relTagCodes in Environment, and also exports an excel copy to the network drive


## Option 2: Load CWT data export from source() above directly --------------------------- 
# If you have already run the source() line above for the year, just load the already dumped Excel version from the network drive (much quicker)
CN_relTagCodes <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/",
                                                     pattern = "^R_OUT - Chinook CWT release tagcodes",    
                                                     full.names = T), 
                                     sheet="Sheet1") %>% 
  print()


# If you get an error message about "atomic vectors"  
  # --> try restarting R and/or your computer. It's can be related to connectivity access and just means R isn't talking to the DFO network properly

# If you get a message about: Error in openSaaWebConnection(extractor_usage_url, user_name, password) : Error when setting up connection to web server: 401
  # --> you may not have the config file in your working directory, or the json query doc name is wrong. 

# If you get an error message about std::bad_alloc() ever
  # --> it means you don't have enough memory. Try closing a lot of windows, and clicking the donut memory icon up by your Global Environment to free up unused memory


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== JOIN EPRO+NPAFC + CWT ===================== 
# This step joins the EPRO file (which now includes thermal mark IDs) to the CWT stock IDs.


## Join EPRO master file to NPAFC master mark file ---------------------------
wcviCNepro_w_NPAFC.MRP <- left_join(wcviCNepro_w_NPAFC ,
                                    CN_relTagCodes,
                                    by="(R) TAGCODE",
                                    relationship="many-to-one")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== LOAD GSI DATA (OPPORTUNISTIC) =====================
# This is a manual stage if you know you have escapement GSI results that aren't in EPRO (EPRO can't handle GSI results right now).
# This was created manually in 2024 for San Juan deadpitch GSI specifically. Until the workflow of getting escapement biodata into CREST is fully formed, this is a bit of a manual step unfortunately.


## Load GSI data ---------------------------
sj.dpitch <- left_join(readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/2024/Chinook/San Juan Deadpitch/PID20240136(1)_San_Juan_DP(24)_sc640_2025-02-27_NF.xlsx",
                                           sheet="repunits_table_ids"),
                        readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/2024/Chinook/San Juan Deadpitch/PID20240136(1)_San_Juan_DP(24)_sc640_2025-02-27_NF.xlsx",
                                           sheet="extraction_sheet") %>%
                          select(indiv, Fish)) %>%
  mutate(across(everything(), as.character)) %>%
  rbind(left_join(readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/2025/Chinook/San Juan/PID20250144(1)_SanJuan_DP(25)_b2_sc746_2026-02-09_NF.xlsx",
                                     sheet="repunits_table_ids"),
                  readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/2025/Chinook/San Juan/PID20250144(1)_SanJuan_DP(25)_b2_sc746_2026-02-09_NF.xlsx",
                                     sheet="extraction_sheet") %>%
                    select(indiv, Fish)))



## Join EPRO master file to GSI file ---------------------------
# Again, join to ever-growing WCVI EPRO file

wcviCNepro_w_NPAFC.MRP.GSI <- left_join(wcviCNepro_w_NPAFC.MRP,
                                        sj.dpitch %>% 
                                          mutate(across(c(Fish), as.numeric)),
                                        by=c("(R) DNA NUM" = "Fish")) %>% 
  mutate(across(PBT_brood_year, as.numeric)) %>%
  mutate(`(R) BROOD YEAR: PBT` = case_when(!is.na(PBT_brood_year) ~ PBT_brood_year,
                                           TRUE ~ `(R) BROOD YEAR: PBT`),
         `(R) TOTAL AGE: PBT` = case_when(!is.na(`(R) BROOD YEAR: PBT`) ~ `(R) RETURN YEAR`-`(R) BROOD YEAR: PBT`,
                                          TRUE ~ `(R) TOTAL AGE: PBT`))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ===================== ASSIGN FINAL STOCK ID and ORIGIN =====================
# This is where the final rollups/stock/age IDs required for the run reconstruction happen


wcviCNepro_w_Results <- wcviCNepro_w_NPAFC.MRP.GSI %>%
  mutate(Otolith.Hatch.Code = case_when(grepl("no mark|not marked|NM|nomk", Otolith.Hatch.Code, ignore.case=T) ~ "NO MARK",
                                           Otolith.Hatch.Code==0 ~ "Not read",
                                           is.na(Otolith.Bag.No) ~ "No sample",
                                           TRUE ~ Otolith.Hatch.Code)) %>%
  mutate(
         # ----- AGE DECISIONS: 
         `(R) RESOLVED TOTAL AGE METHOD` = case_when(!is.na(`(R) TOTAL AGE: CWT`) ~ "CWT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & !is.na(`(R) TOTAL AGE: PBT`) ~ "PBT",
                                                     is.na(`(R) TOTAL AGE: CWT`) & is.na(`(R) TOTAL AGE: PBT`) & !is.na(`(R) TOTAL AGE: SCALE`) ~ "Scale",
                                                     TRUE ~ NA),
         `(R) RESOLVED TOTAL AGE` = case_when(`(R) RESOLVED TOTAL AGE METHOD`=="CWT" ~ `(R) TOTAL AGE: CWT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="PBT" ~ `(R) TOTAL AGE: PBT`,
                                              `(R) RESOLVED TOTAL AGE METHOD`=="Scale" ~ `(R) TOTAL AGE: SCALE`,
                                              TRUE ~ NA),
         
         `(R) RESOLVED BROOD YEAR` = as.numeric(`(R) RETURN YEAR`) - `(R) RESOLVED TOTAL AGE`,
    
    
         # ----- ORIGIN OPTIONS 
         `(R) ORIGIN: CLIP` = case_when(External.Marks=="Clipped" ~ "Hatchery",
                                        TRUE ~ NA),
         `(R) ORIGIN: CWT` = case_when(!is.na(Cwt.Tag.Code) & !grepl("lost tag|no data|no head|no tag", Cwt.Tag.Code, ignore.case=T) ~ "Hatchery",
                                       TRUE ~ NA),
         `(R) ORIGIN: PBT` = case_when(ID_Source=="Too few loci" ~ NA,
                                       
                                       (!is.na(Sire.Dna.Waterbody.Site.Name) & !grepl("N/A", Sire.Dna.Waterbody.Site.Name)) | (!is.na(Dam.Dna.Waterbody.Site.Name) & !grepl("N/A", Dam.Dna.Waterbody.Site.Name))
                                           ~ "Hatchery",
                                       
                                       ID_Source=="PBT" ~ "Hatchery",
                                       
                                       # PBT hit absent (stock/BY dependent) *****REQUIRES MANUAL ADDITION OF BYS***** - tag rates were updated by SEP/MGL April 2026 to include 1P or 2P. Prior to this believe it was only based on 2P, but have updated BYs to reflect 1P tag rates
                                       # Bedwell: only 2 years with PBT
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2016,2019) & grepl("BEDWELL", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Burman: When expanded to allow 1P more BYs are added. Pre-2019 BYs required >= 95% tag rate for 2P method.
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2014,2019,2021,2022,2023,2024) & grepl("BURMAN", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Conuma: All recent BYs high quality for both 1P and 2P. 
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2020,2021,2022,2023,2024,2025) & grepl("CONUMA", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Cypre: no PBT
                                       
                                       # Gold: 2024 1P method was 93% (close) but not included based on cut-off here of wanting >= 95% PBT rate
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2022,2023,2025) & grepl("GOLD", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Kennedy Lower: no PBT
                                       
                                       # Leiner: BYs 2022 and 2023 <90% PBT'd even by 1P method
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2021,2024) & grepl("LEINER", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Marble: only 1 year PBT'd (all others 0s)
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2022) & grepl("marble", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Nahmint: Many early years with 100% tag rate for 1P, but 2P is relatively low. 2018-2020 very high PBT rate (100% both methods)
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2018,2019,2020,2023,2024,2025) & grepl("NAHMINT", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Nitinat: 1P method made significant improvements
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2021,2022,2023,2024,2025) & grepl("NITINAT", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Robertson: Unclear on 2025 tag rate, need to update
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2013,2015,2019,2020,2021,2023,2024) & grepl("ROBERTSON", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # San Juan: 1P made significant improvements in allowable BYs. 2018 is also close - 93% 2P tag rate.
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2021,2022,2023,2024) & grepl("SAN JUAN", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Sarita: early BYs close but <90%
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2013,2015,2019,2020,2021,2022,2023,2024,2025) & grepl("SARITA", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",

                                       # Sucwoa:  no PBT
                                       
                                       # Tahsis: BY 2025 unclear, may need updating later 
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2021,2022,2023,2024) & grepl("TAHSIS", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Tahsish: no PBT
                                       
                                       # Thornton: BY 2025 may need updating later 
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2023,2024) & grepl("THORNTON", Spawning.Stock.Name, ignore.case=T)) ~ "Natural* (this population should not have natural production)",
                                       
                                       # Tlupana: essentially no PBT

                                       # Toquart: BY 2025 may need updating later
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR` %in% c(2019,2020,2023,2024) & grepl("TOQUART", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       # Tranquil: no PBT
                                       
                                       ID_Source=="GSI" ~ "Natural",
                                       
                                       TRUE ~ NA),
         `(R) ORIGIN: OTOLITH` = case_when(grepl("H|,", Otolith.Hatch.Code) #& !grepl("test|no|not|NS|destroyed|NM|unreadable|0|unmountable", Otolith.Hatch.Code, ignore.case=T) 
                                           ~ "Hatchery", 
                                           Otolith.Hatch.Code == "NO MARK" ~ "Natural",
                                           TRUE ~ NA),
         
         `(R) RESOLVED ORIGIN` = case_when(`(R) ORIGIN: CLIP`=="Hatchery" ~ "Hatchery",
                                           `(R) ORIGIN: CWT`=="Hatchery" ~ "Hatchery",
                                           `(R) ORIGIN: PBT`=="Hatchery" ~ "Hatchery",
                                           `(R) ORIGIN: OTOLITH`=="Hatchery" & `(R) ORIGIN: PBT`=="Natural" ~ "Hatchery, but PBT/Oto disagree",
                                           `(R) ORIGIN: OTOLITH`=="Hatchery" & (`(R) ORIGIN: PBT`=="Hatchery"|is.na(`(R) ORIGIN: PBT`)) ~ "Hatchery",
                                           
                                           `(R) ORIGIN: PBT`=="Natural" ~ "Natural (assumed)",
                                           `(R) ORIGIN: OTOLITH`=="Natural" & `(R) ORIGIN: PBT`=="Hatchery" ~ "Hatchery",
                                           `(R) ORIGIN: OTOLITH`=="Natural" & (`(R) ORIGIN: PBT`=="Natural" | is.na(`(R) ORIGIN: PBT`)) ~ "Natural (assumed)",
                                           TRUE ~ "Unknown"),

         
         `(R) RESOLVED ORIGIN METHOD` = case_when(# Ad clip 
                                         !is.na(`(R) ORIGIN: CLIP`) ~ "Ad clip (presence)",
                                         
                                         # CWT  
                                         !is.na(`(R) ORIGIN: CWT`) ~ "CWT (presence)",
                                         
                                         # PBT  
                                         !is.na(`(R) ORIGIN: PBT`) ~ "PBT (presence/absence)",
                                         
                                         # Otolith mark 
                                         !is.na(`(R) ORIGIN: OTOLITH`) ~ "Otolith (presence/absence)", 
                                         
                                         TRUE ~ NA),
         
         
         # ----- STOCK ID DECISIONS
         # Identify CWT Stock ID 
         `(R) STOCK ID: CWT` = case_when(!is.na(`MRP_Stock Site Name`) ~ 
                                          gsub(" Cr", "", 
                                               gsub(" R", "", `MRP_Stock Site Name`, ignore.case = F), 
                                               ignore.case=F),
                                        TRUE ~ NA),
        
    
        #  Identify PBT Stock ID
        `(R) STOCK ID: PBT` = case_when((!is.na(Dam.Dna.Waterbody.Site.Name) & !grepl("N/A", Dam.Dna.Waterbody.Site.Name)) ~ sub(" [^ ]+$", "", Dam.Dna.Waterbody.Site.Name),
                                       (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & (!is.na(Sire.Dna.Waterbody.Site.Name) & !grepl("N/A", Sire.Dna.Waterbody.Site.Name)) ~ sub(" [^ ]+$", "", Sire.Dna.Waterbody.Site.Name),
                                       ID_Source=="PBT" ~ gsub(stringr::str_to_title(stringr::str_replace_all(PBT_brood_collection, "_", " ")),
                                                               pattern=" River",
                                                               replacement=""),
                                       TRUE ~ NA),
         
         
         # 4. Before identifying otolith ID, determine the certainty of the ID (accounts for any duplication of hatchcodes within a BY)
         `(R) STOCK ID: OTOLITH METHOD` = case_when(!is.na(NPAFC_STOCK_1) & is.na(NPAFC_STOCK_2) ~ "To stock (certain)",
                                             !is.na(NPAFC_STOCK_1) & !is.na(NPAFC_STOCK_2) ~ 
                                               "Duplicate BY-hatchcode at >1 facility, assumed stock ID (moderately certain ID)",
                                             #is.na(NPAFC_STOCK_1) & !is.na(OM_FACILITY) ~ "Issue with BY-hatchcode read/application, identified to facility or assumed stock based on facility (least certain ID)",    # NOT RELEVANT FOR EPRO output 
                                             TRUE ~ NA),
         
         
         # 5. Identify otolith stock ID - Note at this point it is irrelevant if a CWT exists because we want to test later whether CWT ID and Otolith ID agree
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
           (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3)) & 
               (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2=="HIGH" | NPAFC_wcvi_prob_3=="HIGH") ~  
               "!! manual decision needed, refer to release sizes!!",
             
             # 5 c) Multiple MEDIUM probability otolith matches, flag for manual ID: 
           (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3)) & 
               (NPAFC_wcvi_prob_1=="MED" & NPAFC_wcvi_prob_2=="MED" | NPAFC_wcvi_prob_3=="MED") ~  
               "!! manual decision needed, refer to release sizes!!",
             
             # 5 d) Multiple otolith matches but Stock1 is HIGH probability and the rest are NOT, therefore choose Otolith stock 1: 
           (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3)) & 
               (NPAFC_wcvi_prob_1=="HIGH" & NPAFC_wcvi_prob_2!="HIGH" | NPAFC_wcvi_prob_3!="HIGH") ~  
               gsub(" R", "",
                    gsub(" Cr", "",  
                         stringr::str_to_title(
                           stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                         ignore.case = F), 
                    ignore.case=F),
             
             # 5 e) Multiple otolith matches but Stock1 is med-high probability and the rest are not, choose Otolith stock 1: 
           (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3)) & 
               (NPAFC_wcvi_prob_1=="MED-HIGH" & NPAFC_wcvi_prob_2!="MED-HIGH" | NPAFC_wcvi_prob_3!="MED-HIGH") ~  
               gsub(" R", "",
                    gsub(" Cr", "",  
                         stringr::str_to_title(
                           stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                         ignore.case = F), 
                    ignore.case=F),
             
             # 5 f) LOW probability otoliths, just choose stock1:
           (!is.na(NPAFC_STOCK_1) | !is.na(NPAFC_STOCK_2) | !is.na(NPAFC_STOCK_3)) & 
               (NPAFC_wcvi_prob_1%in%c("MED","LOW","V LOW") & NPAFC_wcvi_prob_2%in%c("LOW", "V LOW") | NPAFC_wcvi_prob_3%in%c("LOW", "V LOW") ) ~  
               gsub(" R", "",
                    gsub(" Cr", "",  
                         stringr::str_to_title(
                           stringr::str_sub(NPAFC_STOCK_1,3,100)), 
                         ignore.case = F), 
                    ignore.case=F),
           #//end 5. 
           TRUE ~ NA),
        
        `(R) STOCK ID: GSI` = case_when(ID_Source=="GSI" & associated_collection_prob>0.75 ~ gsub(stringr::str_to_title(stringr::str_replace_all(top_collection, "_", " ")), 
                                                                                                  pattern=" River", 
                                                                                                  replacement=""),
                                        TRUE ~ NA)) %>% 
  mutate(
    # 6. Identify the method used to determine the final stock ID: CWT > PBT > Otolith
    `(R) RESOLVED STOCK ID METHOD` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ "CWT",
                                               is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) ~ "PBT",
                                               !is.na(`(R) STOCK ID: GSI`) ~ "GSI",
                                               is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) 
                                               & !is.na(`(R) STOCK ID: OTOLITH METHOD`) ~ paste0("Otolith", sep=" - ", `(R) STOCK ID: OTOLITH METHOD`),
                                               TRUE ~ NA),
    
    # 7. Assign the final stock ID: CWT > PBT > Otolith (with varying levels of oto certainty)
    `(R) RESOLVED STOCK ID` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ `(R) STOCK ID: CWT`,   #if CWT, take CWT first
                                        is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) & is.na(`(R) STOCK ID: GSI`) ~ `(R) STOCK ID: PBT`,   # if no CWT, take PBT
                                        #is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) & !is.na(`(R) STOCK ID: GSI`)  ~ `(R) STOCK ID: GSI`,   # if no CWT, no PBT take GSI ?
                                        is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) #& is.na(`(R) STOCK ID: GSI`) 
                                          & !is.na(`(R) STOCK ID: OTOLITH`) ~ `(R) STOCK ID: OTOLITH`,                                                  # if no cwt, no pbt, take otolith
                                        is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) & is.na(`(R) STOCK ID: OTOLITH`) & !is.na(`(R) STOCK ID: GSI`) ~ `(R) STOCK ID: GSI`,
                                        is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) & is.na(`(R) STOCK ID: OTOLITH`) & is.na(`(R) STOCK ID: GSI`) & grepl("Natural", `(R) RESOLVED ORIGIN`, ignore.case=T) ~ paste0(stringr::str_to_title(str_sub(gsub(pattern=" R Fall Chinook", replacement="", Spawning.Stock.Name), 6, -1)), 
                                                                         " (assumed)"), 
                                        TRUE ~ "Unknown"),
    
    # 8. Combine the origin and ID into the final grouping level for the Term Run files 
    `(R) RESOLVED STOCK-ORIGIN` = paste0(`(R) RESOLVED ORIGIN`, sep=" ", `(R) RESOLVED STOCK ID`)) %>% 
  
  relocate(c(`(R) RETURN YEAR`,
             `(R) TOTAL AGE: CWT`, `(R) TOTAL AGE: PBT`, `(R) TOTAL AGE: SCALE`, `(R) RESOLVED TOTAL AGE`, `(R) RESOLVED TOTAL AGE METHOD`, 
             `(R) BROOD YEAR: CWT`, `(R) BROOD YEAR: PBT`, `(R) BROOD YEAR: SCALE`, `(R) RESOLVED BROOD YEAR`, 
             
             `(R) ORIGIN: CLIP`, `(R) ORIGIN: CWT`, `(R) ORIGIN: PBT`, `(R) ORIGIN: OTOLITH`, `(R) RESOLVED ORIGIN`, `(R) RESOLVED ORIGIN METHOD`,  
             `(R) STOCK ID: CWT`, `(R) STOCK ID: PBT`, `(R) STOCK ID: OTOLITH`, `(R) STOCK ID: OTOLITH METHOD`, `(R) STOCK ID: GSI`,
             `(R) RESOLVED STOCK ID`, `(R) RESOLVED STOCK ID METHOD`, `(R) RESOLVED STOCK-ORIGIN`), .after=last_col()) %>%
  mutate(`(R) CREST BIOKEY` = paste0(stringr::str_sub(Spawning.Year, 3, 4), 
                                     "-", 
                                     stringr::str_extract(File_source, "(?<=All_Adult_Biosampling_)\\d+(?=-)"),
                                     "-",
                                     Activity.No,
                                     "-",
                                     Adult.No)) %>%
  relocate(`(R) CREST BIOKEY`, .before = File_source) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ================== QC ================== 

## Extract PBT parental records ---------------------------
  # --> Can't do this yet as EPRO file doesn't go back far enough (EPRO records only back to ~2021 at best)
# PBT_parents <- esc_biodata_PADS_otoNPAFC_headsCWT %>% 
#   filter(`(R) DNA NUM` %in% c(SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_mFish),]$MGL_mFish, 
#                               SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_dFish),]$MGL_dFish))


## QC flags ---------------------------
# There is brood year data and a useable hatch code but the Otolith stock ID didn't populate (e.g., R code errors)
# qc_noOtoID <- wcviCNepro_w_Results %>%
#   filter(!is.na(`(R) RESOLVED BROOD YEAR`) & !is.na(`(R) HATCHCODE`) & `(R) HATCHCODE` %notin% c("Destroyed", "Not Marked", "No Sample") & is.na(NPAFC_STOCK_1)) %>%
#   print()
# 
# # There is an otolith sample that was taken and a useable brood year, but no otolith result (e.g., sample processing errors)
# qc_noOtoResults <- wcviCNepro_w_Results %>%
#   filter(!is.na(`(R) OTOLITH BOX-VIAL CONCAT`) & !is.na(`(R) RESOLVED BROOD YEAR`) & is.na(`(R) HATCHCODE`)) %>%
#   print()
# 
# # There is a useable CWT but no stock ID (e.g., R code errors)
# qc_noCWTID <- wcviCNepro_w_Results %>% 
#   filter(!is.na(Cwt.Tag.Code) & Cwt.Tag.Code%notin%c("No Tag","Lost Tag","No Head") & Sample.Status=="Tag Read Ok" & is.na(`MRP_Stock Site Name`)) %>% 
#   filter()
# 
# # Stock ID is unknown but there is a useable otolith and/or CWT stock ID available (e.g., code errors)
# qc_noRslvdID <- wcviCNepro_w_Results %>% 
#   filter(`(R) RESOLVED STOCK ID`=="Unknown" & !is.na(NPAFC_STOCK_1) & !is.na(`MRP_Stock Site Name`)) %>% 
#   print()
# 
# # Stock ID(s) disagree (e.g., processing error)
# qc_unRslvdID <- wcviCNepro_w_Results %>% 
#   filter(across(c(`(R) STOCK ID FLAG: CWT-OTO`:`(R) STOCK ID FLAG: PBT-OTO`), ~ grepl("FLAG", .x))) %>%
#   print()
# 
# # Scale, CWT and/or PBT age(s) disagree
# qc_unRslvdAge <- wcviCNepro_w_Results %>% 
#   filter(across(c(`(R) AGE FLAG: CWT-SCALE`:`(R) AGE FLAG: PBT-SCALE`), ~ grepl("FLAG", .x))) %>%
#   print()
# 
# 
## QC summary Report ---------------------------
# qc_summary <- data.frame(qc_flagName = c("QC- No Oto ID",
#                                          "QC- Oto sample no result",
#                                          "QC- No CWT ID",
#                                          "QC- No resolved ID",
#                                          "QC- Stock IDs disagree",
#                                          "QC- Ages disagree",
#                                          "",
#                                          "total EPRO records:"),
#                          number_records = c(nrow(qc_noOtoID),
#                                             nrow(qc_noOtoResults),
#                                             nrow(qc_noCWTID),
#                                             nrow(qc_noRslvdID),
#                                             nrow(qc_unRslvdID),
#                                             nrow(qc_unRslvdAge),
#                                             "",
#                                             nrow(wcviCNepro_w_Results)),
#                          description = c("Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
#                                          "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
#                                          "There is CWT info available but no Stock ID. Note sometimes this is due to exceptionally young age classes (eg, Jimmies) being sampled, or more rarely species ID issues (e.g., tag code 186168). Use https://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/TagSearch to search individual tag #s if concerned about results.",
#                                          "There is a CWT or an NPAFC ID but no Resolved stock ID.",
#                                          "CWT, PBT and/or Otolith stock ID(s) disagree.",
#                                          "CWT, PBT and/or scale age(s) disagree.",
#                                          "",
#                                          paste0("for ", paste(unique(wcviCNepro_w_Results$`(R) RETURN YEAR`), collapse = " ") ))) %>% 
#   print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ================== README ================== 
 readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source EPRO files:",
                             "source NPAFC file:",
                             "CWT tag code source:",
                             "assumptions made/notes:", 
                             "",
                             "TAB NAME",
                             "AllFacilities w RESULTS" #,
                             #"PBT Summary"
                             #"QC Report",
                             #"QC-..."
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/2-EPRO_biodata_with_results.R , https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/EPROcompile.R", 
        "SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RETURN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R (saved from online EPRO output)",
        "SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to {MOST RECENT DATE}.xlsx",
        "http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice", 
        "have manually input good BYs for PBT tag rates at WCVI facilities, these require manual updating.",
        "",
        "TAB DESCRIPTION",
        "All EPRO facilities 'All Adult Biosampling' reports for WCVI combined into 1 file and joined to 1. the NPAFC mark file to give otolith stock ID and 2. CWT releases for last 10 years to give CWT stock ID." #,
        #"Summary of PBT results (only up to 2021) for each system. Flag included for return years with full PBT so that hatchery/natural composition can be used (if desired)."
        #"Summary of QC flags, # of entries belonging to that flag and descriptions.",
        #"QC flag tabs. See QC summary report for details."
        ))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ================== CREATE WORKBOOK ==================

## Create empty workbook ---------------------------
R_OUT_EPRO.NPAFC <- openxlsx::createWorkbook()

## Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "readme")
openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "All Facilities w RESULTS")
#openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "PBT Summary")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC summary")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No Oto stock ID")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Oto sample no result")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No CWT ID")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- No Resolved ID")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Stock IDs disagree")
# openxlsx::addWorksheet(R_OUT_EPRO.NPAFC, "QC- Ages disagree")

## Write data to tabs ---------------------------
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="All Facilities w RESULTS", x=wcviCNepro_w_Results)
#openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="PBT Summary", x=PBTsummary)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet="QC summary", x=qc_summary)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No Oto stock ID", x=qc_noOtoID)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Oto sample no result", x=qc_noOtoResults)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No CWT ID", x=qc_noCWTID)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- No Resolved ID", x=qc_noRslvdID)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Stock IDs disagree", x=qc_unRslvdID)
# openxlsx::writeData(R_OUT_EPRO.NPAFC, sheet = "QC- Ages disagree", x=qc_unRslvdAge)



# ================== EXPORT ================== 
## To github repo ---------------------------
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0(here::here("outputs"), 
                                   "/R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS ",
                                   min(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   "-",
                                   max(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


## To DFO Network drive ---------------------------
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
                                   "/R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS ",
                                   min(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   "-",
                                   max(wcviCNepro_w_Results$`(R) RETURN YEAR`),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


## To SharePoint ---------------------------
  # Discontinue sharepoint export, too confusing with different syncing to different local computers - commit to git and DFO network only
    # openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
    #                        file=paste0(epro_dir, 
    #                                    "/R_OUT - All EPRO facilities master WITH RESULTS ",
    #                                    analysis_year,
    #                                    ".xlsx"),
    #                        overwrite=T,
    #                        returnValue=T)





# /END!


# Cleaup for source() call purposes:
#remove(list=c("CN_relTagCodes", "NPAFC", "readme", "wcviCNepro_w_NPAFC", "wcviCNepro_w_NPAFC.MRP", "wcviEPRO", "analysis_year", "R_OUT_EPRO.NPAFC", "%notin%"))


