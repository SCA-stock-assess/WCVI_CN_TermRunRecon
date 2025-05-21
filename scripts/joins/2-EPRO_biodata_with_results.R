
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
# saves as wcviEPRO



################################################################################################################################################

#                                                                           II. NPAFC LOAD


NPAFC <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All CN Marks from NPAFC",    
                                            full.names = TRUE)) %>% 
  setNames(paste0('NPAFC_', names(.))) %>% 
  rename(`(R) HATCHCODE` = NPAFC_HATCH_CODE,
         `(R) RESOLVED BROOD YEAR` = NPAFC_BROOD_YEAR
         ) %>% 
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

#                                                                           VI. LOAD PBT INVENTORY


# ======================== Load PBT inventory ========================  
# Run PBT source code -------------------------   
# source(here::here("scripts", "misc-helpers", "CalcReliablePBT.R"))   -- need to update file call in source script
# saves a few dataframes 





#############################################################################################################################################################

#                                                                           VII. ASSIGN FINAL STOCK ID and ORIGIN



wcviCNepro_w_Results <- wcviCNepro_w_NPAFC.MRP %>%
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
         `(R) ORIGIN: PBT` = case_when((!is.na(Sire.Dna.Waterbody.Site.Name) & !grepl("N/A", Sire.Dna.Waterbody.Site.Name)) | 
                                         (!is.na(Dam.Dna.Waterbody.Site.Name) & !grepl("N/A", Dam.Dna.Waterbody.Site.Name)) ~ "Hatchery",
                                       
                                       # PBT hit absent (stock/BY dependent)
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR`>=2013 & grepl("robertson|sarita", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR`>=2020 & grepl("conuma", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR`>=2019 & grepl("nitinat", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       (is.na(Sire.Dna.Waterbody.Site.Name) | grepl("N/A", Sire.Dna.Waterbody.Site.Name)) & 
                                         (is.na(Dam.Dna.Waterbody.Site.Name) | grepl("N/A", Dam.Dna.Waterbody.Site.Name)) & 
                                         (`(R) RESOLVED BROOD YEAR`>=2018 & grepl("san juan", Spawning.Stock.Name, ignore.case=T)) ~ "Natural",
                                       
                                       TRUE ~ NA),
         `(R) ORIGIN: OTOLITH` = case_when(grepl("H|,", Otolith.Hatch.Code) #& !grepl("test|no|not|NS|destroyed|NM|unreadable|0|unmountable", Otolith.Hatch.Code, ignore.case=T) 
                                           ~ "Hatchery", 
                                           Otolith.Hatch.Code == "NO MARK" ~ "Natural",
                                           TRUE ~ "Unknown"),
         
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
                                       is.na(Dam.Dna.Waterbody.Site.Name) & (!is.na(Sire.Dna.Waterbody.Site.Name) & !grepl("N/A", Sire.Dna.Waterbody.Site.Name)) ~ sub(" [^ ]+$", "", Sire.Dna.Waterbody.Site.Name),
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
    `(R) RESOLVED STOCK ID METHOD` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ "CWT",
                                               is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) ~ "PBT",
                                               is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) 
                                               & !is.na(`(R) STOCK ID: OTOLITH METHOD`) ~ paste0("Otolith", sep=" - ", `(R) STOCK ID: OTOLITH METHOD`),
                                               TRUE ~ NA),
    
    # 7. Assign the final stock ID: CWT > PBT > Otolith (with varying levels of oto certainty)
    `(R) RESOLVED STOCK ID` = case_when(!is.na(`(R) STOCK ID: CWT`) ~ `(R) STOCK ID: CWT`,
                                        is.na(`(R) STOCK ID: CWT`) & !is.na(`(R) STOCK ID: PBT`) ~ `(R) STOCK ID: PBT`,
                                        is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: PBT`) 
                                        & !is.na(`(R) STOCK ID: OTOLITH`) ~ `(R) STOCK ID: OTOLITH`,
                                        #is.na(`(R) STOCK ID: CWT`) & is.na(`(R) STOCK ID: OTOLITH`) & !is.na(`(R) OTOLITH FACILITY ID`) ~ `(R) OTOLITH FACILITY ID`,            # irrelevant for EPRO output
                                        grepl("Natural", `(R) RESOLVED ORIGIN`, ignore.case=T) ~ paste0(stringr::str_to_title(str_sub(gsub(pattern=" R Fall Chinook", replacement="", Spawning.Stock.Name), 6, -1)), 
                                                                         " (assumed)"), 
                                        TRUE ~ "Unknown"),
    
    # 8. Combine the origin and ID into the final grouping level for the Term Run files 
    `(R) RESOLVED STOCK-ORIGIN` = paste0(`(R) RESOLVED ORIGIN`, sep=" ", `(R) RESOLVED STOCK ID`)) %>% 
  
  relocate(c(`(R) RETURN YEAR`,
             `(R) TOTAL AGE: CWT`, `(R) TOTAL AGE: PBT`, `(R) TOTAL AGE: SCALE`, `(R) RESOLVED TOTAL AGE`, `(R) RESOLVED TOTAL AGE METHOD`, 
             `(R) BROOD YEAR: CWT`, `(R) BROOD YEAR: PBT`, `(R) BROOD YEAR: SCALE`, `(R) RESOLVED BROOD YEAR`, 
             
             `(R) ORIGIN: CLIP`, `(R) ORIGIN: CWT`, `(R) ORIGIN: PBT`, `(R) ORIGIN: OTOLITH`, `(R) RESOLVED ORIGIN`, `(R) RESOLVED ORIGIN METHOD`,  
             `(R) STOCK ID: CWT`, `(R) STOCK ID: CWT`, `(R) STOCK ID: OTOLITH`, `(R) STOCK ID: OTOLITH METHOD`, 
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



View(
  wcviCNepro_w_Results %>% 
    filter(Spawning.Stock.Name=="2023 San Juan R Fall Chinook") %>%
    select(Otolith.Hatch.Code, `(R) HATCHCODE`, `(R) HATCH CODE test`, Otolith.Bag.No, Otolith.Vial.No)
)




#############################################################################################################################################################

#                                                                                 PBT SUMMARY 

# In years prior to RY 2024, PBT was not always tracked at the individual fish level; rather, it was collected from a group of fish (say, over a week), 
#   stratified by sex, and submitted in bulk. I think this varied depending on the hatchery facility. 
#   Seeing as tracking this detail down is currently beyond the scope of this exercise, instead supplementary tabs are provided that show the PBT stock and
#   origin summaries for the years with full returns. These summaries are provided on secondary tabs in the exported Excel file. 
#   The purpose is for analysts to be able to assess the stock/origin composition of the otolith/CWT/scale dataset, and compare it to the PBT dataset for
#   whatever years are available. 
#   For simplicity, this is only done for stock-years with a reliable, complete PBT baseline (i.e., ages 2-6 were all PBTed at > 70% tag rate). Therefore,
#   it is a minimal summary as PBT is relatively new. 

PBTsummary <- left_join(PBTresults %>% 
                          group_by(MGL_Brood_Collection, MGL_oYear, MGL_Offspring_Age, MGL_Parental_Collection) %>% 
                          summarize(n=n()) %>% 
                          ungroup() %>%
                          rename(BY = MGL_oYear,
                                 Broodstock_collection = MGL_Brood_Collection,
                                 Resolved_age = MGL_Offspring_Age,
                                 Resolved_hatchery_PBTorigin = MGL_Parental_Collection) %>%
                          mutate(`(R) STOCK` = gsub(gsub(Broodstock_collection, pattern=" River", replacement=""),
                                                    pattern=" Creek", replacement="")),
                        PBTreliableShort %>% 
                          select(-c(fullPBT_BYid)) %>%
                          mutate(flag = "RETURN YEAR WITH FULL PBT BASELINE"),
                        by=c("BY" = "firstFullReturnYr",
                             "(R) STOCK")
)


#############################################################################################################################################################

#                                                                           VIII. QC and readme

 
# ================== QC ================== 
 
#  Extract PBT parental records ---------------------------
  # --> Can't do this yet as EPRO file doesn't go back far enough (EPRO records only back to ~2021 at best)
# PBT_parents <- esc_biodata_PADS_otoNPAFC_headsCWT %>% 
#   filter(`(R) DNA NUM` %in% c(SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_mFish),]$MGL_mFish, 
#                               SC_PBT_SEP[!is.na(SC_PBT_SEP$MGL_dFish),]$MGL_dFish))


# QC flags ---------------------------
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
# # QC summary Report ---------------------------
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



# ================== README ================== 
 readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source EPRO files:",
                             "source NPAFC file:",
                             "CWT tag code source:",
                             "assumptions made/notes:", 
                             "",
                             "TAB NAME",
                             "AllFacilities w RESULTS" ,
                             "PBT Summary"
                             #"QC Report",
                             #"QC-..."
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/2-EPRO_biodata_with_results.R , https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/EPROcompile.R", 
        "SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RETURN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R (saved from online EPRO output)",
        "SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to {MOST RECENT DATE}.xlsx",
        "http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice", 
        "2021 CURRENTLY INCOMPLETE - EPRO STILL UPDATING HISTORICAL YEARS",
        "",
        "TAB DESCRIPTION",
        "All EPRO facilities 'All Adult Biosampling' reports for WCVI combined into 1 file and joined to 1. the NPAFC mark file to give otolith stock ID and 2. CWT releases for last 10 years to give CWT stock ID.",
        "Summary of PBT results (only up to 2021) for each system. Flag included for return years with full PBT so that hatchery/natural composition can be used (if desired)."
        #"Summary of QC flags, # of entries belonging to that flag and descriptions.",
        #"QC flag tabs. See QC summary report for details."
        ))


#############################################################################################################################################################

#                                                                           X. EXPORT 


# ================== Create excel file ==================

# Create empty workbook ---------------------------
R_OUT_EPRO.NPAFC <- openxlsx::createWorkbook()

# Add empty tabs to the workbook ---------------------------
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

# Write data to tabs ---------------------------
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



# ================== Export ================== 
# To github repo ---------------------------
openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
                       file=paste0(here::here("outputs"), 
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





