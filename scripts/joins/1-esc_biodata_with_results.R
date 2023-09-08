
# Chinook escapement biodata WITH results for Terminal run recon


# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)


# Helpers -------------
"%notin%" <- Negate("%in%")
analysis_year <- 2022



# this file: esc data links
# OUTLINE:
# 1. escapement data - load from network drive. 
# 2. age data - direct query is chinook all years, then filtered to current year. source to function script. 
# 3. otolith data - load from SharePoint; query is chinook all areas current year already so no filtering needed. 
# 4. 


# ********** CANNOT PROCEED WITH THIS SCRIPT: There are fundamental differences in how data are entered, e.g., if a sample is in scale book cell 1,2,3 vs. 1, 11,21. 
# canot resolve without the data group making systemic change, therefore will not be proceeding further.




################################################################################################################################################


#                                                                           I. ESCAPEMENT BIODATA LOAD 



# 1. Examine escapement biodata files avialable: 
list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/"), 
           recursive=F, pattern="^[^~]*.xlsx") 

# 2. Select the most recent one. This is manual because the naming convention sucks: 
esc_biodata_recent_filename <- list.files(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement"),
                                          recursive=F, pattern="^[^~]*.xlsx")[4]

#3. Read in the file and reformat:   (slow)
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
  mutate(`(R) OTOLITH BOX NUM`=`Otolith Box #`,
         `(R) OTOLITH VIAL NUM` = `Otolith Specimen #`,
         `(R) OTOLITH LAB NUM` = `Otolith Lab Number`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`Otolith Lab Number`) & !is.na(`Otolith Box #`) & !is.na(`Otolith Specimen #`) ~ 
                                                paste0(`Otolith Lab Number`,sep="-",`Otolith Box #`,sep="-",`Otolith Specimen #`)),
         
         `(R) SCALE BOOK NUM` = `Scale Book #`,
         
         `(R) FIELD SCALE BOOK NUM` = case_when(`Scale Book #` %in% c(15983:15987,15989) ~ paste0("22SC ", `Scale Book #` ),
                                          `Scale Book #` %in% c(17376,17377) ~ paste0("22SP", `Scale Book #` ),
                                          TRUE ~ NA),
         
         
         
         
         `(R) SCALE CELL NUM` = `Scale #`,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & `Scale #`%in%c(1:51) ~ paste0(`(R) SCALE BOOK NUM`,sep="-",`Scale #`)),
         `(R) FIELD SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) FIELD SCALE BOOK NUM`) ~ paste0(`(R) FIELD SCALE BOOK NUM`,"-",`(R) SCALE CELL NUM`),
                                                        TRUE ~ NA),
         
         `(R) RESOVLED SCALE BOOK-CELL CONCAT` = case_when(is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) SCALE BOOK-CELL CONCAT`,
                                                           !is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) FIELD SCALE BOOK-CELL CONCAT`,
                                                           TRUE~"FLAG"),
         
         
         `(R) SAMPLE YEAR` = Year,
         `(R) HEAD LABEL` = `CWT Head Label #`,
         RrowID = seq(1:nrow(.))) %>%
  drop_na(Year) %>%    # remove entries without year specified
  mutate_at(c("(R) SCALE BOOK NUM", "(R) HEAD LABEL"), as.character) %>%
  filter(Year %in% analysis_year, Species=="Chinook") %>%
  print()



 #*** HERE NEXT DAY: 
# fix scale book numbers that are green in the allPADStest.csv column - in our esc database they need to be changed to "22SC..." or esle they won't join upt o PADS
### BUT FIRST CHECK AND SEE IF THEY FIXED THIS IN NEW TABS OF THE ESC DATABASE (E.G., BIODATA2022)




################################################################################################################################################

#                                                                           II. AGE DATA LOAD 


source(here("scripts","functions","getAgeDataMRP.R"))

wcviCNPADS2022 <- allPADS %>% 
  filter(RecoveryYear %in% analysis_year, Area%in%c(20,21,22,23,24,25,26,27), Species=="Chinook") %>% 
  filter(!grepl("Georgia Str|Sooke", ProjectName)) %>%
  mutate_at("(R) SCALE CELL NUM", as.character) %>%
  mutate(`(R) FIELD SCALE BOOK NUM` = case_when(!is.na(FieldContainerId) ~ FieldContainerId,
                                                TRUE ~ NA),
         `(R) FIELD SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) FIELD SCALE BOOK NUM`) ~ paste0(`(R) FIELD SCALE BOOK NUM`,"-", FishNumber),
                                                        TRUE ~ NA),
         `(R) RESOVLED SCALE BOOK-CELL CONCAT` = case_when(is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) SCALE BOOK-CELL CONCAT`,
                                                           !is.na(`(R) FIELD SCALE BOOK-CELL CONCAT`) ~ `(R) FIELD SCALE BOOK-CELL CONCAT`,
                                                           TRUE~"FLAG")) %>% 
  print()


################################################################################################################################################

#                                                                           II. OTOLITH DATA LOAD 


wcviCNOtos2022 <- readxl::read_excel(path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
                                                        "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx"),
                                            sheet="RcvySpecAge", skip=1, guess_max=20000) %>%
  mutate(`(R) OTOLITH BOX NUM` = `BOX CODE`,
         `(R) OTOLITH VIAL NUM` = `CELL NUMBER`,
         `(R) OTOLITH LAB BUM` = `LAB NUMBER`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`LAB NUMBER`) & !is.na(`BOX CODE`) & !is.na(`CELL NUMBER`) ~ 
                                                paste0(`LAB NUMBER`,sep="-",`BOX CODE`,sep="-",`CELL NUMBER`))) %>%
  print()



#############################################################################################################################################################

#                                                                           III. JOIN ESCAPEMENT BIODATA to PADS 


# NEXT DAY:

#   join esc + pads
# join esc+pads + otomgr
# calculate BY
# join esc+pads+otomgr + NPAFC




# ======================== JOIN ESCAPEMENT BIODATA + PADS ========================  
intersect(colnames(wcviCNesc2022), colnames(wcviCNPADS2022))

esc_biodata_PADS <- left_join(wcviCNesc2022,
                              wcviCNPADS2022, by=c("Species", "(R) RESOVLED SCALE BOOK-CELL CONCAT"))

writexl::write_xlsx(esc_biodata_PADS, "CNesc_biodataPADS_TEST.xlsx")






















