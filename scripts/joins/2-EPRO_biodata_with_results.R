
# Compile EPRO to join to NPAFC stock ID 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)


# Helpers -------------
"%notin%" <- Negate("%in%")
analysis_year <- 2022






################################################################################################################################################


#                                                                           I. EPRO FILES LOAD 



# Load files from Sharepoint -------------
epro.files <- lapply(list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                      "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/"), 
                               pattern = "All_Adult_Biosampling_", full.names = T), 
                    readxl::read_excel,
                    guess_max=20000)
# Should be a Large List of at least 7 elements: Burman, Conuma, Gold, Nahmint, Nitinat, Robertson, Sarita


# Change filenames in the List -------------
names(epro.files) <- list.files(paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                      "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/"), 
                               pattern = "All_Adult_Biosampling_", full.names = F)


# Convert the Large List into a useable R dataframe ---------------------------
wcviCNepro2022 <- do.call("rbind", epro.files) %>%
  #filter(Spawning.Stock !="") %>%
  tibble::rownames_to_column(var="file_source") %>%
  mutate(`(R) OTOLITH BOX NUM` = `Bag No`,
         `(R) OTOLITH VIAL NUM` = `Vial No`,
         `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(`Bag No`) & !is.na(`Vial No`) ~ paste0(`Bag No`,sep="-",`Vial No`)),
         `(R) SCALE BOOK NUM` = `Book No`,
         `(R) SCALE CELL NUM` = `Scale Sample No`,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Book No`) & !is.na(`Scale Sample No`) ~ paste0(`Book No`,sep="-",`Scale Sample No`)),
         `(R) TAGCODE` = `CWT Tag Code`,
         `(R) HATCHCODE` = `Hatch Code`,
         `(R) RESOLVED TOTAL AGE` = case_when(!is.na(`Total Age (yrs)`) ~ `Total Age (yrs)`,
                                              `Scale Part Age`=="2M" ~ 2,
                                              `Scale Part Age`=="3M" ~ 3,
                                              `Scale Part Age`=="4M" ~ 4,
                                              `Scale Part Age`=="5M" ~ 5,
                                              `Scale Part Age`=="6M" ~ 6),
         `(R) BROOD YEAR` = analysis_year-`Total Age (yrs)`,
         UEID = paste0("2022", "-", seq(1:nrow(wcviCNepro2022)))) %>%
  mutate_at(c("(R) SCALE BOOK NUM", "Book No"), as.character) %>%
  print()
remove(epro.files)



# Export to git and SP ---------------------------
# To git:
writexl::write_xlsx(wcviCNepro2022, path=paste0(here("outputs"), sep="/", "R_OUT - All EPRO facilities master.xlsx"))


# To SP: 
writexl::write_xlsx(wcviCNepro2022, path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
                                                "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/R_OUT - All EPRO facilities master.xlsx"))





################################################################################################################################################

#                                                                           (ignore). OTOLITH DATA LOAD 


# wcviCNOtos2022 <- readxl::read_excel(path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/",
#                                                  "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx"),
#                                      sheet="RcvySpecAge", skip=1, guess_max=20000) %>%
#   mutate(`(R) OTOLITH BOX NUM` = `BOX CODE`,
#          `(R) OTOLITH VIAL NUM` = `CELL NUMBER`,
#          `(R) OTOLITH LAB BUM` = `LAB NUMBER`,
#          `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(`BOX CODE`) & !is.na(`CELL NUMBER`) ~ paste0(`BOX CODE`,sep="-",`CELL NUMBER`)),
#          `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`LAB NUMBER`) & !is.na(`BOX CODE`) & !is.na(`CELL NUMBER`) ~ 
#                                                 paste0(`LAB NUMBER`,sep="-",`BOX CODE`,sep="-",`CELL NUMBER`))) %>%
#   print()
# 
# 
# 
# writexl::write_xlsx(wcviCNOtos2022, path=paste0("C:/Users", sep="/", Sys.info()[6], sep="/", 
#                                                 "Desktop/ototest.xlsx"))




#############################################################################################################################################################

#                                                                           II. NPAFC LOAD

# ASSUMPTION: 
# - Assuming only BC marks are relevant. 


NPAFC <- readxl::read_excel(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                            pattern = "^All CN Marks",   #ignore temp files, eg "~All CN Marks...,
                                            full.names = TRUE), 
                            sheet="AllNPAFC CNReleasestoJun8,2022") %>% 
  rename(`(R) HATCHCODE` = HATCH_CODE,
         `(R) BROOD YEAR` = BROOD_YEAR) %>% 
  mutate(FACILITY = case_when(is.na(FACILITY) ~ AGENCY,
                              TRUE ~ FACILITY),
         STOCK = case_when(is.na(STOCK) ~ FACILITY,
                           TRUE ~ STOCK)) %>%
  # ASSUPMTIONS: 
  # 1. Alaska and Kamchatka marks should essentially not exist, so remove them to avoid duplicate marks 
  filter(STATE_PROVINCE %in% c("BRITISH COLUMBIA", "IDAHO", "WASHINGTON", "OREGON"),
         # 2. Remove the one case where RCH and Nanaimo Hatchery used the same mark in 2018 and assume it was a RCH fish
         !grepl("NANAIMO", FACILITY) | `(R) BROOD YEAR`!=2018 | `(R) HATCHCODE`!="H5") %>%
  select(`(R) BROOD YEAR`, FACILITY, RELEASE_YEAR, STOCK, `(R) HATCHCODE`) %>% 
  print()


#############################################################################################################################################################

#                                                                           III. EPRO + NPAFC


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(wcviCNepro2022), colnames(NPAFC))

wcviCNepro_w_NPAFC2022 <- left_join(wcviCNepro2022 ,
                                    NPAFC,
                                    by=c("(R) BROOD YEAR", "(R) HATCHCODE"),
                                    relationship="many-to-one")



# Export to git and SP ---------------------------
# To git:
writexl::write_xlsx(wcviCNepro_w_NPAFC2022, path=paste0(here("outputs"), sep="/", "R_OUT - All EPRO facilities master WITH NPAFC.xlsx"))


# To SP: 
writexl::write_xlsx(wcviCNepro_w_NPAFC2022, 
                    path=paste0("C:/Users", sep="/", 
                                Sys.info()[6], 
                                sep="/",
                                "DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/2022/Communal data/EPRO/R_OUT - All EPRO facilities master WITH NPAFC.xlsx"))














