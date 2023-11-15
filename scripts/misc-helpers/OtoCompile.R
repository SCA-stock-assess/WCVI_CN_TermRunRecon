
# OtoCompile 
# KD Oct 2023



# Load packages ----------------
#library(here)
#library(tidyverse)
#library(readxl)
#library(writexl)



#############################################################################################################################################################

#                                                     ==================== For RECOVERY Specimens ==========================


#                                                           1. LOAD OTOLITH MANAGER RECOVERY SPEC BASE FILES (historical data)


# Read OtoManager files as large list ---------------------------
# Load base files to compile
wcviOtos.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/Import", 
                                 pattern=".xlsx", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="RecoverySpecimens", skip=1, guess_max=20000)
                      })

# Change filenames in the List:
names(wcviOtos.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/Import", 
                                 pattern=".xlsx", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
wcviOtos <- do.call("rbind", wcviOtos.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  select(-c(`...5`)) %>%
  setNames(paste0('OM_', names(.))) %>% 
  mutate(OM_FACILITY = case_when(OM_FACILITY=="H-ROBERTSON CR" ~ "H-ROBERTSON CREEK H",
                                 OM_FACILITY=="H-CONUMA R " ~ "H-CONUMA RIVER H ",
                                 TRUE~as.character(OM_FACILITY)),
         `(R) OTOLITH BOX NUM` = `OM_BOX CODE`,
         `(R) OTOLITH VIAL NUM` = `OM_CELL NUMBER`,
         `(R) OTOLITH LAB NUM` = `OM_LAB NUMBER`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`OM_LAB NUMBER`) & !is.na(`OM_BOX CODE`) & !is.na(`OM_CELL NUMBER`) ~ 
                                                paste0(`OM_LAB NUMBER`,sep="-",`OM_BOX CODE`,sep="-",`OM_CELL NUMBER`))) %>%
  rename(`(R) HATCHCODE` = `OM_HATCH CODE`,
         `(R) SAMPLE YEAR` = `OM_SAMPLE YR`) %>%
  mutate_at("(R) OTOLITH VIAL NUM", as.character) %>%
  #select(-c(`...5`)) %>%
  print()

# Clean up ---------------------------
remove(wcviOtos.LL)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                          2. EXPORT


# Export to Network ---------------------------
writexl::write_xlsx(wcviOtos, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/Export",
                                "/R_OUT - OtoManager_AllSpecies_Area20-27andOffshore",
                                "_",
                                min(wcviOtos$`(R) SAMPLE YEAR`),
                                "-",
                                max(wcviOtos$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))

# Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.' 
# means you have the file open



#############################################################################################################################################################
#############################################################################################################################################################


#                                                     ==================== For REFERENCE Specimens ==========================


#                                                           1. LOAD OTOLITH MANAGER REFERENCE SPEC BASE FILES


# Read OtoManager files as large list ---------------------------
# Load base files to compile
OtosRef.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/Import", 
                                pattern=".xlsx", full.names=T), 
                     function(x) {
                       readxl::read_excel(x, sheet="ReferenceSpecimens", skip=1, guess_max=20000)
                     })

# Change filenames in the List:
names(OtosRef.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/Import", 
                                pattern=".xlsx", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
OtosRef <- do.call("rbind", OtosRef.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  select(-c(`...15`)) %>%
  mutate_at(c("BROOD YEAR", "RELEASE YEAR"), as.numeric) %>%
  print()

# Clean up ---------------------------
remove(OtosRef.LL)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                          2. EXPORT


# Export to Network ---------------------------
writexl::write_xlsx(OtosRef, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/Export",
                                "/R_OUT - OtoManager_CN_REFERENCEspecimens",
                                "_BY",
                                min(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "-",
                                max(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))

# Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'   means you have the file open






# /END!