
# OtoCompile 
# KD Oct 2023



# Load packages ----------------
#library(here)
#library(tidyverse)
#library(readxl)
#library(writexl)



#############################################################################################################################################################

#                                                     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ For RECOVERY Specimens ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== 1. LOAD OTOLITH MANAGER RECOVERY SPEC BASE FILES (historical data) ==================== 


# Read OtoManager files as large list ---------------------------
# Load base files to compile
wcviOtos.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/1-Import-to-R", 
                                 pattern=".xlsx", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="RcvySpecAge", skip=1, guess_max=20000)
                      })

# Change filenames in the List:
names(wcviOtos.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/1-Import-to-R", 
                                 pattern=".xlsx", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
wcviOtos <- do.call("rbind", wcviOtos.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  setNames(paste0('OM_', names(.))) %>% 
  mutate(OM_FACILITY = case_when(OM_FACILITY=="H-ROBERTSON CR" ~ "H-ROBERTSON CREEK H",
                                 OM_FACILITY=="H-CONUMA R " ~ "H-CONUMA RIVER H ",
                                 grepl("GWA'NI", OM_FACILITY) ~ "H-GAW'NI H",
                                 grepl("INCH CR", OM_FACILITY) ~ "H-INCH CREEK H",
                                 grepl("NANAIMO", OM_FACILITY) ~ "H-NANAIMO RIVER H",
                                 grepl("NITINAT", OM_FACILITY) ~ "H-NITINAT RIVER H",
                                 grepl("QUINSAM", OM_FACILITY) ~ "H-QUINSAM RIVER H",
                                 grepl("WALLACE", OM_FACILITY) ~ "WALLACE RIVER HATCHERY",
                                 TRUE ~ as.character(OM_FACILITY)),
         `(R) OTOLITH BOX NUM` = `OM_BOX CODE`,
         `(R) OTOLITH VIAL NUM` = `OM_CELL NUMBER`,
         `(R) OTOLITH LAB NUM` = `OM_LAB NUMBER`,
         `(R) OTOLITH LBV CONCAT` = case_when(!is.na(`OM_LAB NUMBER`) & !is.na(`OM_BOX CODE`) & !is.na(`OM_CELL NUMBER`) ~ 
                                                paste0(`OM_LAB NUMBER`,sep="-",`OM_BOX CODE`,sep="-",`OM_CELL NUMBER`))) %>%
  rename(`(R) HATCHCODE` = `OM_HATCH CODE`,
         `(R) SAMPLE YEAR` = `OM_SAMPLE YR`) %>%
  mutate_at("(R) OTOLITH VIAL NUM", as.character) %>%
  mutate_at("(R) SAMPLE YEAR", as.character) %>% 
  print()

# Clean up ---------------------------
remove(wcviOtos.LL)





# ==================== 2. EXPORT ==================== 


# Export to Network ---------------------------
writexl::write_xlsx(wcviOtos, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/2-Export-from-R",
                                "/R_OUT - OtoManager_AllSpecies_Area20-27andOffshore",
                                "_",
                                min(wcviOtos$`(R) SAMPLE YEAR`),
                                "-",
                                max(wcviOtos$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))



# Export to github ---------------------------
writexl::write_xlsx(wcviOtos, 
                    path=paste0(here::here("outputs", "R_OUT - OtoManager_AllSpecies_Area20-27andOffshore_"),
                                min(wcviOtos$`(R) SAMPLE YEAR`),
                                "-",
                                max(wcviOtos$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))




# "Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.' "   --> means you have the file open



#############################################################################################################################################################
#############################################################################################################################################################


#                                                     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ For REFERENCE Specimens ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# ==================== 1. LOAD OTOLITH MANAGER REFERENCE SPEC BASE FILES ====================


# Read OtoManager files as large list ---------------------------
# Load base files to compile
OtosRef.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/1-Import-to-R", 
                                pattern=".xlsx", full.names=T), 
                     function(x) {
                       readxl::read_excel(x, sheet="ReferenceSpecimens", skip=1, guess_max=20000)
                     })

# Change filenames in the List:
names(OtosRef.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/1-Import-to-R", 
                                pattern=".xlsx", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
OtosRef <- do.call("rbind", OtosRef.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  select(-c(`...15`)) %>%
  mutate_at(c("BROOD YEAR", "RELEASE YEAR"), as.numeric) %>%
  print()

# Clean up ---------------------------
remove(OtosRef.LL)





# ==================== 2. EXPORT ====================


# Export to Network ---------------------------
writexl::write_xlsx(OtosRef, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/2-Export-from-R",
                                "/R_OUT - OtoManager_CN_REFERENCEspecimens_BY_",
                                min(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "-",
                                max(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))


# Export to github ---------------------------
writexl::write_xlsx(wcviOtos, 
                    path=paste0(here::here("outputs", "R_OUT - OtoManager_CN_REFERENCEspecimens_BY_"),
                                min(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "-",
                                max(OtosRef[!is.na(OtosRef$`BROOD YEAR`),]$`BROOD YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))



# Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.'   means you have the file open






# /END!