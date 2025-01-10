
# OtoCompile
# KD Oct 2023



# Load packages ----------------
#library(here)
#library(tidyverse)
#library(readxl)
#library(writexl)




#############################################################################################################################################################

#                                                           1. LOAD HEAD RCVY BASE FILES (historical data)


# Read CWT recovery files as large list ---------------------------
# Load base files to compile
CN_headRcvy.LL <- lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/1-Import-to-R", 
                                    pattern=".csv", full.names=T), 
                         function(x) {
                           read.csv(x)
                         })

# Change filenames in the List:
names(CN_headRcvy.LL) <- list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/1-Import-to-R", 
                                 pattern=".csv", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
CN_headRcvy <- do.call("rbind", CN_headRcvy.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  setNames(paste0('MRP_', names(.))) %>% 
  rename(`(R) HEAD LABEL` = MRP_LabelId,
         `(R) SAMPLE YEAR` = MRP_RecoveryYear) %>%
  mutate_at(c("(R) HEAD LABEL"), as.character) %>%
  filter(MRP_Species=="Chinook") %>% 
  print()

# Clean up ---------------------------
remove(CN_headRcvy.LL)




#############################################################################################################################################################

#                                                          2. EXPORT


# Export to Network ---------------------------
writexl::write_xlsx(CN_headRcvy, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/2-Export-from-R",
                                "/R_OUT - MRPHeadRecoveries_CHINOOK_",
                                min(CN_headRcvy$`(R) SAMPLE YEAR`),
                                "-",
                                max(CN_headRcvy$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))


write.csv(CN_headRcvy, 
          file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/2-Export-from-R",
                      "/R_OUT - MRPHeadRecoveries_CHINOOK_",
                      min(CN_headRcvy$`(R) SAMPLE YEAR`),
                      "-",
                      max(CN_headRcvy$`(R) SAMPLE YEAR`),
                      "_LastUpdate_",
                      Sys.Date(),
                      ".csv"))


# Export to github ---------------------------
writexl::write_xlsx(CN_headRcvy, 
                    path = paste0(here::here("outputs"),
                                  "/R_OUT - MRPHeadRecoveries_CHINOOK_",
                                  min(CN_headRcvy$`(R) SAMPLE YEAR`),
                                  "-",
                                  max(CN_headRcvy$`(R) SAMPLE YEAR`),
                                  "_LastUpdate_",
                                  Sys.Date(),
                                  ".xlsx"))

write.csv(CN_headRcvy, 
          file=paste0(here::here("outputs"),
                      "/R_OUT - MRPHeadRecoveries_CHINOOK_",
                      min(CN_headRcvy$`(R) SAMPLE YEAR`),
                      "-",
                      max(CN_headRcvy$`(R) SAMPLE YEAR`),
                      "_LastUpdate_",
                      Sys.Date(),
                      ".xlsx"))


# Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.' 
# means you have the file open




# /END!