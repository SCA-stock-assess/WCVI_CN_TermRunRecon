
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
mrpHeadRcvy.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/Import", 
                                    pattern=".csv", full.names=T), 
                         function(x) {
                           read.csv(x)
                         })

# Change filenames in the List:
names(mrpHeadRcvy.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/Import", 
                                 pattern=".csv", full.names=T)


# Convert the Large List into a useable R dataframe ---------------------------
mrpHeadRcvy <- do.call("rbind", mrpHeadRcvy.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  setNames(paste0('MRP_', names(.))) %>% 
  rename(`(R) HEAD LABEL` = MRP_LabelId,
         `(R) SAMPLE YEAR` = MRP_RecoveryYear) %>%
  mutate_at(c("(R) HEAD LABEL", "(R) SAMPLE YEAR"), as.character) %>%
  print()

# Clean up ---------------------------
remove(mrpHeadRcvy.LL)




#############################################################################################################################################################

#                                                          2. EXPORT


# Export to Network ---------------------------
writexl::write_xlsx(mrpHeadRcvy, 
                    path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/Export",
                                "/R_OUT - MPRHeadRecoveries_AllSpecies",
                                "_",
                                min(mrpHeadRcvy$`(R) SAMPLE YEAR`),
                                "-",
                                max(mrpHeadRcvy$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))


# Export to github ---------------------------
writexl::write_xlsx(mrpHeadRcvy, 
                    path = paste0(here("outputs"),
                                "/R_OUT - MPRHeadRecoveries_AllSpecies",
                                "_",
                                min(mrpHeadRcvy$`(R) SAMPLE YEAR`),
                                "-",
                                max(mrpHeadRcvy$`(R) SAMPLE YEAR`),
                                "_LastUpdate_",
                                Sys.Date(),
                                ".xlsx"))





# Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.' 
# means you have the file open




# /END!