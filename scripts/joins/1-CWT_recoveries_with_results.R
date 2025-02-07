
# Join CWT release data to Chinook head recovery numbers (currently, MRP only outputs by CWT code, not head tag number)

# Work flow is:
# ( If running for first time this year )
# 1.    Go to MRPIS website and download year-specific recovery file: https://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/RecoveryExport
# 2.    Save to Network drive: SCD_Stad\WCVI\CHINOOK\WCVI_TERMINAL_RUN\Annual_data_summaries_for_RunRecons\HeadRcvyCompile_base-files\1-Import-to-R
# 3.    Run source() code around line 56 ("Option 1")
# ( If just refreshing within a year for edits etc. )
# 1.    Run code starting around line 59 ("Option 2")
# FOR BOTH CASES:
# 2.    Run the rest of the code from Part II onward. File will save to github repo and network (Y:\WCVI\CHINOOK\WCVI_TERMINAL_RUN\Annual_data_summaries_for_RunRecons\HeadRcvyCompile_base-files\2-Export-from-R)


################################################################################################################################################
################################################################################################################################################

# BEFORE YOU START: 
# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


# Define analysis year:
#analysis_year <- 2023



################################################################################################################################################
################################################################################################################################################

# Now should be able to high light and run all! 


# Load packages ----------------
library(tidyverse)



# Helpers -------------
"%notin%" <- Negate("%in%")




################################################################################################################################################

#                                                                           I. Compile/load recoveries 


# Option 1:   Run compile file to compile year-specific files (annual or semi-annual, not required every time *SLOW*) ------------------
# source(here::here("scripts", "misc-helpers", "HeadRcvyCompile.R"))
  # saves as CN_headRcvy


# Option 2:   Load already compiled file from above ------------------
# there is an excel file available, but it often times out memory allocation, so recommend loading the csv version 
CN_headRcvy <- read.csv(file=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/2-Export-from-R",
                                        pattern = "^R_OUT - MRPHeadRecoveries_CHINOOK_.*\\.csv$",    
                                        full.names = TRUE)) 
                   




################################################################################################################################################

#                                                                           II. Compile/load releases


# Option 1:   Run saaWeb query to dump releases from MRP (annual or semi-annual, not required every time, *VERY SLOW*) ------------------
# source(here::here("scripts", "functions", "pullChinookCWTReleases.R"))
# saves as CN_relTagCodes


# Option 2:   Load already compiled file from above ------------------
# there is an excel file available, but it often times out memory allocation, so recommend loading the csv version 
CN_relTagCodes <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons",
                                                     pattern = "^R_OUT - Chinook CWT release tagcodes BY .*\\.xlsx$",    
                                                     full.names = T),
                                     sheet="Sheet1")




#############################################################################################################################################################

#                                                                           III. Join recovery records to release tag codes for Stock ID


# Join head recoveries to tag codes for release stock IDs ---------------------------
CN_headRcvyResults <- left_join(CN_headRcvy,
                                CN_relTagCodes,
                               by=c("MRP_TagCode" = "(R) TAGCODE"),
                               relationship="many-to-one") 



#############################################################################################################################################################

#                                                                           X. EXPORT 


# ================== Export ================== 

# To github repo ---------------------------
writexl::write_xlsx(CN_headRcvyResults, 
                    path=paste0(here::here("outputs"), 
                                "/R_OUT - MRPHeadRecoveries_CHINOOK_WITH RESULTS_",
                                min(CN_headRcvyResults$`(R) SAMPLE YEAR`),
                                "-",
                                max(CN_headRcvyResults$`(R) SAMPLE YEAR`),
                                ".xlsx"))


# To DFO Network drive ---------------------------
writexl::write_xlsx(CN_headRcvyResults, 
                    path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/2-Export-from-R",
                                "/R_OUT - MRPHeadRecoveries_CHINOOK_WITH RESULTS_",
                                min(CN_headRcvyResults$`(R) SAMPLE YEAR`),
                                "-",
                                max(CN_headRcvyResults$`(R) SAMPLE YEAR`),
                                ".xlsx"))




# /END!
