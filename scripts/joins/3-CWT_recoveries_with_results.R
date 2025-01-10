
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
# library(readxl)
# library(writexl)
# library(openxlsx)
# library(saaWeb)


# Helpers -------------
"%notin%" <- Negate("%in%")




################################################################################################################################################

#                                                                           I. Compile individual year files


# Option 1:   Run compile file (annual or semi-annual, not required every time) ------------------
# source(here::here("scripts", "misc-helpers", "HeadRcvyCompile.R"))
  # saves as mrpHeadRcvy


# Option 2:   Load already compiled file from above ------------------
mrpHeadRcvy <- lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/HeadRcvyCompile_base-files/1-Import-to-R", 
                                 pattern="R_OUT - MRPHeadRecoveries_CHINOOK_.*\\.xlsx$", full.names=T), 
                      function(x) {
                        read.csv(x)
                      })

# Load file as a tibble
# head_rec <- list.files(here::here("outputs"), pattern = "(?i)headrecoveries", full.names = T) %>%
#   str_subset("(?i)chinook") %>% # The Chinook data (as opposed to all species)
#   read_excel




################################################################################################################################################

#                                                                           II. CWT Releases Load


# Load files from local repo -------------


# Load file as a tibble
cwt_rel <- list.files(here("outputs"), pattern = "(?i)release.*tagcodes", full.names = T) %>%
  str_subset("(?i)chinook") %>% # The Chinook data (as opposed to all species)
  read_excel |> 
  rename_with(~str_replace_all(.x, "MRP", "MRP_REL")) # Clarify which columns come from the release data



#############################################################################################################################################################

#                                                                           III. Join releases to recoveries


# Join EPRO master file to NPAFC master mark file ---------------------------
intersect(colnames(head_rec), colnames(cwt_rel))

wcviCNheads_w_cwt <- left_join(head_rec,
                               cwt_rel,
                               by=c("MRP_TagCode" = "MRP_REL_Tagcode"),
                               relationship="many-to-one") |> 
  select(-`MRP_REL_Species Name`, -`MRP_REL_Recovery Years`) # Remove some columns from the releases that are redundant with the recovery information



#############################################################################################################################################################

#                                                                           IV. Save the output


# Export joined data file to .xslx --------------------------- 

write_xlsx(
  wcviCNheads_w_cwt,
  path = here(
    "outputs", 
    paste0(
      "R_OUT - MRPHeadRecoveries_CHINOOK_WITH RESULTS_2012-2023_LastUpdate_",
      Sys.Date(),
      ".xlsx"
    )
  )
)


# /END!