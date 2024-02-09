
# Join CWT release data to Chinook head recovery numbers (currently, MRP only outputs by CWT code, not head tag number)

# Work flow is:
# 1.   Import CWT recovery data from local repo "outputs"
# 2.   Import CWT release data from local repo "outputs" 
# 3.   Smash them together
# 4.   Save the resulting output file


################################################################################################################################################
################################################################################################################################################

# BEFORE YOU START: 
# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


# Define analysis year:
analysis_year <- 2023



################################################################################################################################################
################################################################################################################################################

# Now should be able to high light and run all! 


# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(saaWeb)


# Helpers -------------
"%notin%" <- Negate("%in%")




################################################################################################################################################

#                                                                           I. Head Recoveries Load 


# Load files from local repo -------------


# Load file as a tibble
head_rec <- list.files(here("outputs"), pattern = "(?i)headrecoveries", full.names = T) %>%
  str_subset("(?i)chinook") %>% # The Chinook data (as opposed to all species)
  read_excel




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