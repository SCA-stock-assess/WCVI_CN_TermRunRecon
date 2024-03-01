# CRESTcompile
# compile CREST biodata outputs for CN Run Recon  **assumes everything is from the "WCVI CHINOOK ONLY biodata..." report
# Then assign term rollup stock groups
# Feb 2024





# Load packages ----------------
#library(here)
#library(tidyverse)
#library(readxl)
#library(writexl)


#############################################################################################################################################################


# ==================== 1. LOAD CREST CHINOOK BIODATA BASE FILES (2021-present data) ==================== 


# Read CREST files as large list ---------------------------
# Load base files to compile
crestBio.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CrestCompile_base-files/1-Import-to-R", 
                                 pattern="^2023_WCVI_*.*xlsx", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="WCVI_Chinook_Run_Rec", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBio.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CrestCompile_base-files/1-Import-to-R", 
                                 pattern="^2023_WCVI_*.*xlsx", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
crestBio <- do.call("rbind", crestBio.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()


# Clean up ---------------------------
remove(crestBio.LL)




#############################################################################################################################################################

#                                                                           I. BUILD STREAM AUX FILE 






#############################################################################################################################################################

# ==================== 2. CODE TERM RUN GROUPS ==================== 














