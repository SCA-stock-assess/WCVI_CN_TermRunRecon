# crestCompile2 - out of TERM RUN 
# for BDWR compilation separate from term run (just here for record keeping for now)
# March 2024



# Load packages ---------------------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)



# Helpers ---------------------------
"%notin%" <- Negate("%in%")




#############################################################################################################################################################


# ==================== 1. LOAD CREST CHINOOK BIODATA BASE FILES (2021-present data) ==================== 


# Read CREST files as large list ---------------------------
# Load base files to compile
crestBio.LL <- lapply(list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/1-Import-to-R", 
                                 pattern="^[^~]*_Biological_Data_With_Results*.*xlsx", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="Biological_Data_With", guess_max=20000)
                      })

# Change filenames in the List:
names(crestBio.LL) <- list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/1-Import-to-R", 
                                 pattern="^[^~]*_Biological_Data_With_Results*.*xlsx", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
crestBio <- do.call("rbind", crestBio.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  print()


# Clean up ---------------------------
remove(crestBio.LL)


#############################################################################################################################################################

#                                                                           ADD VARIABLES

crestBiocompiled <- crestBio %>% 
  mutate(`(R) Origin` = case_when(HATCHERY_ORIGIN=="Y" | PBT_BROOD_YEAR > 2000 ~ "Hatchery",
                                  THERMALMARK=="Not Marked" ~ "Natural (assumed)",
                                  TRUE ~ "Unknown"),
         RESOLVED_ID_ORIGIN = paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ORIGIN),
         RESOVLED_REGION_ORIGIN = paste0(`(R) Origin`, sep=" ", RESOLVED_STOCK_ROLLUP)) %>% 
  print()



#############################################################################################################################################################

#                                                                           EXPORT 


# ==================== CREATE EXCEL FILE ==================== 

# Create Workbook ---------------------------
R_OUT_CREST.Bio <- openxlsx::createWorkbook()

# Add tabs to Workbook ---------------------------
openxlsx::addWorksheet(R_OUT_CREST.Bio, "CREST Biodata Compiled")



# Add data to tabs ---------------------------
openxlsx::writeData(R_OUT_CREST.Bio, sheet="CREST Biodata Compiled", x=crestBiocompiled)





# ==================== EXPORT EXCEL FILE ==================== 

# To github ---------------------------
openxlsx::saveWorkbook(R_OUT_CREST.Bio,
                       file=paste0(here("outputs"),
                                   "/R_OUT - Biological_Data_With_Results ",
                                   min(crestBiocompiled$YEAR),
                                   "-",
                                   max(crestBiocompiled$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)


# To Network: 
openxlsx::saveWorkbook(R_OUT_CREST.Bio, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CRESTcompile_base-files/2-Export-from-R", 
                                   "/R_OUT - Biological_Data_With_Results ",
                                   min(crestBiocompiled$YEAR),
                                   "-",
                                   max(crestBiocompiled$YEAR),
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)



































