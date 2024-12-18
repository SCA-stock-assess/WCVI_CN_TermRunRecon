# EPRO compile
# Compile multiple EPRO excel file outputs into one master file for Run Recon 
# KD Jan 2024



# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(writexl)


#############################################################################################################################################################


# ==================== 1. LOAD EPRO BASE FILES (2022-2023 data) ==================== 


# Read OtoManager files as large list ---------------------------
# Load base files to compile
wcviCNepro.LL <- lapply(list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R", 
                                 pattern=".csv", full.names=T), 
                      function(x) {
                        read.csv(x)
                      })

# Change filenames in the List:
names(wcviCNepro.LL) <- list.files("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R", 
                                 pattern=".csv", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
wcviCNepro <- do.call("rbind", wcviCNepro.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  filter(Spawning.Stock != "") %>%
  mutate(
    #across(everything(), parse_guess), # Automatically determine column classes based on values
    `(R) RETURN YEAR` = as.numeric(substr(Spawning.Stock, 1,4)),
    `(R) OTOLITH BOX NUM` = Bag.No,
    `(R) OTOLITH VIAL NUM` = Vial.No,
    `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(Bag.No) & !is.na(Vial.No) ~ paste0(Bag.No,sep="-",Vial.No)),
    `(R) SCALE BOOK NUM` = Book.No,
    `(R) SCALE CELL NUM` = Scale.Sample.No,
    `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(Book.No) & !is.na(Scale.Sample.No) ~ paste0(Book.No,sep="-",Scale.Sample.No)),
    `(R) DNA NUM` = DNA.Sample.No,
    `(R) TAGCODE` = CWT.Tag.Code,
    `(R) HATCHCODE` = Hatch.Code,
    `(R) TOTAL AGE: CWT` = case_when(!is.na(CWT.Age..yrs.) ~ as.numeric(CWT.Age..yrs.),
                                               TRUE ~ NA),
    `(R) TOTAL AGE: SCALE` = case_when(!is.na(Scale.Total.Age..yrs.) ~ as.numeric(Scale.Total.Age..yrs.), # Some entries for ttl age are "TRUE"(??)
                                                 Scale.Part.Age=="1M" ~ 2,
                                                 Scale.Part.Age=="2M" ~ 3,
                                                 Scale.Part.Age=="3M" ~ 4,
                                                 Scale.Part.Age=="4M" ~ 5,
                                                 Scale.Part.Age=="5M" ~ 6,
                                                 Scale.Part.Age=="6M" ~ 7,
                                                 T ~ NA_real_),
    `(R) BROOD YEAR: CWT` = `(R) RETURN YEAR` - `(R) TOTAL AGE: CWT`,
    `(R) BROOD YEAR: SCALE` = `(R) RETURN YEAR` - `(R) TOTAL AGE: SCALE`,
    `(R) RESOLVED BROOD YEAR` = case_when(!is.na(`(R) BROOD YEAR: CWT`) ~ `(R) BROOD YEAR: CWT`,
                                          is.na(`(R) BROOD YEAR: CWT`) ~ `(R) BROOD YEAR: SCALE`,
                                          TRUE ~ NA)
    #UEID = paste0(analysis_year, "-", seq(1:nrow(.)))
    ) %>%
  filter(grepl("Chinook", Spawning.Stock)) %>%
  print()

# Clean up ---------------------------
remove(wcviCNepro.LL)





# ==================== 2. EXPORT ==================== 

# Not really needed - more useful with biodata results generated in 2-EPRO_biodata_with_results.R


# Export to Network ---------------------------
  # writexl::write_xlsx(wcviOtos, 
  #                     path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/PBS_SA_DFS$/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/RecoverySpecimens/Export",
  #                                 "/R_OUT - OtoManager_AllSpecies_Area20-27andOffshore",
  #                                 "_",
  #                                 min(wcviOtos$`(R) SAMPLE YEAR`),
  #                                 "-",
  #                                 max(wcviOtos$`(R) SAMPLE YEAR`),
  #                                 "_LastUpdate_",
  #                                 Sys.Date(),
  #                                 ".xlsx"))

# "Error: Error in libxlsxwriter: 'Error creating output xlsx file. Usually a permissions error.' "   --> means you have the file open











