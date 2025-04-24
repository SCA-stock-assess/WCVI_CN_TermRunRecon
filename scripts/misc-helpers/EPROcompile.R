# EPRO compile
# Compile multiple EPRO excel file outputs into one master file for Run Recon 
# KD Jan 2024



# Load packages ----------------
#library(here)
library(tidyverse)
#library(readxl)
#library(writexl)


#############################################################################################################################################################


# ==================== 1. LOAD EPRO BASE FILES (2022-2023 data) ==================== 


# Read EPRO files as large list ---------------------------
# !! before doing this you have to re-save the csvs as xlsx and change the tab name to match "All_Adult_Biosampling"
wcviEPRO.LL <- lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R", 
                                 pattern="^All_Adult_Biosampling_[0-9]*-[0-9a-zA-Z]*_AllStocks-AllSpecies_[0-9]{4}-[0-9]{4}_.*\\.xlsx$", full.names=T), 
                      function(x) {
                        readxl::read_excel(x, sheet="All_Adult_Biosampling", guess_max=20000)
                      })   

# Change filenames in the List:
names(wcviEPRO.LL) <- list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/1-Import-to-R", 
                                 pattern="^All_Adult_Biosampling_[0-9]*-[0-9a-zA-Z]*_AllStocks-AllSpecies_[0-9]{4}-[0-9]{4}_.*\\.xlsx$", full.names=F)


# Convert the Large List into a useable R dataframe ---------------------------
wcviEPRO <- do.call("rbind", wcviEPRO.LL) %>%
  tibble::rownames_to_column(var="file_source") %>%
  select_all(~stringr::str_to_title(.)) %>%
  select_all(~gsub("\\s+|\\.", ".", .)) %>%
  filter(Spawning.Stock.Name != "") %>%
  mutate(
    #across(everything(), parse_guess), # Automatically determine column classes based on values
    `(R) RETURN YEAR` = as.numeric(substr(Spawning.Stock.Name, 1,4)),
    `(R) OTOLITH BOX NUM` = Otolith.Bag.No,
    `(R) OTOLITH VIAL NUM` = Otolith.Vial.No,
    `(R) OTOLITH BOX-VIAL CONCAT` = case_when(!is.na(Otolith.Bag.No) & !is.na(Otolith.Vial.No) ~ paste0(Otolith.Bag.No,sep="-",Otolith.Vial.No)),
    `(R) SCALE BOOK NUM` = Scale.Book.No,
    `(R) SCALE CELL NUM` = Scale.Sample.No,
    `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(Scale.Book.No) & !is.na(Scale.Sample.No) ~ paste0(Scale.Book.No,sep="-",Scale.Sample.No)),
    `(R) DNA NUM` = Dna.Sample.No,
    `(R) TAGCODE` = Cwt.Tag.Code,
    `(R) HATCHCODE` = Otolith.Hatch.Code,
    `(R) TOTAL AGE: CWT` = case_when(!is.na(Cwt.Age.Yrs) ~ as.numeric(Cwt.Age.Yrs),
                                               TRUE ~ NA),
    `(R) TOTAL AGE: SCALE` = case_when(!is.na(Scale.Total.Age.Yrs) ~ as.numeric(Scale.Total.Age.Yrs), # Some entries for ttl age are "TRUE"(??)
                                                 Scale.Part.Age=="1M" ~ 2,
                                                 Scale.Part.Age=="2M" ~ 3,
                                                 Scale.Part.Age=="3M" ~ 4,
                                                 Scale.Part.Age=="4M" ~ 5,
                                                 Scale.Part.Age=="5M" ~ 6,
                                                 Scale.Part.Age=="6M" ~ 7,
                                                 T ~ NA_real_),
    `(R) TOTAL AGE: PBT` = case_when(Dam.Dna.Sample.Year %in% c(2000:3000) ~ `(R) RETURN YEAR`-as.numeric(Dam.Dna.Sample.Year),
                                     is.na(Dam.Dna.Sample.Year) & !is.na(Sire.Dna.Sample.Year) ~ `(R) RETURN YEAR`-as.numeric(Sire.Dna.Sample.Year)),
    `(R) BROOD YEAR: CWT` = `(R) RETURN YEAR` - `(R) TOTAL AGE: CWT`,
    `(R) BROOD YEAR: SCALE` = `(R) RETURN YEAR` - `(R) TOTAL AGE: SCALE`,
    `(R) BROOD YEAR: PBT` = case_when(Dam.Dna.Sample.Year %in% c(2000:3000) ~ as.numeric(Dam.Dna.Sample.Year),
                                      is.na(Dam.Dna.Sample.Year) & !is.na(Sire.Dna.Sample.Year) ~ as.numeric(Sire.Dna.Sample.Year)),
    `(R) RESOLVED BROOD YEAR` = case_when(!is.na(`(R) BROOD YEAR: CWT`) ~ `(R) BROOD YEAR: CWT`,
                                          is.na(`(R) BROOD YEAR: CWT`) & !is.na(`(R) BROOD YEAR: PBT`) ~ `(R) BROOD YEAR: PBT`,
                                          is.na(`(R) BROOD YEAR: CWT`) & is.na(`(R) BROOD YEAR: PBT`) & !is.na(`(R) BROOD YEAR: SCALE`) ~ `(R) BROOD YEAR: SCALE`,
                                          TRUE ~ NA)
    ) %>%
  filter(grepl("Chinook", Spawning.Stock.Name)) %>%
  distinct(across(Facility.Name:Ihn.Lab.Reading.Uom)) %>%
  print()

# Clean up ---------------------------
remove(wcviEPRO.LL)





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











