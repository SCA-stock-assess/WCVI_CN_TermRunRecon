
# Dump and combine age data 


# Load libraries ------------------------
library(saaWeb)
library(tidyverse)
library(writexl)
library(here)

# Helper ------------------------
"%notin%" <- Negate("%in%")





# ============================ 1: MRP AGES (~2020-present) ============================

# 1.1. Dump MRP age batch metadata ------------------------
  # This output has broad metadata like river etc., but no results
SC_ageBatchMeta.MRP <- getAgeBatchList() %>% 
  filter(Sector=="SC", Species=="Chinook") %>% 
  mutate_at("Id", as.character)


# 1.2. Dump MRP age results ------------------------ (slow)
  # This output has the age results, but can't be traced back to specific river etc. without joining to dataframe above
SC_scaleAges.MRP <- getAgeBatchScaleResults(c(SC_ageBatchMeta.MRP$Id)) %>% 
  filter(Species=="Chinook") %>%
  rename(SampleYear=RecoveryYear) %>% 
  mutate_at("SampleYear", as.numeric) %>% 
  filter_all(any_vars(!is.na(.)))


# 1.3. Join MRP batch metadata to results ------------------------
intersect(colnames(SC_ageBatchMeta.MRP), colnames(SC_scaleAges.MRP))

SC_scaleAgesMeta.MRP <- left_join(SC_scaleAges.MRP, SC_ageBatchMeta.MRP) %>%
  filter(#RecoveryYear %in% analysis_year,      
         Area%in%c(20:27, 121:127), Species=="Chinook") %>%
  filter(!grepl("Georgia Str|Sooke", ProjectName)) %>%
  setNames(paste0('PADS_', names(.))) %>%
  mutate(`(R) SCALE BOOK NUM` = PADS_ContainerId,
         `(R) SCALE CELL NUM` = PADS_FishNumber,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) SCALE BOOK NUM`) & !is.na(`(R) SCALE CELL NUM`) ~
                                                    paste0(`(R) SCALE BOOK NUM`,sep="-",`(R) SCALE CELL NUM`)),
         `(R) scale data source` = "MRP") %>%
  rename(`(R) SAMPLE YEAR` = PADS_SampleYear) %>%
  select(PADS_Species, `(R) SAMPLE YEAR`, PADS_LifeHistory, PADS_Region, PADS_Area, PADS_ProjectName, PADS_Location, PADS_GearMrpName,
         PADS_EuAge, PADS_GrAge, PADS_FishEdge, PADS_ScaleCondition, PADS_ContainerId, PADS_FishNumber, `(R) SCALE BOOK NUM`, `(R) SCALE CELL NUM`, 
         `(R) SCALE BOOK-CELL CONCAT`, `(R) scale data source`, PADS_Id, PADS_Structure) %>%
  mutate_at(c("(R) SCALE CELL NUM","(R) SAMPLE YEAR"), as.character) %>%
  mutate_at("(R) SAMPLE YEAR", as.numeric) %>%
  print()





# ============================ 2: NuSEDS AGES (2012-2021) ============================

# 3.1. Define NuSEDS query function (not yet in saaWeb) ------------------------
runNuSEDSQuery <- function (query_doc, config_file = "saaWeb.config", user_name = Sys.getenv("username"), password = NULL) 
{
  config_list <- saaWeb:::loadConfigFile(config_file)
  nuseds_usage_url <- config_list$NusedsExtractorUsageUrl
  nuseds_query_url <- config_list$NusedsExtractorQueryUrl
  query_result <- saaWeb:::runExtractorQuery(query_doc, nuseds_usage_url, 
                                             nuseds_query_url, user_name, password)
  return(query_result)
}


# 3.2. Dump NuSEDS ages ------------------------  ***VERRYYYY slow never do it again unless you HAVE to*** 
  # Query below for reference, should not need to be run ever again
  # ages.NuSEDS <- runNuSEDSQuery(here("scripts", "json", "nuseds_ages_CN_multi-yr.json"))


# 3.2.1. Export because SO SLOW and these shouldn't change anymore ------------------------
  # writexl::write_xlsx(ages.NuSEDS, here("outputs", paste0("R_OUT - NuSEDS Age results ", min(ages.NuSEDS$`Fiscal Year`), "-", max(ages.NuSEDS$`Fiscal Year`), ".xlsx")))


# 3.3. Load historical NuSEDS data dump (slow) ------------------------
SC_ages.NuSEDS <- read.csv(here("outputs", "R_OUT - NuSEDS Age results 2012-2021.csv")) %>%
  select(Fiscal.Year, Project, Location, Species, Sample.Source, Gear.Code, Container.Label, Container.Address, Sample.Number, Sample.Start.Date,
         Sample.End.Date, Part.Age.Code, GR.Age, EU.Age) %>%
  setNames(paste0('PADS_', names(.))) %>%
  rename(`(R) SAMPLE YEAR` = PADS_Fiscal.Year,
         `(R) SCALE BOOK NUM` = PADS_Container.Label,
         `(R) SCALE CELL NUM` = PADS_Container.Address,
         PADS_ProjectName = PADS_Project,
         PADS_Location = PADS_Location,
         PADS_Species = PADS_Species,
         PADS_GearMrpName = PADS_Gear.Code,
         PADS_CntStartDate = PADS_Sample.Start.Date,
         PADS_CntEndDate = PADS_Sample.End.Date,
         PADS_ScaleCondition = PADS_Part.Age.Code,
         PADS_GrAge = PADS_GR.Age,
         PADS_EuAge = PADS_EU.Age) %>%
  mutate(`(R) scale data source` = "NuSEDs",
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) SCALE BOOK NUM`) & !is.na(`(R) SCALE CELL NUM`) ~ paste0(`(R) SCALE BOOK NUM`, sep="-",
                                                                                                                       `(R) SCALE CELL NUM`)),
         PADS_CntStartDate = lubridate::ymd(PADS_CntStartDate),
         PADS_CntEndDate = lubridate::ymd(PADS_CntEndDate)) %>%
  filter(PADS_Sample.Source %in% c("ESCAPEMENT", "ESCAPEMENT - SURPLUS SPAWNING", "FIRST NATIONS SAMPLE", "NATIVE FOOD FISHERY", "MIXED", "UNKNOWN CATCH",
                                     "HATCHERY"),
         !grepl("(FRASER)|(ATNARKO)|(BABINE)|(BC INTERIOR)|(BELLA COOLA)|(BIG BAR)|(CHEHALIS)|(CHILKO)|(CHILLIWACK)|(DEAN)|(DOCEE)|(HARRISON)|(KILBELLA)|
                (KITIMAT)|(KITSUMKALUM)|(KITWANGA)|(KLUKSHU)|(CHILCOTIN)|(SHUSWAP)|(MEZIADIN)|(NECHAKO)|(NICOLA)|(SNOOTLI)|(SPIUS)|(TATSAMENIE)|(TENDERFOOT)|
                (THOMPSON)|(NASS)|(SKEENA)|(STIKINE)|(WANNOCK)|(WHITEHORSE)|(YUKON)", PADS_ProjectName)) %>%
  mutate_at(c("PADS_GearMrpName", "(R) SCALE CELL NUM"), as.character) %>%
  print()





# ============================ 3: NuSEDS + MRP ages ============================

# Join all ages for complete dataset (2012-present) ------------------------
intersect(colnames(SC_ages.NuSEDS), colnames(SC_scaleAgesMeta.MRP))

SC_allAgesMaster <- full_join(SC_scaleAgesMeta.MRP %>% 
                                # currently exclude the years from NuSEDS as the MRP database is in a state of transitioning the data over and is sometimes incomplete for those historical years
                                filter(`(R) SAMPLE YEAR` %notin% SC_ages.NuSEDS$`(R) SAMPLE YEAR`),
                              SC_ages.NuSEDS) %>%
  arrange(`(R) SAMPLE YEAR`)
 



# ============================ 4: EXPORT ============================

# 4.1. Export to StA drive, WCVI Term Run folder ------------------------
writexl::write_xlsx(SC_allAgesMaster, paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/R_OUT - ALL South Coast Age results ", min(SC_allAgesMaster$`(R) SAMPLE YEAR`), "-", max(SC_allAgesMaster$`(R) SAMPLE YEAR`), ".xlsx"))



# 4.2. Export to github repo ------------------------
writexl::write_xlsx(SC_allAgesMaster, here("outputs", 
                                           paste0("R_OUT - ALL South Coast Age results ", min(SC_allAgesMaster$`(R) SAMPLE YEAR`), "-", max(SC_allAgesMaster$`(R) SAMPLE YEAR`), ".xlsx")))








