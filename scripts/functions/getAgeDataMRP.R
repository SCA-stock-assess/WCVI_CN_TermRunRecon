# Functions for querying new PADS database hosted by MRP
# http://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/AgeBatchList
# Code from N. Komick/M. O'Brien/N. Brown
# May 2023


# Dependencies: ---------------------------
library(tidyverse)
library(httr)           # for authenticate()
#library(askpass)       # for askpass() -- retired because doesn't work with rmarkdown
library(getPass)        # replace askpass() with getPass()



# Multiple functions are needed to access different parts of the PADS database right now. They produce different dataframes that are eventually joined in a separate script. 



# ========================= DEFINE EXTRACTION FUNCTIONS =========================

# Credentials to access "online" DFO databases ---------------------------
auth <- authenticate(user = Sys.info()["login"], 
                     password = getPass::getPass(),              # will prompt you to enter your DFO network password once for the session.
                     type = 'ntlm')



# Function for getting container metadata (gives 'CtnStartDate' field) ---------------------------
get_container_meta <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/odata/GetBatchContainers')
  request_body <- paste0(r"({"batch_id":")", batch_id, r"("})")
  x <- httr::POST(url, auth, body=request_body, encode="json", httr::verbose(), httr::content_type_json())
  y <-
    jsonlite::fromJSON(httr::content(x, "text"))$value |>
    dplyr::as_tibble() #|>
    #dplyr::mutate(#across(c(FieldContainerId, ContainerNotes, AnalystName), as.character),
                   #across(c(CntStartDate, CntEndDate), lubridate::as_date))
  return(y)
}



# Function for getting batch metadata (gives 'Location' field) ---------------------------
get_batch_meta <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/AgeBatchDetail/GetAgeBatchDetail/', batch_id)
  x <- httr::GET(url, auth)
  batch_df <-
    httr::content(x, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  url <- 
    paste0("http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/odata/Project?$filter=Id eq ", batch_id) |>
    URLencode()
  y <- 
    httr::GET(url, auth) %>%
    httr::content(., "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() 
  y <- 
    select(y$value, Id, Location) %>%
    inner_join(batch_df, by="Id")
  return(y)
}



# Function for extracting batch age results ---------------------------
get_age_data <- function(batch_id){
  url <- paste0('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/AgeBatchDetail/ScaleAgeDetail/', 
                batch_id)
  x <- httr::GET(url, auth)
  y <- batch_df_sc <- jsonlite::fromJSON(httr::content(x, "text"))
  return(y)
}




# ========================= EXTRACT DATA =========================

# Extract all South Coast scale batch IDs for SampleYear of interest and store in a vector of IDs called "PADS_batch_ids" ---------------------------
## If this gives you trouble, re-start R session, clear Environment and try again...
PADS_batch_ids <- httr::GET('http://pac-salmon.dfo-mpo.gc.ca/Api.CwtDataEntry.v2/api/Report/GetAgeBatchList',
                                 auth) %>%
  # Get list of available age result batches:
  httr::content(x = .,  'text') %>%
  jsonlite::fromJSON() %>%
  # Filter batch list to keep only South Coast samples:
  filter(Sector == "SC",
         # --- If query is too slow, you can add a filter for year(s) here: --- (NOT RUN)
         SampleYear%in%c(2012:analysis_year),
         Structure=="Scales"
         ) %>%
  pull(Id)           # Extract column of age batch Ids from resulting dataframe



# Extract batch metadata, container metadata, and age data for scales in PADS_batch_ids; Join into 1 dataframe ---------------------------
#   intermediate table names are ignored for now, but can be run to help with diagnosing code issues if needed

mrpPADS <- 
  full_join(
    # Batch metadata ---
    # PADS_batch_meta <- 
    PADS_batch_ids %>%
      purrr::map_dfr(get_batch_meta) %>%
      rename(BatchId = Id), 
    # Container metadata ---
    # PADS_container_meta <- 
    PADS_batch_ids %>%
      purrr::map_dfr(get_container_meta) %>%
      rename(ContainerId = Id) %>%
      mutate(across(c(CntStartDate, CntEndDate), lubridate::as_date)), 
    by="BatchId") %>%
  full_join(.,
            # Age data ---
            # PADS_age_data <- 
            PADS_batch_ids %>%
              purrr::map_dfr(get_age_data) %>%
              rename(BatchId = Id),
            by=c("BatchId", "ProjectName", "Region", "Area", "Species", "RecoveryYear", "LifeHistory", "ContainerId")) %>%    # DO NOT join by all fields as there are entry inconsistencies, e.g., "ProjectPriority==H vs ==High")
  select(-c(AgingStructure)) %>%       # remove duplicate column 
  print()




# ========================= QC =========================

# Quick QC to make sure PADS join worked ---------------------------
nrow(PADS_batch_ids %>%
       purrr::map_dfr(get_age_data) %>%
       rename(BatchId = Id))
nrow(mrpPADS)
# There will likely be a difference in the number of data rows in the Join vs. the number that have been aged - this is likely because the lab hasn't completed all aging yet. 


# Do the math to confirm that the unaged samples account for the difference (roughly); should result in TRUE: 
# (total # samples in PADS join) - (# unaged samples in the PADS join) == (total # samples available) 
nrow(mrpPADS) - mrpPADS%>%filter(is.na(GrAge)&is.na(ScaleCondition))%>%summarize(n=n())%>%pull(n) == nrow(PADS_batch_ids %>%
                                                                                                            purrr::map_dfr(get_age_data) %>%
                                                                                                            rename(BatchId = Id))

# NOTE: there are sometimes a handful of issues, e.g., coho in a sockeye test fishery that won't always match up properly 
