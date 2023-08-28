# Function for querying MRP database of CWT recoveries, releases, etc.
# http://pac-salmon.dfo-mpo.gc.ca/DataExtractor/
# Code from N. Komick 
# June 2023


# Dependencies: ---------------------------
library(tidyverse)
library(httr)           # for authenticate()
#library(askpass)       # for askpass() -- retired because does not work with rmarkdown
library(getPass)        # replace askpass() with getPass()



# ========================= DEFINE EXTRACTION FUNCTION =========================

getCWTData <- function(query_doc, password = NULL) {
  extractor_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.DataExtractor.v2/api/DynamicQuery/QueryResult"
  if(file.exists(query_doc)) {
    query_file <- file(query_doc, "r")
    query_doc <- readLines(query_file)
    close(query_file)
  }
  user_name <- Sys.getenv("username")
  if(is.null(password)) {
    password <- getPass(paste0("Please enter your password for ",
                               user_name,
                               ":"))
  }
  data_response <-
    POST(extractor_url,
         authenticate(user_name, password, "ntlm"),
         encode = "json",
         content_type_json(),
         body = query_doc)
  cwt_data <- content(data_response)
  cwt_col_names <- unlist(cwt_data$Names)
  extractor_data <-
    lapply(cwt_data$Rows,
           function(.) {
             as_tibble(.$Cells, .name_repair = ~ cwt_col_names)
           }) %>%
    bind_rows()
  col_types <- unlist(cwt_data$Types)
  int_col <- cwt_col_names[grepl("int", col_types, ignore.case = TRUE)]
  dbl_col <- cwt_col_names[grepl("single", col_types, ignore.case = TRUE)]
  extractor_data <-
    extractor_data %>%
    mutate(across(all_of(int_col), as.integer)) %>%
    mutate(across(all_of(dbl_col), as.double))
  return(extractor_data)
}
