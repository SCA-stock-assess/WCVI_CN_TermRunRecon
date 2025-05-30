# pullNusedsData.R 
# based on saaWeb package by N. Komick
# Feb 2024


# Load library -----------------------
library(saaWeb)




# Define function to extract NuSEDS data -----------------------
pullNusedsData <- function (query_doc, config_file = "saaWeb.config", user_name = Sys.getenv("username"), 
                            password = NULL) 
{
  config_list <- saaWeb:::loadConfigFile(config_file)
  extractor_usage_url <- config_list$NusedsExtractorUsageUrl
  extractor_query_url <- config_list$NusedsExtractorQueryUrl
  query_result <- saaWeb:::runExtractorQuery(query_doc, extractor_usage_url, 
                                             extractor_query_url, user_name, password)
  return(query_result)
}

