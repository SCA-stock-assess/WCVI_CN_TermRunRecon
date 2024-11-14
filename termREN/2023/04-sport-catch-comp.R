# termREN
# 04-terminal-catch-composition
# nov 2024


# ============================= SET UP  ============================
# Load high-use packages -------------------------------
library(tidyverse)

# Helpers -------------------------------
full_age_range <- tibble(RESOLVED_AGE = c(2:6)) 
full_month_range <- tibble(MONTH=c("June", "July", "August", "September"))
"%notin%" <- Negate("%in%")
options(scipen=9999)
analysis_year <- 2023



# ============================= LOAD DATA =============================

# Read mapping file -------------------------------
RENmap03 <- readxl::read_excel(path=paste0(here::here("termREN"), "/", analysis_year, "/", 
                                           list.files(path=paste0(here::here("termREN"), "/", analysis_year),
                                                      pattern="^R_OUT - TERMREN_mapping_[0-9]{4}-output_from_03\\.xlsx$",
                                                      full.names=F)),   
                               sheet="Sheet1")


# WCVI rec catch biodata (output from CRESTcompile.R) -------------------------------
# If you get std:bad_alloc, you have too many windows open. Your system memory usage needs to be at about 70% or less. Open task manager.
# SCrecBio <- readxl::read_excel(path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/",
#                                            list.files(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/",
#                                                       "^R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS [0-9]{4}-[0-9]{4}.xlsx$")),
#                                sheet="WCVI CN CREST Biodata CODED")

# READ FROM DESKTOP FOR NOW WHILE ON VPN:
SCrecBio <- readxl::read_excel(path=list.files(path="C:/Users/DAVIDSONKA/Desktop",
                                               pattern="^R_OUT - Biological_Data_with_Results AND TERM GROUPINGS \\d{4}-\\d{4}.xlsx$",
                                               full.names=T),
                               sheet="Biological_Data_With_GROUPED")


########################################################################################################################################################

#                                                                SUMMARIZE stock comps 


#  ========================= FINE SCALE stock composition =========================
# For supplementary information to print for background, not to join to mapping file. 

# All term run grouping levels -------------------------------
a20recCompFS <- full_join(
  SCrecBio %>%
    filter(AREA=="20", SAMPLE_TYPE=="Sport", SUBAREA %in% c("20A", "20B", "20E", "20-1", "20-3", "Area 20 (West)"), !is.na(RESOLVED_AGE),
           DISPOSITION=="Kept") %>%
    group_by(YEAR, SUBAREA, MONTH, RESOLVED_AGE, `(R) TERM GROUP03`, `(R) TERM GROUP02`, `(R) TERM GROUP01`) %>%
    summarize(n=n()),
  full_age_range) %>%
  print()


# Export data table to supplement folder -------------------------------
writexl::write_xlsx(a20recCompFS %>%
                      arrange(RESOLVED_AGE) %>%
                      pivot_wider(names_from = RESOLVED_AGE, values_from = n, names_prefix = "n_age_") %>%
                      mutate(across(c(n_age_2:n_age_6), ~case_when(is.na(.)~0,
                                                                   TRUE~.))), 
                    path=paste0(here::here("termREN"), "/", analysis_year, "/", "supplementary",
                                "/R_OUT - Recreational fishery fine-scale stock composition (Terminal Renfrew areas) ",
                                analysis_year,
                                "-output_from_04",
                                ".xlsx"))


#  ========================= COURSE SCALE stock composition =========================
# For mapping file
# Recall sample rate is extremely low for Area 20, so samples will be pooled across sub-areas and months within a year


# Roll-up term run grouping levels -------------------------------
a20recCompCS <- full_join(
  SCrecBio %>%
    filter(AREA=="20", SAMPLE_TYPE=="Sport", SUBAREA %in% c("20A", "20B", "20E", "20-1", "20-3", "Area 20 (West)"), !is.na(RESOLVED_AGE),
           DISPOSITION=="Kept", MONTH%in%c("June", "July", "August", "September")) %>%
    group_by(YEAR, RESOLVED_AGE, `(R) TERM GROUP02`) %>%
    summarize(n=n()),
  full_age_range) %>%
  filter(!grepl("Unknown", `(R) TERM GROUP02`)) %>%
  group_by(YEAR, RESOLVED_AGE) %>%
  mutate(sample_size = sum(n),
         propn = n/sample_size) %>%
  group_by(YEAR) %>% 
  mutate(annual_sample_size = sum(n)) %>%
  arrange(RESOLVED_AGE) 



#  ========================= VISUALIZE: SAN JUAN =========================
# To potentially pool multiple years together

# Visualize stock comp - does it change BY YEAR? -------------------------------
ggplot(data=full_join(
  SCrecBio %>%
    filter(AREA=="20", SAMPLE_TYPE=="Sport", SUBAREA %in% c("20A", "20B", "20E", "20-1", "20-3", "Area 20 (West)"), !is.na(RESOLVED_AGE),
           DISPOSITION=="Kept", MONTH%in%c("June", "July", "August", "September")) %>%
    group_by(YEAR, RESOLVED_AGE, `(R) TERM GROUP03`) %>%
    summarize(n=n()),
  full_age_range) %>%
    filter(!grepl("Unknown", `(R) TERM GROUP03`)) %>%
    group_by(YEAR, RESOLVED_AGE) %>%
    mutate(sample_size = sum(n),
           propn = n/sample_size) %>%
    group_by(YEAR) %>% 
    mutate(annual_sample_size = sum(n)) %>%
    arrange(RESOLVED_AGE) %>%
    filter(!is.na(`(R) TERM GROUP03`)) %>%
    mutate(outline_group = case_when(grepl("Natural", `(R) TERM GROUP03`, ignore.case=T) ~ "natural",
                                     TRUE ~ "not"))) +
  geom_bar(aes(y=propn, x=RESOLVED_AGE, fill=`(R) TERM GROUP03`, colour=`(R) TERM GROUP03`, linetype=outline_group), 
           stat="identity", position="stack", alpha=0.9, linewidth=1) +
  geom_text(aes(x=RESOLVED_AGE, y=1.05, label=paste0("n (age) = ", sample_size)), size=3.5) +
  scale_x_continuous(breaks=seq(0,3000,by=1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray80", "#99c7fd", "#fdd067", "gray60", "#56A3FD", "#FCB203")) +
  scale_color_manual(values=c("gray80", "#99c7fd", "#fdd067", "black", "black", "black")) +
  scale_linetype_manual(values=c("solid", "blank"), guide="none") +
  labs(x="Total age", y="Proportion of Area 20 terminal sport samples", fill="Run reconstruction group\n(Level 3)", 
       colour="Run reconstruction group\n(Level 3)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold")) +
  facet_wrap(~YEAR) 



# Visualize stock comp - does it change BY AGE? -------------------------------
ggplot(data=full_join(
  SCrecBio %>%
    filter(AREA=="20", SAMPLE_TYPE=="Sport", SUBAREA %in% c("20A", "20B", "20E", "20-1", "20-3", "Area 20 (West)"), !is.na(RESOLVED_AGE),
           DISPOSITION=="Kept", MONTH%in%c("June", "July", "August", "September")) %>%
    group_by(YEAR, RESOLVED_AGE, `(R) TERM GROUP03`) %>%
    summarize(n=n()),
  full_age_range) %>%
    filter(!grepl("Unknown", `(R) TERM GROUP03`)) %>%
    group_by(YEAR, RESOLVED_AGE) %>%
    mutate(sample_size = sum(n),
           propn = n/sample_size) %>%
    group_by(YEAR) %>% 
    mutate(annual_sample_size = sum(n)) %>%
    arrange(RESOLVED_AGE) %>%
    filter(!is.na(`(R) TERM GROUP03`)) %>%
    mutate(outline_group = case_when(grepl("Natural", `(R) TERM GROUP03`, ignore.case=T) ~ "natural",
                                     TRUE ~ "not"))) +
  geom_bar(aes(y=propn, x=YEAR, fill=`(R) TERM GROUP03`, colour=`(R) TERM GROUP03`, linetype=outline_group), 
           stat="identity", position="stack", alpha=0.9, size=1) +
  geom_text(aes(x=YEAR, y=1.05, label=paste0("n (year) = ", annual_sample_size)), size=4) +
  scale_x_continuous(breaks=seq(0,3000,by=1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray80", "#99c7fd", "#fdd067", "gray60", "#56A3FD", "#FCB203")) +
  scale_color_manual(values=c("gray80", "#99c7fd", "#fdd067", "black", "black", "black")) +
  scale_linetype_manual(values=c("solid", "blank"), guide=F) +
  labs(x="", y="Proportion of Area 20 terminal sport samples", fill="Run reconstruction group\n(Level 3)", 
       colour="Run reconstruction group\n(Level 3)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold")) +
  facet_wrap(~RESOLVED_AGE) 






### join to mapping
a20recCompCS%>%
  pivot_wider(names_from = RESOLVED_AGE, values_from = c(n, sample_size, propn), names_prefix="age_") %>%
  arrange(YEAR) %>%
  group_by(YEAR) %>% 
  fill(c(sample_size_age_2:sample_size_age_6), .direction = "updown") %>%
  mutate(across(c(n_age_2:propn_age_6), ~case_when(is.na(.)~0,
                                                   TRUE~.))) %>%
  print()





