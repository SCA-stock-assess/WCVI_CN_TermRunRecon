
# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "readxl", "writexl", "here", "janitor")
#install.packages(pkgs)

# Load packages
library(tidyverse); library(readxl); library(here); library(janitor); library(writexl)



# Load data dump file -----------------------------------------------------

# Load and clean column names
rr_data <- here(
  "outputs", 
  "R_OUT - WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_AND TERM GROUPINGS 2017-2023.xlsx"
  ) |> 
  read_xlsx(sheet = "WCVI CN CREST Biodata CODED") |> 
  clean_names()



# Deal with the sport data ------------------------------------------------


# Need to apportion data into chunks that align with the run reconstruction files
# Focusing only on the TermOTHER file for now
termOTHER_sport <- rr_data |> 
  filter(
    area %in% c("20", "120", "24", "124", "26", "126", "27", "127"),
    sample_type == "Sport"
  ) |> 
  mutate(
    program = str_replace_all(program, "/", " "),
    sport_area = case_when(
      subarea %in% c("20A", "20E", "20B", "20-1", "20-2", "20-3", "Area 20 (West)") ~ "Recreational Port Renfrew",
      subarea %in% c("24S", "24R", "24T", "24U", "24E", "24Q", "24F", "24D", "24N", "24-4") ~ "Recreational Inner Clayoquot",
      subarea %in% c("24L", "24J", "24P", "24-8") ~ "Recreational Outer Clayoquot",
      subarea %in% c("24O", "24M", "24K", "24I", "124O", "124M", "124K", "124V") ~ "Recreational Clayoquot Corridor",
      subarea %in% c("26G", "26A", "26J", "Area 26") ~ "Recreational Outer Kyuquot",
      subarea %in% c("26L", "26M", "26K", "126M", "126N", "126P") ~ "Recreational Kyuquot Corridor",
      subarea %in% c("27G", "27H", "27I", "27J", "27K") ~ "Recreational Inner Quatsino",
      subarea %in% c("27A", "27B", "27D") ~ "Recreational Outer Quatsino",
      subarea %in% c("27L", "27M", "127M", "127N") ~ "Recreational Quatsino Corridor",
      TRUE ~ NA_character_
    ),
    month_stratum = case_when(
      month %in% c("June", "July") ~ "June-July",
      month %in% c("August", "September") ~ "August-September",
      TRUE ~ NA_character_
    )
  ) |> 
  nest(.by = c(year, program))


# Case study to figure out how to build the correct tables
test <- termOTHER_sport[1,] |> 
  unnest(cols = data)

mk_rr_matrix <- function(data) {
  data |> 
    filter(!if_any(c(sport_area, resolved_age, r_term_sum, month_stratum), is.na)) |> 
    count(sport_area, month_stratum, resolved_age, r_term_sum) |> 
    complete(
      sport_area, month_stratum, resolved_age, r_term_sum, 
      fill = list(n = 0)
    ) |> 
    mutate(
      .by = c(sport_area, month_stratum),
      ttl = sum(n),
      prop = if_else(ttl == 0, 0, n/ttl)
    ) |> 
    select(-ttl, -n) |> 
    pivot_wider(
      names_from = resolved_age,
      values_from = prop,
      values_fill = 0
    )
}

mk_rr_matrix(test)


# Apply to all data chunks and save as list
termOTHER_matrices <- termOTHER_sport |> 
  mutate(
    data = map(data, mk_rr_matrix),
    name = paste0(year, "_", program)
  ) |> 
  select(data, name) |> 
  unnest(data) %>% 
  split(.$name) |> 
  map(~select(.data = .x, -name))



# Export to excel ---------------------------------------------------------


# Save the data
write_xlsx(
  x = termOTHER_matrices,
  path = here(
    "outputs",
    "R_OUT - Term file matrices by stock and age for TermOTHER sport 2017-2023.xlsx"
  )
)


