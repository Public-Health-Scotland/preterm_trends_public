# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(janitor)

# multiples ----
source(paste0(here::here(), "/00_setup.R"))

multiples_list <-
  multiples_path   %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = multiples_path, skip = 3)


# initial data clean ------------------------------------------------------

# cleans up the column names of each of the list elements
multiples_list <- multiples_list %>%
  map(~ rename_with(.x, .cols = where(is.numeric), ~ glue::glue("year_{.}"))) %>% #prefix with year - clean_names on numeric names prefixes with "x"
  map(~ clean_names(.))

names(multiples_list)
# cleans up the list element names
names(multiples_list) <- make_clean_names(names(multiples_list))

# remove unneeded list elements
multiples_list["onset_algorithm"] <- NULL
multiples_list["ethnicity_code_map"] <- NULL


# further data cleaning ---------------------------------------------------

# need to fill down the age categories
# does this for every column that starts with "gestation" in all of the lists
# and add sinlgeton flag and remove the extra rows generated due to comments on lines below data ine xcel
multiples_list_filled <-
  multiples_list %>%
  map(~ fill(.x, starts_with("gestation"))) %>%  # fill in the blanks in the first column where excel  had grouped cells
  map(~ mutate(.x,single_or_multi= "multiples")) %>% # add a column labelling data as singleton (safer for later processing!)
  map(~ filter(.x,!is.na(.x[2])))  #%>% # remove rows with NA in the second column.
# map(~ str_replace(.x, "[^Unk]", "Unknown")) #this doesnt work with the multiples for unknown reasons


# wide to long --------------%>% # remove rows with NA in the second column.

# want long df with variables  Gestation/demographic or other variable / Year / N
names(multiples_list_filled)
long_gest_wks <- multiples_list_filled[[1]] %>%
  pivot_longer(cols = -c( "gestation_at_delivery_completed_weeks", "single_or_multi"), names_to = "year", values_to = "N") %>%
  mutate(year =  str_remove(year, "year_")) %>%
  mutate(across(where(is_character), ~ case_when(
    str_detect(.x, "^Unk") ~ "Unknown",
    TRUE ~ .x
  ))) %>%
  mutate(preterm_category = case_when(
    gestation_at_delivery_completed_weeks <=27 ~ "Extremely Preterm",
    gestation_at_delivery_completed_weeks %in% 28:31 ~ "Very Preterm",
    gestation_at_delivery_completed_weeks %in% 32:36 ~ "Moderate-Late Preterm",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown",
    TRUE ~ "Non Preterm"
  )) %>%
  mutate(birth_category = case_when(
    gestation_at_delivery_completed_weeks <= 36 ~ "Preterm",
    gestation_at_delivery_completed_weeks  %in% 37:41 ~ "Term",
    gestation_at_delivery_completed_weeks  %in% 42:44 ~ "Post-term",
    gestation_at_delivery_completed_weeks  %in% "Unknown" ~ "Unknown"
  ))


long_by_onset <- multiples_list_filled[[2]] %>%
  pivot_longer(cols = -c( "gestation_at_delivery_completed_weeks", "onset_of_delivery", "single_or_multi"), names_to = "year", values_to = "N") %>%
  mutate(year =  str_remove(year, "year_")) %>%
  mutate(across(where(is_character), ~ case_when(
    str_detect(.x, "^Unk") ~ "Unknown",
    TRUE ~ .x
  )))%>%mutate(preterm_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" ~ "Extremely Preterm",
    gestation_at_delivery_completed_weeks == "28-31" ~ "Very Preterm",
    gestation_at_delivery_completed_weeks == "32-36" ~ "Moderate-Late Preterm",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown",
    TRUE ~ "Non Preterm"
  )) %>%
  mutate(birth_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" | gestation_at_delivery_completed_weeks == "28-31" |
      gestation_at_delivery_completed_weeks == "32-36" ~ "Preterm",
    gestation_at_delivery_completed_weeks == "37-41" ~ "Term",
    gestation_at_delivery_completed_weeks == "42-44" ~ "Post-term",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown"
  ))



long_mat_age <- multiples_list_filled[[3]] %>%
  pivot_longer(cols = -c( "gestation_at_delivery_completed_weeks", "maternal_age_at_delivery_years", "single_or_multi"), names_to = "year", values_to = "N")%>%
  mutate(year =  str_remove(year, "year_")) %>%
  mutate(across(where(is_character), ~ case_when(
    str_detect(.x, "^Unk") ~ "Unknown",
    TRUE ~ .x
  )))%>%mutate(preterm_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" ~ "Extremely Preterm",
    gestation_at_delivery_completed_weeks == "28-31" ~ "Very Preterm",
    gestation_at_delivery_completed_weeks == "32-36" ~ "Moderate-Late Preterm",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown",
    TRUE ~ "Non Preterm"
  )) %>%
  mutate(birth_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" | gestation_at_delivery_completed_weeks == "28-31" |
      gestation_at_delivery_completed_weeks == "32-36" ~ "Preterm",
    gestation_at_delivery_completed_weeks == "37-41" ~ "Term",
    gestation_at_delivery_completed_weeks == "42-44" ~ "Post-term",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown"
  ))



long_mat_simd  <- multiples_list_filled[[4]] %>%
  pivot_longer(cols = -c( "gestation_at_delivery_completed_weeks",  "maternal_simd_quintile_from_postcode_of_residence", "single_or_multi"), names_to = "year", values_to = "N")%>%
  mutate(year =  str_remove(year, "year_")) %>%
  mutate(across(where(is_character), ~ case_when(
    str_detect(.x, "^Unk") ~ "Unknown",
    TRUE ~ .x
  )))%>%mutate(preterm_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" ~ "Extremely Preterm",
    gestation_at_delivery_completed_weeks == "28-31" ~ "Very Preterm",
    gestation_at_delivery_completed_weeks == "32-36" ~ "Moderate-Late Preterm",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown",
    TRUE ~ "Non Preterm"
  )) %>%
  mutate(birth_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" | gestation_at_delivery_completed_weeks == "28-31" |
      gestation_at_delivery_completed_weeks == "32-36" ~ "Preterm",
    gestation_at_delivery_completed_weeks == "37-41" ~ "Term",
    gestation_at_delivery_completed_weeks == "42-44" ~ "Post-term",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown"
  ))




long_mat_ethnicity   <- multiples_list_filled[[5]] %>%
  pivot_longer(cols = -c("gestation_at_delivery_completed_weeks", "maternal_ethnicity_from_smr02_delivery_record", "single_or_multi"), names_to = "year_group", values_to = "N")%>%
  mutate(year_group =  str_remove(year_group, "year_")) %>%
  mutate(across(where(is_character), ~ case_when(
    str_detect(.x, "^Unk") ~ "Unknown",
    TRUE ~ .x
  )))%>%mutate(preterm_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" ~ "Extremely Preterm",
    gestation_at_delivery_completed_weeks == "28-31" ~ "Very Preterm",
    gestation_at_delivery_completed_weeks == "32-36" ~ "Moderate-Late Preterm",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown",
    TRUE ~ "Non Preterm"
  )) %>%
  mutate(birth_category = case_when(
    gestation_at_delivery_completed_weeks == "22-27" | gestation_at_delivery_completed_weeks == "28-31" |
      gestation_at_delivery_completed_weeks == "32-36" ~ "Preterm",
    gestation_at_delivery_completed_weeks == "37-41" ~ "Term",
    gestation_at_delivery_completed_weeks == "42-44" ~ "Post-term",
    gestation_at_delivery_completed_weeks == "Unknown" ~ "Unknown"
  ))




# save as RDS ----
saveRDS(long_gest_wks, paste0(data_path, "2_working_data/gest_wks_multi.rds"))
saveRDS(long_by_onset, paste0(data_path, "2_working_data/gest_by_onset_multi.rds"))
saveRDS(long_mat_age, paste0(data_path, "2_working_data/gest_by_age_multi.rds"))
saveRDS(long_mat_simd, paste0(data_path, "2_working_data/gest_by_simd_multi.rds"))
saveRDS(long_mat_ethnicity, paste0(data_path, "2_working_data/gest_by_ethn_multi.rds"))
