library(lubridate)
library(stringr)
library(tidyverse)

############
# Settings #
############
# SUD Patient registry
SUD_registry_file = "raw_data/Registry_20230201_SUD.csv"
# PC Patient registry
PC_registry_file = "raw_data/Registry_20230201_PC.csv"

####################
# Load & Prep Data #
####################
input_cleanup = function(file_name, type){
  read.csv(file_name) %>%
    mutate(site_type = type) %>%
    # Fill in site participation data for every entry
    group_by(program_id) %>%
    fill(imp_support, .direction="down") %>%
    # Remove sites no longer in study
    filter(imp_support != 5) %>%
    # Get survey month
    rowwise() %>%
    mutate(survey_month = my(str_extract(redcap_event_name, "^[A-Za-z]+_[0-9]+"))) %>%
    # Only want complete surveys
    filter(quarterly_moud_service_registry_complete==2) %>%
    # Force blanks to be NA's
    mutate(across(starts_with("pr_"), na_if, "")) 
}

raw_data = rbind(input_cleanup(SUD_registry_file, "SUD"),
                 input_cleanup(PC_registry_file, "PC"))

#####################
# Adoption Measures #
#####################

provider_data = raw_data %>%
  select(program_id, survey_month, starts_with("pr_moud_prescriber")) %>%
  # Get provider name, month, and relationship columns
  pivot_longer(cols = c(-program_id, -survey_month),
               names_to = ".value",
               names_pattern = "pr_moud_([a-z_]+)_[0-9]",
               values_transform = list(prescriber = as.character,
                                       prescriber_mo = paste),
               values_drop_na = T) %>%
  # Sites without providers excluded
  filter(!is.na(prescriber)) %>%
  # Get monthly rows
  separate_rows(prescriber_mo, sep = ",", convert = T) %>%
  # Calculate observation month for each row relative to the survey month
  mutate(month_diff = (prescriber_mo-2)*-1+2,
         month = survey_month-months(month_diff)) %>%
  # Get only relevant cols
  select(program_id, month, prescriber, prescriber_relat)

# A1: Number of prescribers employed or contracted by the agency who have 
#     prescribed medications for opioid use disorder (MOUD) to at least 1 
#     patient in the past month.
A1 = provider_data %>%
  #  TEMP: Temporarily removed filter while sites are inputting new data
  #  filter(prescriber_relat==1) %>%
  group_by(program_id, month) %>%
  summarise(A1 = n(),
            .groups = "keep")

# A2: Number of prescribers who are NOT employed or contracted by the agency who 
#     have prescribed medications for opioid use disorder (MOUD) to at least 1 
#     patient in the past month.
A2 = provider_data %>%
  filter(prescriber_relat==2) %>%
  group_by(program_id, month) %>%
  summarise(A2 = n(),
            .groups = "keep")

