
# Load utilities
source(here::here("code/__utils.R"))

# Settings #####################################################################
# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   By default, use redcap API unless the variable is declared in the environment elsewhere
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"),get_raw_from_api,T)

# Import #######################################################################
# REAIM Outcomes
if(get_raw_from_api){
  sud_reaim = get_redcap_report("SC", "131201")
  pc_reaim = get_redcap_report("PC", "131199")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "Program Measure RE-AIM" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format REAIM_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_reaim = read_csv(sort(file_names[grepl("(?=.*REAIM)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_reaim = read_csv(sort(file_names[grepl("(?=.*REAIM)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}

# Process & Format Data ########################################################
# Get data in long format for easier processing/stacking
# Flip PC & SUD files separately to avoid future issues with columns not matching
flip_reaim = function(df) {
  df %>%
    # Keep rows that have any REAIM data
    filter(if_any(starts_with("reaim_"), ~!(.x))) %>%
    # Keep program id, event name (that has the date), and all REAIM cols
    select(program_id, redcap_event_name, starts_with("reaim_")) %>%
    # Flip to long
    pivot_longer(starts_with("reaim_"),
                 names_to = "reaim_var",
                 values_to = "value")
}

long_reaim = flip_reaim(sud_reaim) %>%
  bind_rows(flip_reaim(pc_reaim)) %>%
  # Get survey date from redcap_event_name
  mutate(survey_date = str_extract(redcap_event_name, "[a-z]{3}_[0-9]{4}")) %>%
  # Get month from reaim variable
  separate_wider_regex(reaim_var, c("variable" = ".*", "_",
                                    "month_num" = ".*")) %>%
  # Calculate actual month for each row
  # The month number indicates the month in the previous quarter
  # So reaim_a1_01 in dec_2022 is September 2022 data for REAIM A1
  mutate(date = parse_date_time(survey_date, "b_Y") - months(4 - as.numeric(month_num))) %>%
  # Select desired columns
  select(date, program_id, variable, value)

### Output #####################################################################
saveRDS(long_reaim, "data/current_reaim.rds")
