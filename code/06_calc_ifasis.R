
# Load utilities
source(here::here("code/__utils.R"))

############
# Settings #
############
# Output file naming prefix
output_prefix = paste0("IFASIS_",gsub("-","",as.character(today())),"_")
# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   By default, use redcap API unless the variable is declared in the environment elsewhere
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"),get_raw_from_api,T)

##########
# Import #
##########

# CDI Survey
if(get_raw_from_api){
  sud_ifasis = get_redcap_report("SC", "151246")
  pc_ifasis = get_redcap_report("PC", "151245")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "[API] IFASIS All Items" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format IFASIS_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_cdi = read_csv(sort(file_names[grepl("(?=.*IFASIS)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_cdi = read_csv(sort(file_names[grepl("(?=.*IFASIS)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}

raw_ifasis = bind_rows(mutate(sud_ifasis, type="SUD"), 
                    mutate(pc_ifasis, type="PC")) %>%
  # Remove rows that contain no data
  filter(if_any(starts_with("ifasis_"), ~!is.na(.x))) %>%
  # Get only complete surveys
  filter(facilitators_barriers_inventory_team_report_complete == 2) %>%
  select(-facilitators_barriers_inventory_team_report_complete) %>%
  # Remove test sties
  filter(!(program_id %in% c("id00","id50"))) %>%
  # Extract date from redcap event
  mutate(date = fast_strptime(str_extract(redcap_event_name, 
                                          "^([a-z]*_[0-9]*)"), "%b_%Y")) %>%
  select(-starts_with("redcap_"), -ifasis_completion_date)

###########################
# Get long & wide formats #
###########################

# Get data in long format
ifasis_score = raw_ifasis %>%
  # Means have subscales
  select(-ends_with("_mean")) %>%
  # Rename so that value columns end in "_value"
  rename_with(~sub("(.*[0-9])$", "\\1_value", .)) %>%
  # Get value and importance column for each item
  pivot_longer(cols = starts_with("ifasis_"),
               names_pattern = "(.*)_(value|imp)",
               names_to = c("variable", ".value")) %>%
  mutate(ifasis_type = "raw_item") %>%
  select(program_id, date, variable, value, importance = imp, ifasis_type)

ifasis_subscale = raw_ifasis %>%
  # Get only means (subscales)
  select(program_id, date, ends_with("_mean")) %>%
  # Get value for each mean
  pivot_longer(cols = starts_with("ifasis_"),
               names_to = "variable",
               values_to = "value") %>%
  mutate(ifasis_type = "scale_score")

##########
# Export #
##########

write_csv(ifasis_score, file=paste0("data/",output_prefix,"score.csv"))
write_csv(ifasis_subscale, file=paste0("data/",output_prefix,"subscale.csv"))

# Get data for report ==========================================================
ifasis_to_report = ifasis_score %>%
  bind_rows(ifasis_subscale) %>%
  arrange(program_id, variable, date)

write_csv(ifasis_to_report, file="data/SITTMAT_report_data/sittmat_ifasis.csv")  
