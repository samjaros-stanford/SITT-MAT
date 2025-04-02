# Load utilities
source(here::here("code/__utils.R"))

############
# Settings #
############

# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   By default, use redcap API unless the variable is declared in the environment elsewhere
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"),get_raw_from_api,T)

##########
# Import #
##########

# IMAT all items
if(get_raw_from_api){
  sud_imat = get_redcap_report("SC", "119747")
  pc_imat = get_redcap_report("PC", "119745")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "IMAT - All Items" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format IMAT-all_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_imat = read_csv(sort(file_names[grepl("(?=.*IMAT-all)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_imat = read_csv(sort(file_names[grepl("(?=.*IMAT-all)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}


sud_imat$imat_count_4 = as.numeric(sud_imat$imat_count_4)
pc_imat$imat_count_4 = as.numeric(pc_imat$imat_count_4)

##########
# Import #
##########
raw_imat = pc_imat %>%
  bind_rows(sud_imat) %>%
  # Remove incomplete surveys (completion!=2)
  filter(imatpc_team_report_complete==2 | imatsc_team_report_complete==2) %>%
  # Get survey date from redcap event
  mutate(date = my(substr(redcap_event_name, 1, 8)),
         complete_display_date = if_else(is.na(imat_completion_date), 
                                         date, 
                                         (imat_completion_date))) %>%
  # Select only desired columns (no comment columns)
  select(program_id, date, starts_with("imat_d"), imat_total_mean, 
         -ends_with("_c"), complete_display_date, event_label = analysis_event_label_2) %>%
  # Remove rows where all data are NA
  filter(if_any(starts_with("imat"), ~!is.na(.x)))

#############
# All items #
#############
item_imat = raw_imat %>%
  select(-ends_with("_mean")) %>%
  pivot_longer(starts_with("imat_"),
               names_to="variable",
               values_to="value")

##########################
# Dimensions & Subscales #
##########################
subscale_imat = raw_imat %>%
  select(date, program_id, ends_with("_mean"), complete_display_date, event_label) %>%
  pivot_longer(ends_with("_mean"),
               names_pattern="(.*)_mean",
               names_to="variable",
               values_to="value") %>%
  # Define & merge in Low Barrier Care subscale
  rbind(raw_imat %>%
          rowwise() %>%
          mutate(imat_s1_mean = mean(c(imat_d3_11,imat_d3_12,imat_d3_14,imat_d4_9,
                                       #imat_d4_11, imat_d4_12, 
                                       imat_d5_5,imat_d5_6,imat_d6_1,imat_d6_2,imat_d7_3), na.rm=T)) %>%
          select(date, program_id, imat_s1_mean, complete_display_date, event_label) %>%
          pivot_longer(imat_s1_mean,
                       names_pattern="(.*)_mean",
                       names_to="variable",
                       values_to="value"))


##########
# Export #
##########
saveRDS(item_imat, "data/current_imat_item.rds")
saveRDS(subscale_imat, "data/current_imat_subscale.rds")

# Get data for report ==========================================================
imat_to_report = item_imat %>%
  mutate(imat_type = "raw_item") %>%
  bind_rows(subscale_imat %>%
              mutate(imat_type = "subscale_score")) %>%
  select(program_id, date, variable, value, imat_type, event_label) %>%
  arrange(program_id, variable, date)

write_csv(imat_to_report, file="data/SITTMAT_report_data/sittmat_imat.csv")
