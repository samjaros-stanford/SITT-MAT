########################
# Accepts: REDCap Baseline RE-AIM & IMAT outcome reports
# Outputs: Long form of baseline RE-AIM & IMAT outcomes
########################

# Load utilities
source(here::here("code/__utils.R"))

############
# Settings #
############

# Output
output_file = paste0("data/baseline_EMF_",gsub("-","",as.character(today())),".csv") # Path to output .csv
# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   Use the value if this has been set elsewhere. Otherwise, use the API
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"),get_raw_from_api,T)

##########
# Import #
##########

if(get_raw_from_api){
  sud_imat = get_redcap_report("SC", "119747")
  sud_reaim = get_redcap_report("SC", "107003")
  pc_imat = get_redcap_report("PC", "119745")
  pc_reaim = get_redcap_report("PC", "103592")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "Baseline RE-AIM" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format REAIM_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_imat = read_csv(sort(file_names[grepl("(?=.*IMAT)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  sud_reaim = read_csv(sort(file_names[grepl("(?=.*REAIM)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1], na=c("","NA","na","."))
  pc_imat = read_csv(sort(file_names[grepl("(?=.*IMAT)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_reaim = read_csv(sort(file_names[grepl("(?=.*REAIM)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}

##########
# RE-AIM #
##########

# Import and stack raw data
raw_reaim = bind_rows(mutate(sud_reaim, type="SUD"), 
                      mutate(pc_reaim, type="PC")) %>%
  filter(imp_support!=5)

# Get long data
long_reaim = raw_reaim %>%
  select(program_id, type, starts_with("demo_moud")) %>% # Remove all the columns RedCap adds
  # Get 1 row per site per month per variable
  pivot_longer(starts_with("demo_moud_"),
               names_to=c("variable", "month"), names_prefix="demo_moud_", names_sep="_",
               values_transform=as.numeric, values_drop_na=T) %>%
  # Get variables as columns for calculating b4p
  pivot_wider(names_from=variable,
              values_from=value) %>%
  # Flip b1 and b2
  rename(b1=b2,
         b2=b1) %>%
  # Calculate new b4 and b4p
  mutate(
    b4 = if_else(is.na(b4) & is.na(b3),
                 NA_real_,
                 if_else(is.na(b4),0,b4) + if_else(is.na(b3),0,b3)),
    b4p = case_when(
      b1==0 ~ NA_real_,
      b4>b1 ~ 100,
      T     ~ b4/b1*100)) %>%
  # Get 1 row per site per month per variable
  pivot_longer(starts_with(c("a","b")),
               names_to="variable",
               values_to="value", values_drop_na=F) %>%
  mutate(variable = paste0("reaim_", variable)) %>%
  mutate(date = fast_strptime(month, "%b%Y"))

reaim_data = long_reaim %>%
  select(date, program_id, variable, value, type)

########
# IMAT #
########

sud_imat$imat_count_4 = as.numeric(sud_imat$imat_count_4)
pc_imat$imat_count_4 = as.numeric(pc_imat$imat_count_4)

# Import and stack raw data
raw_imat = bind_rows(mutate(sud_imat, type="SUD"), 
                     mutate(pc_imat, type="PC")) %>%
  filter(imp_support!=5 & redcap_event_name=="sep_2022_arm_1")

# Get long data
long_imat = raw_imat %>%
  select(program_id, type, redcap_event_name, ends_with("_mean")) %>%
  pivot_longer(c(-program_id, -redcap_event_name, -type),
               names_to="variable", names_pattern="(.*)_mean$",
               values_transform=list(char_value=as.character), values_to="char_value") %>%
  filter(!is.na(char_value)) %>%
  mutate(value = as.numeric(char_value)) %>%
  mutate(date = fast_strptime(redcap_event_name, "%b_%Y_arm_1"))

imat_data = long_imat %>%
  select(date, program_id, variable, value, type)

##########
# Export #
##########

reaim_imat = rbind(reaim_data, imat_data)
write.csv(reaim_imat, file=output_file, row.names=F)
saveRDS(reaim_imat, file="data/current_baseline_reaim-imat.rds")

