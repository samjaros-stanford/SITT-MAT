# TODO: Program info file needs to be rewritten with better readability


# Load utilities
source(here::here("code/__utils.R"))

############
# Settings #
############

# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   By default, use redcap API unless the variable is declared in the environment elsewhere
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"), get_raw_from_api, T)

##########
# Import #
##########

# Raw program info
if(get_raw_from_api){
  sud_info = get_redcap_report("SC", "112702")
  pc_info = get_redcap_report("PC", "112703")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "Program Information" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format INFO_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_info = read_csv(sort(file_names[grepl("(?=.*INFO)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_info = read_csv(sort(file_names[grepl("(?=.*INFO)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}

#######################
# Load & Process Data #
#######################
clean_SUD = sud_info %>% 
  filter(if_any(starts_with("demo"), ~!is.na(.x))) %>%
  mutate(site_type = "SUD",
         demo_level_of_care = as.character(demo_level_of_care),
         isOutpatient = grepl("1", demo_level_of_care) | grepl("2", demo_level_of_care),
         isInpatient = grepl("3", demo_level_of_care) | grepl("4", demo_level_of_care) | grepl("5", demo_level_of_care),
         care_level = case_when(
           isOutpatient & isInpatient ~ "Both",
           isOutpatient               ~ "Outpatient",
           isInpatient                ~ "Inpatient",
           NA ~ NA_character_
         ),
         demo_goal = factor(demo_goal, levels=c(1,2), labels=c("Start MOUD", "Expand MOUD")))

# Assumes all primary cares are outpatient
clean_PC = pc_info %>% 
  filter(if_any(starts_with("demo"), ~!is.na(.x))) %>%
  mutate(site_type="PC",
         isOutpatient = T,
         isInpatient = F,
         care_level = case_when(
           isOutpatient & isInpatient ~ "Both",
           isOutpatient               ~ "Outpatient",
           isInpatient                ~ "Inpatient",
           NA ~ NA_character_
         ),
         demo_goal = factor(demo_goal, levels=c(1,2), labels=c("Start MOUD", "Expand MOUD")))

##########
# Export #
##########

saveRDS(bind_rows(clean_SUD, clean_PC) %>%
          filter(imp_support != 5) %>%
          select(program_id, demo_goal, site_type, care_level), 
        file="data/study_program_info.rds")

saveRDS(bind_rows(clean_SUD, clean_PC) %>%
          select(program_id, demo_goal, site_type, care_level), 
        file="data/all_program_info.rds")
