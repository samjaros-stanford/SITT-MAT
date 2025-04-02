########################
# Accepts: REDCap CDI survey results
# Outputs: CDI scores to input into EMF google sheet
########################

# Load utilities
source(here::here("code/__utils.R"))

############
# Settings #
############

# How to deal with "Does not apply (8)"
dna = 4 #4 makes neutral, NA makes it missing
# Cutoff for neutral. 0 means only 4 is neutral. 1 means 3,4,5 are neutral.
neutral_cutoff = 0
# Output file naming prefix
output_prefix = paste0("CDI_",gsub("-","",as.character(today())),"_")
# Should the raw data be pulled from the API? If false, the file needs to be in the raw_data folder with proper naming
#   By default, use redcap API unless the variable is declared in the environment elsewhere
#   Set this value to FALSE to use local files
get_raw_from_api = ifelse(exists("get_raw_from_api"),get_raw_from_api,T)

##########
# Import #
##########

# Import scoring scheme
cdi_scoring = read.csv("public_data/cdi_scoring.csv", colClasses=c("q_num"="character"))

# CDI Survey
if(get_raw_from_api){
  sud_cdi = get_redcap_report("SC", "110001")
  pc_cdi = get_redcap_report("PC", "110006")
} else {
  # Get these files from REDCap and put them in the raw_data folder
  #   In REDCap, this is "CDI Scores" under "Reports" in the sidebar
  #   Click on "Export Data" > "CSV/Microsoft Excel (raw data)" > "Export Data" > File icon
  # Change the file name to match format CDI_[YYYYMMDD]_[PC|SUD].csv using the current date and the site type
  file_names = list.files(path="raw_data/", full.names=T)
  sud_cdi = read_csv(sort(file_names[grepl("(?=.*CDI)(?=.*SUD)", file_names, perl = TRUE)], decreasing=T)[1])
  pc_cdi = read_csv(sort(file_names[grepl("(?=.*CDI)(?=.*PC)", file_names, perl = TRUE)], decreasing=T)[1])
}

raw_cdi = bind_rows(mutate(sud_cdi, type="SUD"), 
                    mutate(pc_cdi, type="PC")) %>%
  # Remove sites that have withdrawn
  #filter(imp_support!=5) %>%
  # Remove test sties
  filter(!(program_id%in%c("id00","id50"))) %>%
  select(-imp_support)

###########################
# Get long & wide formats #
###########################

long_cdi = raw_cdi %>%
  select(program_id, redcap_event_name, type, starts_with("cdi_")) %>%
  pivot_longer(starts_with("cdi_") & !cdi_team_report_timestamp,
               names_to="item",
               values_to="value", values_drop_na=T) %>%
  # Bring in scoring information
  left_join(cdi_scoring, by="item") %>%
  # Put score in terms of centered around 0
  # Modify missing as needed
  # Reverse scales where needed
  mutate(centered_score = case_when(value==8  ~ dna-4, 
                                    isReverse ~ (value-4)*-1, 
                                    T         ~ value-4),
         score = centered_score+4,
         # Categorize into barriers, neutral, and facilitators
         cat = case_when(centered_score<neutral_cutoff*-1 ~ "barriers",
                         centered_score>neutral_cutoff    ~ "facilitators",
                         is.na(centered_score)            ~ NA_character_,
                         T                                ~ "neutral")) %>%
  mutate(date = fast_strptime(str_extract(redcap_event_name, 
                                          "^([a-z]*_[0-9]*)"), "%b_%Y"))

cdi_score = long_cdi %>%
  # Get mean score by subscale
  group_by(date, program_id, type, subscale) %>%
  summarize(value = mean(score)) %>%
  ungroup() %>%
  # Get subscales as columns
  pivot_wider(names_from = "subscale",
              values_from = "value") %>%
  # Add in total score
  full_join(long_cdi %>%
              # Get mean score for everything
              group_by(date, program_id, type) %>%
              summarize(cdi_9total = mean(score)) %>%
              ungroup(),
            join_by(date, program_id, type)) %>%
  # Add in level scores
  full_join(long_cdi %>%
              # Get just the level (patient, provider, inner, outer) from question
              mutate(level = str_extract(item, "^([a-z]+_[a-z]+)")) %>%
              # Get mean by level
              group_by(date, program_id, type, level) %>%
              summarize(value = mean(score)) %>%
              ungroup() %>%
              # Get levels as columns
              pivot_wider(names_from = "level",
                          values_from = "value"),
            join_by(date, program_id, type))

cdi_cat = long_cdi %>%
  # Count up barriers, facilitators, and neutral by subscale
  group_by(date, program_id, type, subscale) %>%
  summarize(barriers = sum(cat=="barriers"),
            neutral = sum(cat=="neutral"),
            facilitators = sum(cat=="facilitators")) %>%
  ungroup() %>%
  # Count up barriers, facilitators, and neutral by level
  bind_rows(long_cdi %>%
              # Get level name (named subscale for binding)
              mutate(subscale = str_extract(item, "^([a-z]+_[a-z]+)")) %>%
              group_by(date, program_id, type, subscale) %>%
              summarize(barriers = sum(cat=="barriers"),
                        neutral = sum(cat=="neutral"),
                        facilitators = sum(cat=="facilitators")) %>%
              ungroup()) %>%
  # Count up barriers, facilitators, and neutral by site
  bind_rows(long_cdi %>%
              group_by(date, program_id, type) %>%
              summarize(barriers = sum(cat=="barriers"),
                        neutral = sum(cat=="neutral"),
                        facilitators = sum(cat=="facilitators")) %>%
              ungroup() %>%
              # Add dummy subscale for total
              mutate(subscale="cdi_9total")) %>%
  arrange(program_id, date, subscale)

##########
# Export #
##########

write.csv(cdi_score, file=paste0("data/", output_prefix, "score.csv"), row.names=F)
write.csv(cdi_cat, file=paste0("data/", output_prefix, "cat.csv"), row.names=F)
saveRDS(long_cdi, file="data/current_long_cdi.rds")

# Get data for report ==========================================================
cdi_to_report = long_cdi %>%
  # Individual items
  select(program_id, date, item, value) %>%
  mutate(cdi_type = "raw_item") %>%
  # Subscores
  bind_rows(cdi_score %>%
              pivot_longer(cols = starts_with("cdi_"), 
                           names_to = "item",
                           values_to = "value") %>%
              select(program_id, date, item, value) %>%
              mutate(cdi_type = "scale_score")) %>%
  # Barriers/Neutral/Facilitators
  bind_rows(cdi_cat %>%
              pivot_longer(cols = c("barriers", "neutral", "facilitators"),
                           names_to = "affect",
                           values_to = "value") %>%
              mutate(item = paste0(subscale, "_", affect),
                     cdi_type = "scale_bnf") %>%
              select(program_id, date, item, value, cdi_type)) %>%
  rename(variable = item) %>%
  arrange(program_id, variable, date)

write_csv(cdi_to_report, file="data/SITTMAT_report_data/sittmat_cdi.csv")  
