library(gt)
library(stringr)
library(tidyverse)
library(webshot2)

############
# Settings #
############
# Long CDI location
cdi_long_file = "data/current_cdi_item.rds"
# Include "Does not apply (8)" in the mean calculation?
#   NOTE: Barriers/facilitators calculation is unaffected
dna = NA_real_ #4 makes neutral, NA_real_ doesn't include it in the mean

########
# Data #
########

### Get mean Likert value
# Long data from 00_calc_cdi.R
cdi = readRDS(cdi_long_file) %>%
  # Rescale for purposes of the table
  mutate(scaled_value = if_else(value==8, dna, scaled_value + 4)) %>%
  # Get levels column
  mutate(level = sapply(strsplit(item, "_"), "[", 2))

# Rows are questions, columns are mean value & site type
question_value = cdi %>%
  # Get mean Likert for each question/type combination but keep subscale around
  group_by(type, subscale, q_num) %>%
  summarize(mean_v = mean(scaled_value, na.rm=T),
            .groups="keep") %>%
  ungroup() %>%
  # Get copies of columns for SUD/PC
  pivot_wider(names_from = type,
              names_glue = "{type}_mean_v",
              values_from = mean_v) %>%
  # Join in "all" data
  left_join(cdi %>% group_by(q_num) %>%
              summarize(all_mean_v = mean(scaled_value, na.rm=T), .groups = "keep"),
            by="q_num")

# Rows are question subgroups, columns are combinations of mean value & site type
subscale_value = cdi %>%
  # Get mean Likert for each type/subscale combination
  group_by(type, subscale) %>%
  summarize(mean_v = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  # Get copies of columns for SUD/PC
  pivot_wider(names_from = type,
              names_glue = "{type}_mean_v",
              values_from = mean_v) %>%
  # Join in "all" data
  left_join(cdi %>% group_by(subscale) %>%
              summarize(all_mean_v = mean(scaled_value, na.rm=T), .groups = "keep"),
            by="subscale")

# Rows are level subgroups, columns are combinations of mean value & site type
level_value = cdi %>%
  # Get mean Likert value for each type/level combination
  group_by(type, level) %>%
  summarize(mean_v = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  # Get copies of columns for SUD/PC
  pivot_wider(names_from = type,
              names_glue = "{type}_mean_v",
              values_from = mean_v) %>%
  # Join in "all" data
  left_join(cdi %>% group_by(level) %>%
              summarize(all_mean_v = mean(scaled_value, na.rm=T), .groups = "keep"),
            by="level")

total_value = cdi %>%
  # Get mean Likert for each type combination
  group_by(type) %>%
  summarize(mean_v = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  # Get copies of columns for SUD/PC
  pivot_wider(names_from = type,
              names_glue = "{type}_mean_v",
              values_from = mean_v) %>%
  # Join in "all" data
  cbind(cdi %>% group_by() %>%
          summarize(all_mean_v = mean(scaled_value, na.rm=T), .groups = "keep"))

### Get mean percent barriers/facilitators
cdi_bf = cdi %>%
  # Prep data for counting of facilitators/neutral/barriers
  # Extract "level" from each question name
  select(program_id, type, item, subscale, cat) %>%
  mutate(total = 1,
         level = sapply(strsplit(item, "_"), "[", 2)) %>%
  select(-item)

subscale_bf_data = cdi_bf %>%
  # Get total of each outcome category for each site for each subscale
  group_by(program_id, type, subscale, cat) %>%
  summarize(total = sum(total),
            .groups = "keep") %>%
  # Get the percent of each outcome category
  group_by(program_id, type, subscale) %>%
  mutate(percent = total/sum(total)) %>%
  filter(cat != "neutral") %>% # Not interested in neutral for this figure
  # Get outcome categories as columns
  pivot_wider(id_cols = c(program_id, type, subscale),
              names_from = cat,
              names_glue = "{cat}_{.value}",
              values_from = percent,
              values_fill = 0) %>%
  ungroup() %>%
  select(-program_id)

subscale_bf = subscale_bf_data %>%
  # Get mean percent for each subscale/type combination
  group_by(type, subscale) %>%
  summarize(across(.cols=everything(), .fns=mean),
            .groups = "keep") %>%
  # Get site types across columns
  pivot_wider(names_from = type,
              names_glue = "{type}_{.value}",
              values_from = contains("percent")) %>%
  # Add in "all" site type columns
  left_join(subscale_bf_data %>% select(-type) %>% group_by(subscale) %>%
              summarize(across(.cols=everything(), .fns=mean, .names = "all_{.col}"),
                        .groups = "keep"),
            by="subscale")

level_bf_data = cdi_bf %>%
  # Get total of each outcome category for each site for each level
  group_by(program_id, type, level, cat) %>%
  summarize(total = sum(total),
            .groups = "keep") %>%
  # Get percent of each outcome category
  group_by(program_id, type, level) %>%
  mutate(percent = total/sum(total)) %>%
  filter(cat != "neutral") %>% # Neutral not needed for this figure
  # Get outcome categories as columns
  pivot_wider(id_cols = c(program_id, type, level),
              names_from = cat,
              names_glue = "{cat}_{.value}",
              values_from = percent,
              values_fill = 0) %>%
  ungroup() %>%
  select(-program_id)

level_bf = level_bf_data %>%
  # Get mean percent of each type/level combination
  group_by(type, level) %>%
  summarize(across(.fns=mean),
            .groups = "keep") %>%
  # Get site types across the columns
  pivot_wider(names_from = type,
              names_glue = "{type}_{.value}",
              values_from = contains("percent")) %>%
  # Add in "all" site type columns
  left_join(level_bf_data %>% select(-type) %>% group_by(level) %>%
              summarize(across(.cols=everything(), .fns=mean, .names="all_{.col}"),
                        .groups = "keep"),
            by="level")

total_bf = level_bf_data %>%
  select(-level) %>%
  # Get mean percent of each type/level combination
  group_by(type) %>%
  summarize(across(.fns=mean),
            .groups = "keep") %>%
  # Get site types across the columns
  pivot_wider(names_from = type,
              names_glue = "{type}_{.value}",
              values_from = contains("percent")) %>%
  # Add in "all" site type columns
  cbind(level_bf_data %>% select(-type, -level) %>% group_by() %>%
          summarize(across(.cols=everything(), .fns=mean, .names = "all_{.col}"),
                    .groups = "keep"))
  
  
##################
# Table creation #
##################


# Get N's for heading
PC_n = nrow(cdi %>% filter(type=="PC") %>% select(program_id) %>% distinct())
SUD_n = nrow(cdi %>% filter(type=="SUD") %>% select(program_id) %>% distinct())
all_n = nrow(cdi %>% select(program_id) %>% distinct())


# Formatted variable names as they should be displayed
formatted_names = tribble(
  ~var_name, ~fmt_name,
  "cdi_1eep", "Enabiling External Policies",
  "cdi_2cs",  "Community Support",
  "cdi_3ls",  "Leadership Support",
  "cdi_4he",  "Health Equity",
  "cdi_5spm", "Staff Perception of MOUD",
  "cdi_6fea", "Feasibility",
  "cdi_7rtc", "Readiness to Change",
  "cdi_8ic",  "Individualized Care",
  "outer",    "System Level",
  "inner",    "Clinic/Program Level",
  "staff",    "Staff Level",
  "patient",  "Patient Level",
  "total",    "Total"
)


# Column naming
formatted_columns = list(
  SUD_mean_v               = "Likert Score", 
  SUD_barriers_percent     = "% Barriers",
  SUD_facilitators_percent = "% Facilitators",
  PC_mean_v                = "Likert Score",
  PC_barriers_percent      = "% Barriers",
  PC_facilitators_percent  = "% Facilitators",
  all_mean_v               = "Likert Score",
  all_barriers_percent     = "% Barriers",
  all_facilitators_percent = "% Facilitators"
)

### Question table
question_value %>%
  mutate(q_num = str_pad(as.character(q_num), 2, "left", "0")) %>%
  rbind(mutate(subscale_value, q_num="Subgroup Mean")) %>%
  left_join(formatted_names, by=c("subscale"="var_name")) %>%
  arrange(subscale, q_num) %>%
  select(-subscale) %>%
  gt(rowname_col = "q_num",
     groupname_col = "fmt_name") %>%
  cols_move(columns = c(PC_mean_v, all_mean_v),
            after = SUD_mean_v) %>%
  tab_spanner(label = md(paste0("**SUD Programs (n=", SUD_n, ")**")),
              id = "sud_programs",
              columns = starts_with("SUD")) %>%
  tab_spanner(label = md(paste0("**Primary Care Clinics (n=", PC_n, ")**")),
              id = "pc_programs",
              columns = starts_with("PC")) %>%
  tab_spanner(label = md(paste0("**All Sites (n=", all_n, ")**")),
              id = "all_programs",
              columns = starts_with("all")) %>%
  cols_label(PC_mean_v = "Mean Likert Score",
             SUD_mean_v = "Mean Likert Score",
             all_mean_v = "Mean Likert Score") %>%
  tab_stubhead("Question Number") %>%
  fmt_number(ends_with("_mean_v")) %>%
  fmt_percent(ends_with("_percent")) %>%
  cols_width(~px(215)) %>%
  gtsave("figures/question_cdi_table.html")
  

### Subscale table
left_join(subscale_value, subscale_bf, by="subscale") %>%
  rbind(cbind(total_value, total_bf) %>% mutate(subscale="total")) %>%
  left_join(formatted_names, by=c("subscale"="var_name")) %>%
  ungroup() %>%
  select(-subscale) %>%
  column_to_rownames("fmt_name") %>%
  gt(rownames_to_stub = T) %>%
  cols_move(columns = c(SUD_barriers_percent, SUD_facilitators_percent, PC_mean_v, 
                        PC_barriers_percent, PC_facilitators_percent, all_mean_v, 
                        all_barriers_percent, all_facilitators_percent),
            after = SUD_mean_v) %>%
  tab_spanner(label = md(paste0("**SUD Programs (n=", SUD_n, ")**")),
              id = "sud_programs",
              columns = starts_with("SUD")) %>%
  tab_spanner(label = md(paste0("**Primary Care Clinics (n=", PC_n, ")**")),
              id = "pc_programs",
              columns = starts_with("PC")) %>%
  tab_spanner(label = md(paste0("**All Sites (n=", all_n, ")**")),
              id = "all_programs",
              columns = starts_with("all")) %>%
  cols_label(.list = formatted_columns) %>%
  tab_stubhead("Mean Values") %>%
  fmt_number(ends_with("_mean_v")) %>%
  fmt_percent(ends_with("_percent")) %>%
  gtsave("figures/subscale_cdi_table.html")


### Levels table
left_join(level_value, level_bf, by="level") %>%
  rbind(cbind(total_value, total_bf) %>% mutate(level="total")) %>%
  left_join(formatted_names, by=c("level"="var_name")) %>%
  ungroup() %>%
  select(-level) %>%
  column_to_rownames("fmt_name") %>%
  gt(rownames_to_stub = T) %>%
  cols_move(columns = c(SUD_barriers_percent, SUD_facilitators_percent, PC_mean_v, 
                        PC_barriers_percent, PC_facilitators_percent, all_mean_v, 
                        all_barriers_percent, all_facilitators_percent),
            after = SUD_mean_v) %>%
  tab_spanner(label = md(paste0("**SUD Programs (n=", SUD_n, ")**")),
              id = "sud_programs",
              columns = starts_with("SUD")) %>%
  tab_spanner(label = md(paste0("**Primary Care Clinics (n=", PC_n, ")**")),
              id = "pc_programs",
              columns = starts_with("PC")) %>%
  tab_spanner(label = md(paste0("**All Sites (n=", all_n, ")**")),
              id = "all_programs",
              columns = starts_with("all")) %>%
  cols_label(.list = formatted_columns) %>%
  tab_stubhead("Mean Values") %>%
  fmt_number(ends_with("_mean_v")) %>%
  fmt_percent(ends_with("_percent")) %>%
  gtsave("figures/level_cdi_table.html")



