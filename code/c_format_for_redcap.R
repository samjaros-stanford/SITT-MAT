# Take long data as generated in the R files pipeline and format it for upload
#   to REDCap

library(here)
library(tidyverse)

# Import =======================================================================
long_data <- readRDS(here("data/current_53-62_reaim.rds"))

# Formatting ===================================================================
# Baseline RE-AIM upload
#   program id goes in program_id
#   event name for all is "sep_2022_arm_1" in redcap_event_name
#   outcome info goes into columns in the format demo_moud_a1_mar2022
#   baseline reaim are a1, b1, b2, b3, b4, b5

event_name <- "sep_2022_arm_1"

clean_data <- long_data %>%
  # Only want baseline data
  # Only want reaims in baseline
  filter(date <= ymd("2022-08-01")) %>%
  filter(grepl("reaim_[ab][1-5]$", variable)) %>%
  # Get reaim variable and monthYear in a column
  #   NOTE: only certain variables are tracked at baseline
  # Get event name (same for all baseline)
  mutate(reaim_month = paste(str_extract(variable, "[ab][1-5]"), 
                             tolower(format(date, "%b%Y")), 
                             sep = "_"),
         redcap_event_name = event_name) %>%
  # Get outcome columns
  pivot_wider(id_cols = c(program_id, redcap_event_name), 
              names_from = reaim_month,
              names_prefix = "demo_moud_",
              values_from = value)

# Export =======================================================================

write_csv(clean_data, here("data/redcap_export.csv"))
