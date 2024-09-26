# Creates report requested in #9
## All sites
## REAIM + IMAT + CDI
## All items and subscales
## All dates
## Columns are program_id and instrument_item_date

# Import #######################################################################
reaim = readRDS("data/current_reaim.rds")
imat_item = readRDS("data/current_imat_item.rds")
imat_subscale = readRDS("data/current_imat_subscale.rds")
cdi_item = readRDS("data/current_cdi_item.rds")
cdi_subscale = readRDS("data/current_cdi_subscale.rds")

# Clean & Combine ##############################################################
clean_reaim = reaim %>%
  filter(!is.na(value)) %>%
  # Create what will become the column names in ultrawide
  mutate(col_name = paste0(tolower(format(date, "%b_%Y")), "_", variable)) %>%
  # Arrange so that final columns are in order
  arrange(variable, date) %>%
  # Select only desired columns
  select(program_id, col_name, value)

clean_imat = imat_item %>%
  # Get imat items in the format d1-1 instead of d1_1 for easy splitting
  separate(variable, into=c("survey", "dimension", "q_num"), sep="_") %>%
  unite(col="surv_dim", "survey", "dimension", remove=T) %>%
  unite(col="variable", "surv_dim", "q_num", sep="-", remove=T) %>%
  # Arrange so that final columns are in order
  arrange(variable, date) %>%
  # Add in subscale data
  bind_rows(imat_subscale %>%
              # Arrange so that final columns are in order
              arrange(variable, date)) %>%
  # Create what will become the column names in ultrawide
  mutate(col_name = paste0(tolower(format(date, "%b_%Y")), "_", variable)) %>%
  # Select only desired columns
  select(program_id, col_name, value)

clean_cdi = cdi_item %>%
  pivot_longer(cols = c(value, cat),
               names_to = "measure_type",
               values_to = "measure_value",
               values_transform = as.character) %>%
  arrange(q_num, date) %>%
  bind_rows(cdi_subscale %>%
              rename(item=subscale) %>%
              pivot_longer(
                cols = c(barriers, neutral, facilitators),
                names_to = "measure_type",
                values_to = "measure_value",
                values_transform = as.character)
              ) %>%
  mutate(col_name = paste0(tolower(format(date, "%b_%Y")), "_", item, "_", measure_type)) %>%
  pivot_wider(id_cols = program_id,
              names_from = col_name,
              values_from = measure_value) %>%
  mutate(across(ends_with(c("score", "barriers", "neutral", "facilitators")), as.numeric))

# NOTE: CDI was already flipped to wide so I could transform the columns where needed
ultrawide = clean_reaim %>%
  bind_rows(clean_imat) %>%
  pivot_wider(id_cols = program_id,
              names_from = col_name,
              values_from = value) %>%
  left_join(clean_cdi, join_by("program_id")) %>%
  arrange(program_id)

# Export #######################################################################
write_csv(ultrawide, "data/ultrawide_reaim-imat-cdi.csv")
