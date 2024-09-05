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
cdi_item = readRDS("data/current_long_cdi.rds")
# cdi_subscale = TBD, current subscale is barrier/neutral/facilitator

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

ultrawide = clean_reaim %>%
  bind_rows(clean_imat) %>%
  pivot_wider(id_cols = program_id,
              names_from = col_name,
              values_from = value)

# Export #######################################################################
write.csv(ultrawide, "data/ultrawide_reaim-imat_forHannah.csv")
