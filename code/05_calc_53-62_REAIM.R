# Take raw inputs directly from EHR and turn it into counts for sites 53-62

# NOTE: IDs provided by the site are incorrect, so these patients will be issued
#         corrected id's in the format "c##-####"

require(readxl)
require(tidyverse)
require(here)

# TODO: Change variable "months" to "month_year" to standardize
# TODO: Patient id_translation should reference a file with previously stored IDs to make sure past patients keep their IDs

############
# Settings #
############
# PC group registry files
#   These files are sent directly to us by the data manager and transferred via
#   Stanford Medicine Box
visit_file = "raw_data/SITT-MAT - id53 - DE-ID OUD DIAGNOSIS 3.2022-3.2023.xlsx"
rx_file = "raw_data/SITT-MAT - id53- DE-ID MOUD 3.2022-3.2023.xlsx"

########
# Data #
########

# Import data & give more friendly column names
raw_visits = read_excel(here::here(visit_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         encounter_date = "cln enc date",
         provider = prvdr,
         site_name = "svc dprtmnt") %>%
  mutate(encounter_date = ymd(encounter_date)) %>%
  distinct()
raw_prescriptions = read_excel(here::here(rx_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         rx_order_date = "order chartdate",
         rx_name = "order name (single)",
         rx_quantity = "quantity prescribed",
         rx_num_refills =  "no. of refills",
         rx_provider = "ordr provdr fll nm",
         rx_site_name = "order dprtmnt",
         rx_icd10_dx = "cd-10 clncl rdr dgnss cd",
         patient_isHomeless = "homelessstatus",
         patient_sex = "patientsex",
         patient_gender = "pat gender",
         patient_gender_identity = "pat gender identity",
         patient_ethnicity = "ethnicity",
         patient_race = "race",
         patient_language = "patient lang",
         patient_sexual_orientation = "pat sex orientation") %>%
  mutate(rx_order_date = ymd(rx_order_date)) %>%
  distinct()

# Correct patient id's to reflect site
site_name_id = read.csv("data/53-62_decoder.csv")

id_translation = raw_visits %>%
  # Get only 1 row per patient
  arrange(id_from_site, encounter_date) %>%
  group_by(id_from_site) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  # Join in table that maps site name to site ID
  left_join(site_name_id, by="site_name") %>%
  # Create patient id by combining site ID and patient number (in order of encounter)
  arrange(program_id, encounter_date) %>%
  group_by(program_id) %>%
  mutate(pr_id = paste0(str_sub(program_id, start=-2),"-",str_pad(row_number(), width=4, side="left", pad="0"))) %>%
  ungroup() %>%
  select(id_from_site, pr_id)

######################
# Analysis data sets #
######################
# Only look at prescriptions that are valid for 
all_prescriptions = raw_prescriptions %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", substr(pr_id,1,2)),
         month_year = paste0(month(rx_order_date, label=T), " ", year(rx_order_date)),
         # Shots get an assumed 34 day effectiveness
         est_rx_days = case_when(
           grepl("VIVITROL", rx_name)  ~ 34,
           grepl("SUBLOCADE", rx_name) ~ 34,
           T                           ~ parse_number(rx_quantity)
         )) %>%
  arrange(pr_id, rx_order_date) %>%
  group_by(pr_id) %>%
  mutate(is_restart = case_when(
    row_number()==1 ~ F,
    rx_order_date-lag(rx_order_date)<34 ~ F,
    rx_order_date-lag(rx_order_date)<lag(est_rx_days)*(lag(rx_num_refills)+1) ~ F,
    T ~ T
  )) %>%
  mutate(is_break = if_else(row_number()==n(), T, lead(is_restart, n=1))) %>%
  ungroup()

all_visits = raw_visits %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", substr(pr_id,1,2)),
         month_year = paste0(month(encounter_date, label=T), " ", year(encounter_date))) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id) %>%
  mutate(is_start = row_number()==1) %>%
  ungroup() %>%
  left_join(all_prescriptions %>%
              select(pr_id, rx_order_date, is_restart) %>%
              filter(is_restart),
            by=c("pr_id", "encounter_date"="rx_order_date")) %>%
  replace_na(list(is_restart=F))

# For study data, the patient must have been diagnosed after Sep 1 2022
# 1) Decide on which ID's will be included in the study
#     If they were started or restarted after Sep 1, 2022, they're eligible
after_sep2022_ids = all_visits %>%
  filter(is_start | is_restart) %>%
  filter(encounter_date>=ymd("2022-09-01")) %>%
  group_by(pr_id) %>%
  filter(row_number()==1) %>%
  rename(first_study_date=encounter_date) %>%
  select(pr_id, first_study_date) %>%
  ungroup()

# 2) Get new prescriptions data set that only has the valid study prescriptions
#     Any non-eligible patients' data is excluded
#     For eligible patients, only prescriptions after the study date are included
#     Re-calculate restarts and breaks given these new restrictions (mostly re-starts become starts)
study_prescriptions = after_sep2022_ids %>%
  left_join(all_prescriptions, by="pr_id") %>%
  filter(rx_order_date>=first_study_date) %>%
  arrange(pr_id, rx_order_date) %>%
  group_by(pr_id) %>%
  mutate(is_restart = case_when(
    row_number()==1 ~ F,
    rx_order_date-lag(rx_order_date)<60 ~ F,
    rx_order_date-lag(rx_order_date)<lag(est_rx_days)*(lag(rx_num_refills)+1) ~ F,
    T ~ T
  )) %>%
  mutate(is_break = if_else(row_number()==n(), T, lead(is_restart, n=1))) %>%
  ungroup()

# 3) Get visits data set that only has valid visits
#     Any non-eligible patients' data is excluded
#     Only take starts or restarts after the study start date
#     Reassign the first valid study visit to is_start
study_visits = after_sep2022_ids %>%
  left_join(all_visits, by="pr_id") %>%
  filter(encounter_date>=first_study_date) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id) %>%
  mutate(is_start = row_number()==1,
         is_restart = if_else(is_start,F,is_restart)) %>%
  ungroup()

# Get months where the patient was on MOUD as defined in the PCHS data documentation
rx_start_stop = study_prescriptions %>%
  group_by(pr_id) %>%
  filter(row_number()==1 | is_restart | is_break) %>%
  ungroup() %>%
  mutate(continuous_tx_id = if_else(row_number()==1 | lag(is_break) | is_restart, row_number(), lag(row_number()))) %>%
  group_by(pr_id, program_id, continuous_tx_id) %>%
  summarize(start_date = first(rx_order_date),
            end_date = last(rx_order_date) + last(est_rx_days),
            .groups="keep") %>%
  ungroup() %>%
  mutate(month_year=toString(unique(study_prescriptions$month_year))) %>%
  separate_rows(month_year, sep=", ") %>%
  # Test if the interval month (ex. Sep 1 2022 - Sep 30 2022) overlaps with the treatment interval
  mutate(is_rx_month = int_overlaps(interval(my(month_year), ceiling_date(my(month_year),"month")-days(1)), interval(start_date, end_date)))

# Given a visit_date and a patient, find the nearest prescription date after that visit
find_nearest_rx = function(dataset, patient, visit_date){
  to_return = dataset %>%
    filter(pr_id==patient, rx_order_date >= visit_date) %>%
    filter(row_number()==1) %>%
    select(rx_order_date) %>%
    pull()
    
  if(length(to_return)==0)
    return(NA)
  return(to_return)
}
visit_rx_match = study_visits %>%
  rowwise() %>%
  mutate(nearest_rx = find_nearest_rx(study_prescriptions, pr_id, encounter_date))

################
# Calculations #
################
# A1: Number of integrated prescribers per month
#     Number of prescribers employed or contracted by the agency who have 
#       prescribed medications for opioid use disorder to at least 1 patient in
#       the past month
A1 = study_prescriptions %>%
  group_by(rx_site_name, month_year) %>%
  summarize(reaim_a1 = length(unique(rx_provider)), .groups="keep") %>%
  ungroup() %>%
  left_join(site_name_id, by=join_by(rx_site_name==site_name)) %>%
  select(program_id, month_year, reaim_a1)

# B1: Number of new and existing patients diagnosed with OUD
#     # OLD MEASURE #
B1 = study_visits %>%
  arrange(pr_id, encounter_date) %>%
  filter(row_number()==1, .by="pr_id") %>%
  select(pr_id, program_id, month_year) %>%
  bind_rows(rx_start_stop %>%
              filter(is_rx_month) %>%
              select(pr_id, program_id, month_year)) %>%
  distinct() %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_b1 = n(), .groups="keep") %>%
  select(program_id, month_year, reaim_b1)

# B2: Number of new patients diagnosed with OUD
#     The total number of patients with a new ICD10 or DSM5 diagnosis of OUD in
#       the past month. *All patients listed will be assumed to have the correct
#       ICD-10/DSM-5 diagnosis*
B2 = study_visits %>%
  filter(is_start | is_restart) %>%
  select(pr_id, program_id, month_year) %>%
  distinct() %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_b2 = n(), .groups="keep") %>%
  ungroup()

# B4: Number of patients prescribed MOUD
#     The total number of patients administered MOUD in the past month. Note: 
#       Include patients who may be new, restarted, or established.
B4 = rx_start_stop %>%
  # Filter for only rows where the patient was taking MOUD that month
  filter(is_rx_month) %>%
  # Get distinct patient/month combinations in case a patient has more than one continuous tx in a month
  select(pr_id, program_id, month_year) %>%
  distinct() %>%
  # Get count of patients by site * month
  group_by(program_id, month_year) %>%
  summarize(reaim_b4 = n(), .groups="keep") %>%
  ungroup()
  
# B5: Number of new patients prescribed MOUD within 30 days of diagnosis
#     Of the total number of patients reported for B2, calculate the subset of 
#       opioid-naive patients who were newly started on the MOUD during the past
#       month. Note: Include patients who re-started MOUD after a break in 
#       treatment.
# B5P describes this as a percent of B2
B5 = visit_rx_match %>%
  filter(is_start | is_restart) %>%
  filter(!is.na(nearest_rx)) %>%
  filter(nearest_rx-encounter_date<=30) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_b5 = n(), .groups="keep") %>%
  ungroup()

# C1: Number of new patients prescribed MOUD within 72h
#     Of the total number of patients reported for B2, calculate the subset of
#       patients who were newly started on MOUD within 72 hours of OUD 
#       diagnosis. Note: Include patients who re-started MOUD after a break in
#       treatment.
# C1P describes this as a percent of B2
C1 = visit_rx_match %>%
  filter(is_start | is_restart) %>%
  filter(!is.na(nearest_rx)) %>%
  filter(nearest_rx-encounter_date<=3) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_c1 = n(), .groups="keep") %>%
  ungroup() 

# C3: Number of new patients retained on MOUD
#     Of the total number of patients reported in B5, the subset of patients who
#       had 2+ in-person outpatient clinical visits within 34 days of starting
#       MOUD.
# C3P describes this as a percent of B5
C3 = visit_rx_match %>%
  # Get all patients who were prescribed MOUD within 30 days of start or restart
  filter(is_start | is_restart) %>%
  filter(!is.na(nearest_rx)) %>%
  filter(nearest_rx-encounter_date<=30) %>%
  select(pr_id, program_id, month_year, nearest_rx) %>%
  # Join in all eligible study visits and prescriptions
  left_join(rbind(select(study_visits, pr_id, encounter_date),
                  select(study_prescriptions, pr_id, rx_order_date) %>% rename(encounter_date=rx_order_date)) %>%
              distinct(), by="pr_id", relationship="many-to-many") %>%
  # Only keep visits that are within 34 days of the prescription date
  mutate(rx_visit_gap = encounter_date-nearest_rx) %>%
  filter(rx_visit_gap>0 & rx_visit_gap<=34) %>%
  # Count number of post-rx visits per patient per start/restart
  group_by(pr_id, program_id, month_year, nearest_rx) %>%
  summarize(n_post_rx_visits = n(), .groups="keep") %>%
  # Only keep patients with 2+ visits
  filter(n_post_rx_visits >= 2) %>%
  # Get count by program_id and month and get in format ready for storage
  group_by(program_id, month_year) %>%
  summarize(reaim_c3 = n(), .groups="keep") %>%
  ungroup()


##########
# Export #
##########

assembled_reaim = full_join(A1, B1) %>%
  full_join(B2) %>%
  full_join(B4) %>%
  full_join(B5) %>%
  full_join(C1) %>%
  full_join(C3) %>%
  # Create percent variables
  mutate(reaim_b4p = reaim_b4/reaim_b1*100,
         reaim_b5p = reaim_b5/reaim_b2*100,
         reaim_c1p = reaim_c1/reaim_b2*100,
         reaim_c3p = reaim_c3/reaim_b5*100) %>%
  # Replace NAs with 0
  mutate(across(where(is.numeric), ~coalesce(.x,0)))

saveRDS(pivot_longer(assembled_reaim, 
                     cols=c(-program_id, -month_year),
                     names_to="variable",
                     values_to="value"), 
        "data/current_53-62_reaim.rds")

ultrawide_reaim = assembled_reaim %>%
  # Get actual date for ordering and display date for naming
  mutate(date = my(month_year),
         disp_my = tolower(str_replace_all(month_year, "([A-Za-z]{3})\\s\\d{2}(\\d{2})", "\\1\\2"))) %>%
  # Arrange correctly
  arrange(program_id, date) %>%
  # Pivot to put reaim and date in column names, backwards for ordering
  pivot_wider(id_cols=program_id,
              names_from=disp_my,
              names_glue="{.value}_{disp_my}",
              values_from=starts_with("reaim_")) %>%
  # Flip names
  rename_with(.cols=starts_with("reaim_"),
              .fn=~str_replace(.x, "(\\w*)_(\\w{5})$", "\\2_\\1"))

write.csv(ultrawide_reaim, file="data/PCHS_ultrawide_reaim.csv")

