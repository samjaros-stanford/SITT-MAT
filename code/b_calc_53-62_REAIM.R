# Take raw inputs directly from EHR and turn it into counts for sites 53-62

# NOTE: IDs provided by the site are incorrect, so these patients will be issued
#         corrected id's in the format "c##-####"

require(readxl)
require(tidyverse)
require(here)

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

# Import data from site
#   Give more friendly column names
#   Get dates as lubridate
#   Duplicate rows removed as multiple visits in a day (if possible) are not relevant
raw_visits = read_excel(here::here(visit_file)) %>%
  rename(id_from_site = "SITT-MAT ID",
         encounter_date = "cln enc date",
         provider = prvdr,
         site_name = "svc dprtmnt") %>%
  mutate(encounter_date = ymd(encounter_date),
         # Added is_visit column for when visit and rx data are combined
         is_visit = T) %>%
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
  mutate(rx_order_date = ymd(rx_order_date),
         # Added is_rx column for when visit and rx data are combined
         is_rx = T) %>%
  distinct()

# Get association between site names in raw data and study site ID's
site_name_id = read.csv("data/53-62_decoder.csv")

# Assign each patient a new study ID based on the standardized format
#   TODO: Patient id_translation should reference a file with previously stored 
#         IDs to make sure past patients keep their IDs
id_translation = raw_visits %>%
  select(id_from_site, encounter_date, site_name) %>%
  bind_rows(raw_prescriptions %>%
              select(id_from_site, rx_order_date, rx_site_name) %>%
              rename(encounter_date=rx_order_date,
                     site_name=rx_site_name)) %>%
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
  mutate(pr_id = paste0("c",
                        str_sub(program_id, start=-2),
                        "-",
                        str_pad(row_number(), width=4, side="left", pad="0"))) %>%
  ungroup() %>%
  select(id_from_site, pr_id)

######################
# Analysis data sets #
######################

# Data sets with prefix "all_" indicate all data given to us by the site is used
# Data sets with prefix "study_" ensure the diagnosis (or re-diagnosis) occurred
#   after the study start date 2022-09-01

# Only look at prescriptions that are valid for 
all_prescriptions = raw_prescriptions %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", str_sub(pr_id,2,3)),
         # Prefer column name encounter_date, provider
         encounter_date = rx_order_date,
         # Provider name in format lastnamef because they can't be trusted to
         #   be consistent with spacing, hyphens, or capitalization
         #   gsub regex replaces all non-word characters with blank
         provider = tolower(gsub("\\W", "",
                         lapply(str_extract_all(rx_provider,
                                                # Match Last Name at beginning
                                                # OR first initial after ", "
                                                "(^[^\\,]+)|(?<=\\,\\h)[A-Z]"),
                                paste, collapse = ""))),
         # Number representing the effectiveness multiplier for each prescription
         #  NAs assumed to be prescribed just once
         #  + 1 to all others for initial prescription
         rx_multiplier = if_else(is.na(rx_num_refills),1,rx_num_refills+1),
         # Shots get an assumed 34 day effectiveness
         #  Multiply by refills + 1 (for cases where there are no refills)
         eff_days = case_when(
           grepl("VIVITROL", rx_name)  ~ 34*rx_multiplier,
           grepl("SUBLOCADE", rx_name) ~ 34*rx_multiplier,
           !is.na(rx_quantity)         ~ parse_number(rx_quantity)*rx_multiplier,
           # Remaining NA quantities get 0 effectiveness, 
           #  assuming one tablet/film was administered
           T                           ~ 0)) %>%
  # Get a column for where the precription was given for calculating number of prescribers
  left_join(site_name_id %>%
              rename(rx_site_id=program_id),
            by=c("rx_site_name"="site_name"))

all_visits = raw_visits %>%
  left_join(id_translation, by="id_from_site") %>%
  mutate(program_id = paste0("id", str_sub(pr_id,2,3)),
         # Visits get no effective days
         eff_days = 0,
         # Provider name in format lastnamef because they can't be trusted to
         #   be consistent with spacing, hyphens, or capitalization
         #   gsub regex replaces all non-word characters with blank
         provider = tolower(gsub("\\W", "",
                         lapply(str_extract_all(provider,
                                                # Match Last Name at beginning
                                                # OR first initial after "_"
                                                "(^[^_]+)|(?<=_)[A-Z]"),
                                paste, collapse = ""))))

all_data = all_prescriptions %>%
  select(pr_id, program_id, encounter_date, provider, eff_days, is_rx) %>%
  bind_rows(all_visits %>%
              select(pr_id, program_id, encounter_date, 
                     provider, eff_days, is_visit)) %>%
  # Replace missing values in indicator columns with F
  replace_na(list(is_rx=F, is_visit=F)) %>%
  # Collapse visits and rx's that occur on the same day
  group_by(pr_id, program_id, encounter_date, provider) %>%
  summarize(eff_days = max(eff_days, na.rm=T),
            is_rx = any(is_rx),
            is_visit = any(is_visit)) %>%
  ungroup() %>%
  # Define month year for easier aggregation
  mutate(month_year = format(encounter_date, "%b %Y")) %>%
  # Get indicator for ever treated
  group_by(pr_id) %>%
  mutate(ever_treated = any(is_rx)) %>%
  ungroup() %>%
  # Create empty columns for loop below
  mutate(is_dx = NA,
         is_tx_start = NA,
         is_tx_end = NA,
         end_tx_date = NA_Date_,
         status = NA_character_)

# Calculate start, restarts, and breaks based on documentation
#   is_dx = Initial diagnosis visit or when a previous patient is seen 
#             after treatment lapses
#   is_tx_start = First medication in this continuous treatment time
#   is_tx_end = When treatment lapses - effective days does not cover gap + 60 days
cur_pat = ""
for(i in 1:nrow(all_data)){
  # For a patient's first visit
  if(all_data[i,"pr_id"]!=cur_pat){
    # Reset patient tracker
    cur_pat = all_data[i,"pr_id"]
    # is_dx for first visit
    all_data[i,"is_dx"]=T
    # Calculate date when treatment would be considered broken
    all_data[i,"end_tx_date"] = all_data[i,]$encounter_date + all_data[i,]$eff_days + 60
    if(all_data[i,]$is_rx){
      all_data[i,"is_tx_start"]=T
      all_data[i,"is_tx_end"]=T
      all_data[i,"status"]="start tx"
    } else{
      all_data[i,"is_tx_start"]=F
      all_data[i,"is_tx_end"]=F
      all_data[i,"status"]="start dx"
    }
    next
  }
  
  # For all other visits
  
  ## Set this end_tx_date based on this eff_days and last eff_days
  all_data[i,"end_tx_date"] = max(all_data[i,]$encounter_date + all_data[i,]$eff_days + 60,
                                  all_data[i-1,]$end_tx_date,
                                  na.rm=T)

  # If they were already on treatment
  if(grepl("tx",all_data[i-1,"status"])){
    # If this encounter is within the end_tx_date of last time
    if(all_data[i,"encounter_date"] < all_data[i-1,"end_tx_date"]){
      # Treatment has continued
      all_data[i,"is_dx"]=F
      all_data[i,"is_tx_start"]=F
      all_data[i-1,"is_tx_end"]=F # Go back and tell last that tx has continued
      all_data[i,"is_tx_end"]=T # Default to this treatment being the end
      all_data[i,"status"]="cont tx"
    } else {
      # Treatment has discontinued and now they're back
      ## If this is only a visit
      if(!all_data[i,]$is_rx){
        all_data[i,"is_dx"]=T
        all_data[i,"is_tx_start"]=F
        all_data[i,"is_tx_end"]=F # Treatment hasn't started so it can't end
        all_data[i,"status"]="restart dx"
      } else { ## If this is a new treatment
        all_data[i,"is_dx"]=T
        all_data[i,"is_tx_start"]=T
        all_data[i,"is_tx_end"]=T # Default to this treatment being the end
        all_data[i,"status"]="restart tx"
      }
    }
    next
  }
  
  # If they weren't already on treatment
  ## If this encounter is within the end_tx_date of last time
  if(all_data[i,"encounter_date"] < all_data[i-1,"end_tx_date"]){
    ## If this visit is a prescription
    if(all_data[i,]$is_rx){
      ### Treatment has started
      all_data[i,"is_dx"]=F # Diagnosis already happened
      all_data[i,"is_tx_start"]=T
      all_data[i,"is_tx_end"]=T # Default to this treatment being the end
      all_data[i,"status"]="start tx"
    } else { ## If this visit isn't a prescription
      ### Diagnosis stage has continued
      all_data[i,"is_dx"]=F # Diagnosis already happened
      all_data[i,"is_tx_start"]=F
      all_data[i,"is_tx_end"]=F # Treatment hasn't started so it can't end
      all_data[i,"status"]="cont dx"
    }
  } else {
    ## Treatment has discontinued and now they're back
    ### If this is only a visit
    if(!all_data[i,]$is_rx){
      all_data[i,"is_dx"]=T
      all_data[i,"is_tx_start"]=F
      all_data[i,"is_tx_end"]=F # Treatment hasn't started so it can't end
      all_data[i,"status"]="restart dx"
    } else { ### If this is a new treatment
      all_data[i,"is_dx"]=T
      all_data[i,"is_tx_start"]=T
      all_data[i,"is_tx_end"]=T # Default to this treatment being the end
      all_data[i,"status"]="restart tx"
    }
  }
}

# For study data, the patient must have been diagnosed after Sep 1 2022
#   Decide on which ID's will be included in the study
#     If they were started or restarted after Sep 1, 2022, they're eligible
#   For the patients whose starts were cut off but restarts remain,
#     reassign the first restart to be the start
study_data = all_data %>%
  filter(is_dx) %>%
  filter(encounter_date>=ymd("2022-09-01")) %>%
  group_by(pr_id) %>%
  filter(row_number()==1) %>%
  rename(first_study_date=encounter_date) %>%
  select(pr_id, first_study_date) %>%
  ungroup() %>%
  # Join back in the data for eligible patients
  left_join(all_data, by="pr_id") %>%
  filter(encounter_date>=first_study_date) %>%
  arrange(pr_id, encounter_date) %>%
  ungroup() %>%
  # Join in prescribing site information for A1 calculation
  left_join(all_prescriptions %>%
              select(pr_id, encounter_date, provider, rx_site_id) %>%
              distinct(), 
            by=c("pr_id", "encounter_date", "provider"))

study_months = study_data %>%
  arrange(encounter_date) %>%
  pull(encounter_date) %>%
  format("%b %Y") %>%
  unique() %>%
  toString()

# Get months where the patient was on MOUD as defined in the PCHS data documentation
# TODO: Is assigning continuous_tx_id needed?
rx_start_stop = study_data %>%
  filter(is_tx_start | is_tx_end) %>%
  mutate(continuous_tx_id = if_else(row_number()==1 | is_tx_start, row_number(), lag(row_number()))) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id, program_id, continuous_tx_id) %>%
  summarize(start_date = first(encounter_date),
            # Accept a previous visit if giving longer effective time (need to subtract 60 buffer)
            end_date = max(last(encounter_date) + last(eff_days),
                           last(end_tx_date) - 60),
            .groups="keep") %>%
  ungroup() %>%
  mutate(month_year=study_months) %>%
  separate_rows(month_year, sep=", ") %>%
  # Test if the interval month (ex. Sep 1 2022 - Sep 30 2022) overlaps with the treatment interval
  mutate(is_rx_month = int_overlaps(interval(my(month_year), ceiling_date(my(month_year),"month")-days(1)), interval(start_date, end_date)))

dx_tx_match = study_data %>%
  filter(is_dx | is_tx_start) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id) %>%
  mutate(tx_date = if_else(is_tx_start, encounter_date, lead(encounter_date))) %>%
  ungroup() %>%
  filter(is_dx)

################
# Calculations #
################
# A1: Number of integrated prescribers per month
#     Number of prescribers employed or contracted by the agency who have 
#       prescribed medications for opioid use disorder to at least 1 patient in
#       the past month
A1 = study_data %>%
  filter(!is.na(rx_site_id)) %>%
  group_by(rx_site_id, month_year) %>%
  summarize(reaim_a1 = length(unique(provider)), .groups="keep") %>%
  ungroup() %>%
  rename(program_id=rx_site_id) %>%
  select(program_id, month_year, reaim_a1)

# B1: Number of new and existing patients diagnosed with OUD
#     # OLD MEASURE #
B1 = study_data %>%
  filter(is_dx | is_tx_end) %>%
  mutate(continuous_dx_id = if_else(row_number()==1 | is_dx, row_number(), lag(row_number()))) %>%
  arrange(pr_id, encounter_date) %>%
  group_by(pr_id, program_id, continuous_dx_id) %>%
  summarize(start_date = first(encounter_date),
            end_date = max(last(encounter_date) - last(eff_days),
                           last(end_tx_date) - 60),
            .groups="keep") %>%
  ungroup() %>%
  mutate(month_year=study_months) %>%
  separate_rows(month_year, sep=", ") %>%
  # Test if the interval month (ex. Sep 1 2022 - Sep 30 2022) overlaps with the treatment interval
  mutate(is_dx_month = int_overlaps(interval(my(month_year), 
                                             ceiling_date(my(month_year),
                                                          "month")-days(1)), 
                                    interval(start_date, end_date))) %>%
  filter(is_dx_month) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_b1 = n(), .groups="keep") %>%
  ungroup()

# B2: Number of new patients diagnosed with OUD
#     The total number of patients with a new ICD10 or DSM5 diagnosis of OUD in
#       the past month. *All patients listed will be assumed to have the correct
#       ICD-10/DSM-5 diagnosis*
B2 = study_data %>%
  filter(is_dx) %>%
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
B5 = dx_tx_match %>%
  filter(!is.na(tx_date),
         tx_date-encounter_date<=30) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_b5 = n(), .groups="keep") %>%
  ungroup()

# C1: Number of new patients prescribed MOUD within 72h
#     Of the total number of patients reported for B2, calculate the subset of
#       patients who were newly started on MOUD within 72 hours of OUD 
#       diagnosis. Note: Include patients who re-started MOUD after a break in
#       treatment.
# C1P describes this as a percent of B2
C1 = dx_tx_match %>%
  filter(!is.na(tx_date),
         tx_date-encounter_date<=3) %>%
  group_by(program_id, month_year) %>%
  summarize(reaim_c1 = n(), .groups="keep") %>%
  ungroup()

# C3: Number of new patients retained on MOUD
#     Of the total number of patients reported in B5, the subset of patients who
#       had 2+ in-person outpatient clinical visits within 34 days of starting
#       MOUD.
# C3P describes this as a percent of B5
C3 = dx_tx_match %>%
  # Get all patients who were prescribed MOUD within 30 days of start or restart
  filter(!is.na(tx_date),
         tx_date-encounter_date<=30) %>%
  select(pr_id, program_id, month_year, tx_date) %>%
  # Join in all eligible study visits and prescriptions
  left_join(select(study_data, pr_id, encounter_date) %>%
              distinct(), 
            by="pr_id", relationship="many-to-many") %>%
  # Only keep visits that are within 34 days of the prescription date
  mutate(rx_visit_gap = encounter_date-tx_date) %>%
  filter(rx_visit_gap>0 & rx_visit_gap<=34) %>%
  # Count number of post-rx visits per patient per start/restart
  group_by(pr_id, program_id, month_year, tx_date) %>%
  summarize(n_post_tx_visits = n(), .groups="keep") %>%
  # Only keep patients with 2+ visits
  filter(n_post_tx_visits >= 2) %>%
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
  mutate(across(where(is.numeric), ~coalesce(.x,0)),
         date = my(month_year))

saveRDS(select(assembled_reaim, -month_year) %>%
          pivot_longer(cols=c(-program_id, -date),
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

write.csv(ultrawide_reaim, file="data/PCHS_ultrawide_reaim.csv", row.names=F)

