# Data need to be in long format for use in EMF
# This file can be changed to produce different data sets that update
#   alongside the current CDI, REAIM, and IMAT datasets

library(stringr)
library(tidyverse)

# Prep REAIM
#   Add clinic type
reaim = readRDS("data/current_reaim.rds") %>%
  bind_rows(readRDS("data/current_53-62_reaim.rds")) %>%
  mutate(type = if_else(program_id %in% paste0("id", str_pad(0:49, 2, "left", "0")),
                                                                "SUD",
                                                                "PC"))

# Prep IMAT
#   Add clinic type
imat = readRDS("data/current_imat_subscale.rds") %>%
  mutate(type = if_else(program_id %in% paste0("id", str_pad(0:49, 2, "left", "0")),
                        "SUD",
                        "PC"))

# Create data set for "Quarterly REAIM/IMAT Reports" (QRIR)
write.csv(bind_rows(reaim, imat), "data/current_QRIR.csv", row.names=F)
