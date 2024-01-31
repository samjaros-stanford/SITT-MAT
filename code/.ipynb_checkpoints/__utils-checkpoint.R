# Standard imports for data import
require(here)
require(lubridate)
require(stringr)
require(tidyverse)

# Pull in data directly from RedCap
require(RCurl)
require(yaml)
get_redcap_report <- function(project_acronym, report_id) {
  result <- postForm(
    "https://redcap.stanford.edu/api/",
    token=yaml.load_file(here::here("secrets.yaml"))[[paste0("redcap_",project_acronym,"_key")]],
    content='report',
    format='csv',
    report_id=report_id
  )
  return(read_csv(I(result), show_col_types=F, na=c("","NA","na",".")))
}
