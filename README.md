<a href="https://github.com/samjaros-stanford/SITT-MAT"> <img src="arch/SITT-MAT_logo.jpeg" alt="SITT-MAT Project Logo" width="120" height="120"/> </a>

# SITT-MAT

Process respondent data into project outcomes and generate figures for the Stagewise Implementation To Treat - Medications for Addiction Treatment Clinical Trial (NCT05343793)

## About

This repository contains the files required to run the analysis for the SITT-MAT clinical trial. Data cleaning steps take raw survey data from REDCap and calculate the needed outcome variables by site by month. There is also a data pipeline for calculating cost data from Qualtrics survey results.

### Software

[R \>= 4.2.2](https://cran.r-project.org/)

*Recommended:* [RStudio](https://posit.co/download/rstudio-desktop/)

### Dependencies

-   ggplot2 \>= 3.4
-   lubridate \>= 1.9
-   stringr \>= 1.5
-   tidyverse \>= 1.3

### Privacy

Maintaining patient and participant privacy is of primary importance for this project. For this reason, the `data` and `raw_data` folders have already been added to `.gitignore` so they will not be uploaded to Github. You will need to download the data from the source using your credentials and run the analyses to be able to see the data.

## Organization

### Folders

**arch** - Contains abandoned code and other static files

**code** - Contains all working code for data cleaning and analysis

**data** - Contains working data files calculated by analysis scrips

**figures** - Contains all image files and tables for figures

**raw_data** - Empty folder for raw input data

### Code Naming

`00-09` - Intake & process raw data into tables for analysis

`10-19` - Statistical analyses

`20-29` - Tables & visualizations

`30-39` - Data reports
