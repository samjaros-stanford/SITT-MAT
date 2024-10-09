library(extrafont)
#font_import()
#loadfonts(device="win") # NOTE: May need to change this to have it work on Mac
library(ggplot2)
library(grid)
library(lubridate)
library(scales)
library(tidyverse)
library(zoo)


############
# Settings #
############
# QRIR Data assembled by code/09_combine_for_emf.R
report_data = read.csv(here::here("data/current_QRIR.csv")) %>%
  mutate(date = ymd(date))
# Colors for most plots
SITTMAT_colors = c("#007ea7", "#ff9f1c", "#5d9818", "#e71d36", "#9900ff", "#00ffff")
# Colors for linkage plots
red_yellow_green = c("#e71d36", "#ff9f1c", "#5d9818")
# red_yellow_green = c( "#5d9818", "#ff9f1c","#e71d36")

font = "Century Gothic"
month_for_filenames = "Aug24"
date_filter = as.Date("2024-05-29")
x_labels <- c("Sep22 -\nNov22", "",  "Dec22 - \nFeb23", "",   "Mar23 - \nMay23",  "", "Jun23 - \nAug23","", "Sep23 - \nNov23","", "Dec23-\nFeb24", "Mar24", "Apr24", "May24")

reaim_dims = c(5.4,1.3) # in inches, normally 5.49, 1.3
imat_size = "short" #tall or short

MOUD_measure_labels = list("new_percentage_b5p"="MOUD within 30 days",
                           "new_percentage_c1p"="MOUD within 72 hours")

##########
# RE-AIM #
##########

# Line plot for percent patients diagnosed with OUD on MOUD within 30 days & 72h
make_MOUDplot = function(id, save=F, labels=T){
  start_date <- min(report_data$date, na.rm= TRUE)
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% c("reaim_c1p", "reaim_c1", "reaim_b2", "reaim_b5", "reaim_b5p"),
           date <= date_filter) %>% 
    mutate(
      months_since_start = interval(as.Date(start_date), date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date)
    )
  
  # Handle position of "SITT-MAT Target" line
  targetLabel_x = max(quarter_data$date)
  # targetLabel_y = ifelse(quarter_data[quarter_data$date==targetLabel_x & quarter_data$variable=="reaim_c1p","value"]>75,65,85)
  # For manual adjustment of the target label, when needed
  targetLabel_x = ymd("2023-1-01")
  targetLabel_y = 85
  
  # last_month <- as.numeric(format(max(quarter_data$date), "%m"))
  last_quarter <- max(quarter_data$quarter)
  max_months_since_start = max(quarter_data$months_since_start)
  
  sum_data <- quarter_data %>%
    mutate(grouping_period = if_else(quarter == last_quarter, 
                                     (as.numeric(last_quarter) + (as.numeric(months_since_start) - (max_months_since_start-2))),
                                     as.numeric(quarter))) %>%
    group_by(grouping_period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  # Spread data to wide format
  wide_data <- pivot_wider(sum_data, names_from = variable, values_from = value)
  # Calculate new percentages
  wide_data <- wide_data %>%
    mutate(
      new_percentage_c1p = ifelse(reaim_c1 == 0 & reaim_b2 == 0, 0, (reaim_c1 / reaim_b2) * 100),
      new_percentage_b5p = ifelse(reaim_b5 == 0 & reaim_b2 == 0, 0, (reaim_b5 / reaim_b2) * 100)
      
    )


  # Prepare plot data
  plot_data <- wide_data %>%
    pivot_longer(cols = c(new_percentage_c1p, new_percentage_b5p),
                 names_to = "variable", values_to = "value") %>%
    filter(variable %in% c("new_percentage_c1p", "new_percentage_b5p"))

  # Handle presence (or absense) of labels
  if(labels){
    label_vars <- wide_data
    label_data = rbind(label_vars %>%
                         mutate(variable = "new_percentage_c1p",
                                value = ifelse(new_percentage_c1p == 90, 80, new_percentage_c1p),
                                denom = paste0(reaim_c1,"/",reaim_b2)),
                       label_vars %>%
                         mutate(variable = "new_percentage_b5p",
                                value = ifelse(new_percentage_b5p == 100, new_percentage_b5p, new_percentage_b5p ),
                                denom = paste0(reaim_b5,"/",reaim_b2))
                        )
    label_data$grouping_period = c(0,2,4,6,8,10,11,12,13,0,2,4,6,8,10,11,12,13) # create the grouping period based on quarters and months
    labels_geom = geom_label(data=label_data, aes(x=grouping_period, color=variable, y=value, label=denom), 
                             label.padding=unit(0.1, "lines"),size=3, show.legend=F)
  } else {
    labels_geom = geom_blank()
  }

  # Function to insert NA rows between specific rows
  insert_na_rows <- function(df, insert_positions, num_na_rows) {
    for (pos in sort(insert_positions, decreasing = TRUE)) {
      new_rows <- data.frame(
        grouping_period = rep(NA, num_na_rows),
        reaim_b2 = rep(NA, num_na_rows),
        reaim_b5 = rep(NA, num_na_rows),
        reaim_b5p = rep(NA, num_na_rows),
        reaim_c1 = rep(NA, num_na_rows),
        reaim_c1p = rep(NA, num_na_rows),
        variable = rep("new_percentage_c1p", num_na_rows),
        value = rep(NA, num_na_rows)
      )
      df <- rbind(df[1:pos, ], new_rows, df[(pos+1):nrow(df), ])
    }
    return(df)
  }
  
  # Function to insert NA rows between specific rows
  insert_na_rows_b5 <- function(df, insert_positions, num_na_rows) {
    for (pos in sort(insert_positions, decreasing = TRUE)) {
      new_rows <- data.frame(
        grouping_period = rep(NA, num_na_rows),
        reaim_b2 = rep(NA, num_na_rows),
        reaim_b5 = rep(NA, num_na_rows),
        reaim_b5p = rep(NA, num_na_rows),
        reaim_c1 = rep(NA, num_na_rows),
        reaim_c1p = rep(NA, num_na_rows),
        variable = rep("new_percentage_b5p", num_na_rows),
        value = rep(NA, num_na_rows)
      )
      df <- rbind(df[1:pos, ], new_rows, df[(pos+1):nrow(df), ])
    }
    return(df)
  }
  
  plot_data_c1 <- plot_data[plot_data$variable == 'new_percentage_c1p', ]
  plot_data_b5 <- plot_data[plot_data$variable == 'new_percentage_b5p', ]
  
  insert_positions <- c(1, 2, 3, 4, 5)  # Positions to insert NA rows
  num_na_rows <- 1  # Number of NA rows to insert at each position
  
  data_with_na_c1 <- insert_na_rows(plot_data_c1, insert_positions, num_na_rows)
  data_with_na_b5 <- insert_na_rows_b5(plot_data_b5, insert_positions, num_na_rows)
  
  data_with_na_c1$grouping_period <- seq(0, length.out = nrow(data_with_na_c1), by = 1)
  data_with_na_b5$grouping_period <- seq(0, length.out = nrow(data_with_na_b5), by = 1)
  
  # Interpolate NA values using linear interpolation
  data_with_na_c1 <- data_with_na_c1 %>%
    group_by(variable) %>%
    mutate(value = na.approx(value, na.rm = FALSE))
  
  data_with_na_b5 <- data_with_na_b5 %>%
    group_by(variable) %>%
    mutate(value = na.approx(value, na.rm = FALSE))
  
  data_with_na <- rbind(data_with_na_c1, data_with_na_b5)

  min_x <- min(data_with_na$grouping_period, na.rm = TRUE)
  max_x <- max(data_with_na$grouping_period, na.rm = TRUE)
  
  plot = ggplot(data_with_na) +
    geom_hline(yintercept=75, linetype="dashed", color=SITTMAT_colors[3]) +
    geom_text(aes(x=3, y=targetLabel_y, label="SITT-MAT Target"),
              color=SITTMAT_colors[3], size=3, family=font, fontface="bold",
              lineheight=0.75, hjust=0.8) +
    geom_line(aes(x=grouping_period, color=variable, y=value), linewidth=1) +
    labels_geom +
    geom_vline(xintercept = last_quarter + 4.5, linetype = "dotted", color = SITTMAT_colors[4]) +
    geom_text(aes(x = last_quarter+1.5, y = 50, label = "Previous \nQuarters"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    geom_text(aes(x = last_quarter+5.5, y = 50, label = "This \nQuarter"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    scale_color_manual(values = c(SITTMAT_colors[2], SITTMAT_colors[1]),labels = c(MOUD_measure_labels[1], MOUD_measure_labels[2]))+
    scale_x_continuous(labels = x_labels, breaks = seq(min_x, max_x, by = 1)) +
    scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=7.3, family=font, color="black"),
          axis.text.x = element_text(angle=-10, vjust=0.5, hjust=0.3),
          plot.title = element_blank(),
          plot.margin = margin(t=3,r=4,b=-3),

          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size=8, family=font, color="black"),
          legend.margin = margin(t=-10)) +
    coord_cartesian(clip="off")

  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_MOUDplot_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

##########################
# | USE THIS FOR PLOTS | #
# v  Runner functions  v #
##########################

# Define which sites you want to produce plots for
programs = c('id44')

for(program in programs){
  make_MOUDplot(program, save=T)

}
