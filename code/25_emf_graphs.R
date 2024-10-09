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
    filter(program_id == id, variable %in% c("reaim_c1p", "reaim_c1", "reaim_b2"),
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
  
  if (id == 'id45'){
    # Calculate new percentages
    plot_data <- quarter_data %>% 
      mutate(variable = ifelse(variable == "reaim_c1p", "new_percentage_c1p", variable))
    # Handle presence (or absense) of labels
    if(labels){
      label_vars = report_data %>%
        filter(program_id==id,
               variable %in% c("reaim_b5p", "reaim_c1p", "reaim_b5", "reaim_b2", "reaim_c1")) %>%
        pivot_wider(id_cols=c("date"),
                    names_from="variable",
                    values_from="value")
      
      label_data = rbind(label_vars %>%
                           mutate(variable = "reaim_b5p",
                                  value = reaim_b5p,
                                  denom = paste0(reaim_b5,"/",reaim_b2)),
                         label_vars %>%
                           mutate(variable = "reaim_c1p",
                                  value = reaim_c1p,
                                  denom = paste0(reaim_c1,"/",reaim_b2)))
      
      labels_geom = geom_label(data=label_data, aes(x=date, color=variable, y=value, label=denom), 
                               label.padding=unit(0.1, "lines"), color=SITTMAT_colors[1], size=3, show.legend=F)
    } else {
      labels_geom = geom_blank()
    }
    
    plot_data <- plot_data %>%
      filter(variable == "new_percentage_c1p")
    targetLabel_x = max(plot_data$date)
    plot_data$variable <- "MOUD within 72 hours"
    
    plot = ggplot(plot_data) +
      geom_hline(yintercept=75, linetype="dashed", color=SITTMAT_colors[3]) +
      geom_text(aes(x=targetLabel_x, y=targetLabel_y, label="SITT-MAT Target"), 
                color=SITTMAT_colors[3], size=3, family=font, fontface="bold",
                lineheight=0.75, hjust=0.8) +
      geom_line(aes(x=date, color=variable, linetype=variable, y=value), linewidth=1) +
      labels_geom +
      scale_color_manual(values=SITTMAT_colors[1]) +
      scale_x_date(date_breaks="month", date_labels="%b-%y", 
                   limits=c(as.Date("2024-03-01"), max(plot_data$date))) + 
      scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size=8, family=font, color="black"),
            axis.text.x = element_text(angle=0, vjust=0, hjust=0.3),
            plot.title = element_blank(),
            plot.margin = margin(t=3,r=10,b=-3),    
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
  else {
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
        new_percentage_c1p = ifelse(reaim_c1 == 0 & reaim_b2 == 0, 0, (reaim_c1 / reaim_b2) * 100)
      )

    
    # Prepare plot data
    plot_data <- wide_data %>%
      pivot_longer(cols = c(new_percentage_c1p),
                   names_to = "variable", values_to = "value") %>%
      filter(variable %in% c("new_percentage_c1p"))
    
    
    # Handle presence (or absense) of labels
    if(labels){
      label_vars <- wide_data
      label_data = rbind(label_vars %>%
                           mutate(variable = "new_percentage_c1p",
                                  value = new_percentage_c1p,
                                  denom = paste0(reaim_c1,"/",reaim_b2)))
      label_data$grouping_period = c(0,2,4,6,8,10,11,12,13) # create the grouping period based on quarters and months, CHANGE FOR NEXT QUATER
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
          reaim_c1 = rep(NA, num_na_rows),
          reaim_c1p = rep(NA, num_na_rows),
          variable = rep("new_percentage_c1p", num_na_rows),
          value = rep(NA, num_na_rows)
        )
        df <- rbind(df[1:pos, ], new_rows, df[(pos+1):nrow(df), ])
      }
      return(df)
    }
    
    insert_positions <- c(1, 2, 3, 4, 5)  # Positions to insert NA rows
    num_na_rows <- 1  # Number of NA rows to insert at each position
    data_with_na <- insert_na_rows(plot_data, insert_positions, num_na_rows)
    data_with_na$grouping_period <- seq(0, length.out = nrow(data_with_na), by = 1)
    
    min_x <- min(data_with_na$grouping_period, na.rm = TRUE)
    max_x <- max(data_with_na$grouping_period, na.rm = TRUE)
    
    # Interpolate NA values using linear interpolation
    data_with_na <- data_with_na %>%
      group_by(variable) %>%
      mutate(value = na.approx(value, na.rm = FALSE))
    
    plot = ggplot(data_with_na) +
      geom_hline(yintercept=75, linetype="dashed", color=SITTMAT_colors[3]) +
      geom_text(aes(x=3, y=targetLabel_y, label="SITT-MAT Target"),
                color=SITTMAT_colors[3], size=3, family=font, fontface="bold",
                lineheight=0.75, hjust=0.8) +
      geom_line(aes(x=grouping_period, color=variable, y=value), linewidth=1) +
      labels_geom +
      geom_vline(xintercept = last_quarter + 4.6, linetype = "dotted", color = SITTMAT_colors[4]) +
      geom_text(aes(x = last_quarter+3.4, y = 50, label = "Previous \nQuarters"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
      geom_text(aes(x = last_quarter+5.6, y = 50, label = "This \nQuarter"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
      scale_color_manual(values=c(SITTMAT_colors[1], SITTMAT_colors[1]), labels=MOUD_measure_labels[2]) +
      scale_x_continuous(labels = x_labels, breaks = seq(min_x, max_x, by = 1)) +
      scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size=7.3, family=font, color="black"),
            axis.text.x = element_text(angle=-20, vjust=0.5, hjust=0.3),
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
}

# Line plot for percent new patients prescribed MOUD with 2+ clinical visits in 34 days
make_2Visits = function(id, save=F, labels=T){
  start_date <- min(report_data$date)
  
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% c("reaim_c3p", "reaim_c3", "reaim_b5"),
           date <= date_filter) %>%
    mutate(
      months_since_start = interval(start_date, date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date)
    )
  if (id == "id45"){
    plot_data <- report_data %>%
        filter(program_id == id, variable %in% c("reaim_c3p", "reaim_c3", "reaim_b5"),
             date <= date_filter) %>%
      mutate(variable = ifelse(variable == "reaim_c3p", "new_percentage_c3p", variable))%>%
      pivot_wider(id_cols=c(program_id, date),
                names_from=variable,
                values_from=value) 
    if(labels){
      labels_geom = geom_label(aes(x=date, y=new_percentage_c3p, color=program_id, label=paste(reaim_c3,"/",reaim_b5)), 
                               label.padding=unit(0.1, "lines"), size=3, show.legend=F)
    } else {
      labels_geom = geom_blank()
    }
    print(plot_data)

    plot = ggplot(plot_data, aes(x=date, y=new_percentage_c3p)) +
      geom_line(aes(color=program_id), linewidth=1, show.legend=F) +
      labels_geom +
      scale_color_manual(values=SITTMAT_colors) +
      scale_x_date(date_breaks="month", date_labels="%b-%y") +
      scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.margin = margin(t=5,r=13, b=3),
            axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.3),
            axis.title = element_blank(),
            axis.text = element_text(size=7.3, family=font, color="black")) +
      coord_cartesian(clip="off")
    
    if(save){
      filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_2Visits_", month_for_filenames, ".png")
      ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
    }
    
    return(plot)
    
  }
  else{
    last_month <- as.numeric(format(max(quarter_data$date), "%m"))
    last_quarter <- max(quarter_data$quarter)
    
    sum_data <- quarter_data %>%
      mutate(grouping_period = if_else(quarter == last_quarter, 
                                       (as.numeric(last_quarter) + (as.numeric(months_since_start) - 15) ),
                                       as.numeric(quarter))) %>%
      group_by(grouping_period, variable) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
    
   
    wide_data <- pivot_wider(sum_data, names_from = variable, values_from = value)
    plot_data <- wide_data %>%
      mutate(
        new_percentage_c3p = ifelse(reaim_c3 == 0 & reaim_b5 == 0, 0, (reaim_c3 / reaim_b5) * 100)
      )
    
    if (id %in% c("id17")) { # for these ids, we don't have data for the first two quarters
      plot_data[1, 'reaim_c3'] <- 0
      plot_data[1, 'new_percentage_c3p'] <- 0
      plot_data[2, 'reaim_c3'] <- 0
      plot_data[2, 'new_percentage_c3p'] <- 0
    }
    
    if (id %in% c("id35", "id39", "id63")) { # for these ids, we don't have data for the first three quarters
      plot_data[1, 'reaim_c3'] <- 0
      plot_data[1, 'new_percentage_c3p'] <- 0
      plot_data[2, 'reaim_c3'] <- 0
      plot_data[2, 'new_percentage_c3p'] <- 0
      plot_data[3, 'reaim_c3'] <- 0
      plot_data[3, 'new_percentage_c3p'] <- 0
    }
    if (id %in% c("id15", "id44")) { # for this id, we don't have data for the first four quarters
      plot_data[1, 'reaim_c3'] <- 0
      plot_data[1, 'new_percentage_c3p'] <- 0
      plot_data[2, 'reaim_c3'] <- 0
      plot_data[2, 'new_percentage_c3p'] <- 0
      plot_data[3, 'reaim_c3'] <- 0
      plot_data[3, 'new_percentage_c3p'] <- 0
      plot_data[4, 'reaim_c3'] <- 0
      plot_data[4, 'new_percentage_c3p'] <- 0
        }

    # Function to insert NA rows between specific rows
    insert_na_rows <- function(df, insert_positions, num_na_rows) {
      for (pos in sort(insert_positions, decreasing = TRUE)) {
        new_rows <- data.frame(
          grouping_period = rep(NA, num_na_rows),
          reaim_b5 = rep(NA, num_na_rows),
          reaim_c3 = rep(NA, num_na_rows),
          reaim_c3p = rep(NA, num_na_rows),
          new_percentage_c3p = rep(NA, num_na_rows)
        )
        df <- rbind(df[1:pos, ], new_rows, df[(pos+1):nrow(df), ])
      }
      return(df)
    }
    
    insert_positions <- c(1, 2, 3, 4, 5)  # Positions to insert NA rows
    num_na_rows <- 1  # Number of NA rows to insert at each position
    
    data_with_na <- insert_na_rows(plot_data, insert_positions, num_na_rows)
    data_with_na$grouping_period <- seq(0, length.out = nrow(data_with_na), by = 1)
    
    data_with_na <- data_with_na %>% # Interpolate NA values using linear interpolation for new_percentage_c3p
      mutate(new_percentage_c3p = na.approx(new_percentage_c3p, na.rm = FALSE))
    
    data_labels <- data_with_na %>%
      filter(!is.na(reaim_c3) & !is.na(reaim_b5))
    
    data_labels$label <- paste0(data_labels$reaim_c3, "/", data_labels$reaim_b5)
    
    
    if (id %in% c("id17")) { # for these ids, we don't have data for the first three quarters
      data_labels$label[1:2] <- "NA"
    }
    if (id %in% c("id35", "id39", "id63")) { # for these ids, we don't have data for the first three quarters
      data_labels$label[1:3] <- "NA"
    }
    if (id %in% c("id15", "id44")) { # for this id, we don't have data for the first four quarters
      data_labels$label[1:4] <- "NA"
    }
    
    min_x <- min(data_with_na$grouping_period, na.rm = TRUE)
    max_x <- max(data_with_na$grouping_period, na.rm = TRUE)
    
    plot = ggplot(data_with_na, aes(x=grouping_period, y=new_percentage_c3p)) +
      geom_line(aes(color="black"), linewidth=1, show.legend=F) +
      geom_label(data = data_labels, aes(x = grouping_period, y = new_percentage_c3p,
                                         label = label), color=SITTMAT_colors[1], label.padding=unit(0.1, "lines"),size=3, show.legend=F)+
      scale_color_manual(values=SITTMAT_colors) +
      geom_vline(xintercept = last_quarter +4.5, linetype = "dotted", color = SITTMAT_colors[4]) +
      geom_text(aes(x = last_quarter+3.5, y = 75, label = "Previous \nQuarters"),  color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
      geom_text(aes(x = last_quarter+5.5, y = 75, label = "This \nQuarter"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
      scale_x_continuous(labels = x_labels, breaks = seq(min_x, max_x, by = 1)) +
      scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.margin = margin(t=5,r=13, b=3),
            axis.text.x = element_text(angle=-20, vjust=0.5, hjust=0.3),
            axis.title = element_blank(),
            axis.text = element_text(size=7.3, family=font, color="black")) +
      coord_cartesian(clip="off")
    
    if(save){
      filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_2Visits_", month_for_filenames, ".png")
      ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
    }
    
    return(plot)
  }
}

# Bar plot for percent of patients discharged by linkage status
make_referralLinkage = function(id, save=F){
  referral_vars = c("reaim_c6", "reaim_c7", "reaim_c8", "reaim_c6p", "reaim_c7p", "reaim_c8p")
  referral_labels = list("new_percentage_c6p"="Referred and linkage confirmed",
                         "new_percentage_c7p"="Referred but did not confirm linkage",
                         "new_percentage_c8p"="No referral")
  
  start_date <- min(report_data$date)
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% referral_vars, date <= date_filter) %>%
    mutate(
      months_since_start = interval(start_date, date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date),
    )
  
  last_month <- as.numeric(format(max(quarter_data$date), "%m"))
  last_quarter <- max(quarter_data$quarter)
  
  # Adjusting summarization based on whether it's the last quarter or not
  sum_data <- quarter_data %>%
    mutate(grouping_period = if_else(quarter == last_quarter, 
                                     (as.numeric(last_quarter) + (as.numeric(months_since_start) - 15) ),
                                     as.numeric(quarter))) %>%
    group_by(grouping_period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  wide_data <- pivot_wider(sum_data, names_from = variable, values_from = value)
  wide_data <- wide_data %>%
    mutate(
      denom = reaim_c6 + reaim_c7 + reaim_c8,
      new_percentage_c6p = ifelse(reaim_c6 == 0 & denom == 0, 0, (reaim_c6 / denom) * 100),
      new_percentage_c7p = ifelse(reaim_c7 == 0 & denom == 0, 0, (reaim_c7 / denom) * 100),
      new_percentage_c8p = ifelse(reaim_c8 == 0 & denom == 0, 0, (reaim_c8 / denom) * 100)
    )%>%
    select(grouping_period, new_percentage_c6p, new_percentage_c7p, new_percentage_c8p)
  
  # Function to insert NA rows between specific rows
  insert_na_rows <- function(df, insert_positions, num_na_rows) {
    for (pos in sort(insert_positions, decreasing = TRUE)) {
      new_rows <- data.frame(
        grouping_period = rep(NA, num_na_rows),
        new_percentage_c6p = rep(NA, num_na_rows),
        new_percentage_c7p = rep(NA, num_na_rows),
        new_percentage_c8p = rep(NA, num_na_rows)
      )
      df <- rbind(df[1:pos, ], new_rows, df[(pos+1):nrow(df), ])
    }
    return(df)
  }
  
  insert_positions <- c(1, 2, 3, 4, 5)  # Positions to insert NA rows
  num_na_rows <- 1  # Number of NA rows to insert at each position
  
  data_with_na <- insert_na_rows(wide_data, insert_positions, num_na_rows)
  data_with_na$grouping_period <- seq(0, length.out = nrow(data_with_na), by = 1)
  print(data_with_na)
  #  Prepare plot data
  plot_data <- data_with_na %>%
    pivot_longer(cols = c(new_percentage_c6p, new_percentage_c7p, new_percentage_c8p),
                 names_to = "variable", values_to = "value") %>%
    filter(variable %in% c("new_percentage_c6p", "new_percentage_c7p", "new_percentage_c8p"))
  
  min_x <- min(plot_data$grouping_period, na.rm = TRUE)
  max_x <- max(plot_data$grouping_period, na.rm = TRUE)
  
  red_yellow_green <- c("new_percentage_c8p" = "#e71d36", "new_percentage_c7p" = "#ff9f1c", "new_percentage_c6p" = "#5d9818")

  plot = ggplot(plot_data, aes(x=grouping_period, y=value)) +
    geom_col(aes(fill=variable), width=0.3) +
    scale_fill_manual(labels=~referral_labels[.x], values=red_yellow_green, guide=guide_legend(reverse=T, override.aes=list(shape=15))) +
    scale_x_continuous(labels = x_labels, breaks = seq(min_x, max_x, by = 1)) +
    scale_y_continuous(labels=~paste0(.x,"%")) +
    geom_vline(xintercept = last_quarter +4.6, linetype = "dotted", color = SITTMAT_colors[4]) +
    geom_text(aes(x = last_quarter+3, y = 95, label = "Previous \nQuarters"),  color = SITTMAT_colors[4], size=1.8, family=font, fontface="bold") +
    geom_text(aes(x = last_quarter+7, y = 95, label = "This \nQuarter"), color = SITTMAT_colors[4], size=1.8, family=font, fontface="bold") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=7.3, family=font, color="black"),
          axis.text.x = element_text(angle=-20, vjust=.3, hjust=0.3),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(3, "pt"),
          legend.text = element_text(size=8, family=font, color="black"),
          legend.box.margin = margin(-12,-10,-8,-10)) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_referralLinkage_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}


########
# IMAT #
########

# Line plot for IMAT subscale & total by month
make_imat = function(id, save=F, imat_dates=NULL){
  # Need to set sizing based on tall/short desired size
  #   imat_dims specifies final plot size in inches (width, height)
  #   imat_text_size specifies text sizes (all other text, x axis labels)
  if(imat_size=="short"){
    imat_dims = c(7.5,2.3)
    imat_text_size = c(8,7.8)
  } else {
    imat_dims = c(7.5,5)
    imat_text_size = c(11, 8.2)
  }
  
  imat_labels = list("imat_d1" = "Infrastructure",
                     "imat_d2" = "Clinic\nCulture &\nEnvironment",
                     "imat_d3" = "Patient\nIdentification\n& Initiating\nCare",
                     "imat_d4" = "Care Delivery\n& Treatment\nResponse\nMonitoring",
                     "imat_d5" = "Care\nCoordination",
                     "imat_d6" = "Workforce",
                     "imat_d7" = "Staff Training\n& Development",
                     "imat_s1" = "Low Barrier\nCare",
                     "imat_total" = "Total")
  imat_scale_labels = c("1: Not\nIntegrated", "2", "3: Partially\nIntegrated",
                        "4", "5: Fully\nIntegrated")
  
  if (id=="all") {
    plot_data = report_data %>%
      filter(startsWith(variable, prefix="imat"), date %in% imat_dates) %>%
      group_by(date, variable) %>%
      summarize(value = mean(value, na.rm=T), .groups="keep") %>%
      # Dates need to be factors for legend
      mutate(display_date = factor(format(date, "%b-%y"), levels=imat_dates, ordered=T)) 
  } else {
    plot_data = report_data %>%
      filter(program_id==id, startsWith(variable, "imat")) %>%
      # Select the first (baseline) survey and the most recent (comparison) survey
      filter(date==max(date) | date==min(date)) %>%
      arrange(complete_display_date) %>%
      # Dates need to be factors for legend
      mutate(display_date = factor(format(ymd(complete_display_date), "%b-%y")))
    ordered_display_dates = plot_data %>%
      arrange(date) %>%
      select(display_date) %>%
      unique() %>%
      pull()
    plot_data$display_date = factor(plot_data$display_date, levels=ordered_display_dates, ordered=T)
    
    ## ID04 and id03 business ##
    # plot_data = report_data %>%
    #   filter((program_id == id | program_id == "id04") & startsWith(variable, "imat")) %>%
    #   group_by(program_id) %>%
    #   filter(date == max(date) | date == min(date)) %>%
    #   ungroup() %>%
    #   arrange(complete_display_date) %>%
    #   # Dates need to be factors for legend
    #   mutate(display_date = factor(format(ymd(complete_display_date), "%b-%y")))
    # 
    # # Get the max date for id 04
    # max_date_id04 = report_data %>%
    #   filter(program_id == "id04") %>%
    #   summarize(max_date = max(date)) %>%
    #   pull(max_date)
    # 
    # # Add data from the max date of id 04 to plot_data
    # plot_data = plot_data %>%
    #   filter(!(program_id == "id04" & date != max_date_id04))
    # 
    # ordered_display_dates = plot_data %>%
    #   arrange(date) %>%
    #   select(display_date) %>%
    #   unique() %>%
    #   pull()
    # 
    # plot_data$display_date = factor(plot_data$display_date, levels = ordered_display_dates, ordered = TRUE)
  }
  
  plot = ggplot(plot_data, aes(x=variable, y=value, group=display_date)) +
    geom_line(aes(color=display_date), linewidth=2) +
    scale_color_manual(values=SITTMAT_colors) +
    scale_x_discrete(labels=~imat_labels[.x]) +
    scale_y_continuous(limits=c(1,5), breaks=1:5, labels=imat_scale_labels) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size=imat_text_size[2]),
          axis.text.y = element_text(size=imat_text_size[1]),
          axis.text = element_text(family=font, color="black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(-10,2,20,2),
          legend.box.margin = margin(5,-10,-10,-10),
          legend.position = "top",
          legend.text = element_text(size=imat_text_size[1], family=font, color="black"),
          legend.title = element_blank()) +
    coord_cartesian(clip="off")
  
  # Annotate plot to specify which variables are dimensions
  dim_line = linesGrob(x=c(.4,10.5), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  dim_text = textGrob(label="DIMENSIONS", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  sub_line = linesGrob(x=c(14.25,15.9), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  sub_text = textGrob(label="SUBSCALE", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  total_line = linesGrob(x=c(16.2,17.85), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  total_text = textGrob(label="TOTAL", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  
  if(imat_size=="short"){
    plot = plot +
      annotation_custom(dim_line, ymin=-1.33, ymax=-1.33, xmin=.2, xmax=.9) +
      annotation_custom(dim_text, ymin=-2.1, ymax=-1.1, xmin=1.5, xmax=6.5) +
      annotation_custom(sub_line, ymin=-1.33, ymax=-1.33, xmin=.5, xmax=.999) +
      annotation_custom(sub_text, ymin=-2.1, ymax=-1.1, xmin=7.51, xmax=8.51) +
      annotation_custom(total_line, ymin=-1.33, ymax=-1.33, xmin=.5, xmax=.999) +
      annotation_custom(total_text, ymin=-2.1, ymax=-1.1, xmin=8.51, xmax=9.51)
  } else {
    plot = plot +
      annotation_custom(dim_line, ymin=0.12, ymax=0.12, xmin=.2, xmax=.9) +
      annotation_custom(dim_text, ymin=-0.5, ymax=0.5, xmin=1.5, xmax=6.5) +
      annotation_custom(sub_line, ymin=0.12, ymax=0.12, xmin=.5, xmax=.999) +
      annotation_custom(sub_text, ymin=-0.5, ymax=0.5, xmin=7.51, xmax=8.51) +
      annotation_custom(total_line, ymin=0.12, ymax=0.12, xmin=.5, xmax=.999) +
      annotation_custom(total_text, ymin=-0.5, ymax=0.5, xmin=8.51, xmax=9.51)
  }
  
  if(save){
    filepath = here::here(paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_imat_", month_for_filenames, ".png"))
    ggsave(here::here(filepath), plot, width=imat_dims[1], height=imat_dims[2], units="in")
  }
  
  return(plot)
}

##########################
# | USE THIS FOR PLOTS | #
# v  Runner functions  v #
##########################

# Produce all plots
#   If show_plots=T, it will ask for user input before showing next plot
make_allPlots = function(id, save_plots=F, show_plots=T){
  print(paste0("Creating plots for ", id))
  p = make_MOUDplot(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_2Visits(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_referralLinkage(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_imat(id, save=save_plots)
  if(show_plots) print(p)
}
# Define which sites you want to produce plots for
#programs = sort(unique(report_data[grepl("reaim_b5p",report_data$variable),"program_id"]))
# programs = paste0("id",c(64:73))
programs = c('id04')
reaim_dims=c(5.4,2.2)
# Iterate through programs & produce all plots
# imat_size="tall"
for(program in programs){
  make_MOUDplot(program, save=T)
  # make_referralLinkage(program, save=T)
  # make_2Visits(program, save=T)
  # make_imat(program, save=T)
}


# # Produce only IMAT (for sites that don't have REAIM data)
# #imat_programs = sort(unique(report_data$program_id[!report_data$program_id%in%programs]))
# imat_programs = paste0("id",c(64:73))
# imat_size="tall"
# for(program in imat_programs){
#   print(paste0("Creating IMAT plot for ", program))
#   make_imat(program, save=T)
# }
