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
font = "Century Gothic"
month_for_filenames = "May24"
reaim_dims = c(5.5,1.3) # in inches, normally 5.49, 1.3
imat_size = "short" #tall or short
# MOUD_measure_labels = list("reaim_b5p"="MOUD within 30 days",
#                            "reaim_c1p"="MOUD within 72 hours")
MOUD_measure_labels = list("new_percentage_b5p"="MOUD within 30 days",
                           "new_percentage_c1p"="MOUD within 72 hours")

##########
# RE-AIM #
##########

# Line plot for percent patients diagnosed with OUD on MOUD within 30 days & 72h
make_MOUDplot = function(id, save=F, labels=T){
  # Create custom quarters
  start_date <- min(report_data$date, na.rm= TRUE)
  # Calculate quarter data with an additional 'month' column for later use
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% c("reaim_c1p", "reaim_c1", "reaim_b2")) %>%
    mutate(
      months_since_start = interval(as.Date(start_date), date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date)
    )
  
  # Handle position of "SITT-MAT Target" line
  targetLabel_x = max(quarter_data$date)
  targetLabel_y = ifelse(quarter_data[quarter_data$date==targetLabel_x & quarter_data$variable=="reaim_c1p","value"]>75,65,85)
  # For manual adjustment of the target label, when needed
  targetLabel_x = ymd("2023-1-01")
  #targetLabel_y = 65
  
  last_month <- as.numeric(format(max(quarter_data$date), "%m"))
  last_quarter <- max(quarter_data$quarter)

  sum_data <- quarter_data %>%
    mutate(grouping_period = if_else(quarter == last_quarter, 
                                     (as.numeric(last_quarter) + (as.numeric(months_since_start) - 15) ),
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
    label_data$grouping_period = c(0,2,4,6,8,9,10,11)
    # if not quarters, change x=quarter to x=date
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
  
  insert_positions <- c(1, 2, 3, 4)  # Positions to insert NA rows
  num_na_rows <- 1  # Number of NA rows to insert at each position
    data_with_na <- insert_na_rows(plot_data, insert_positions, num_na_rows)
  # Reset the `grouping_period` column to be a sequence of integers
  data_with_na$grouping_period <- seq(0, length.out = nrow(data_with_na), by = 1)
  
  x_labels <- c("Sep22 -\nNov22", "",  "Dec22 - \nFeb23", "",   "Mar23 - \nMay23",  "", "Jun23 - \nAug23","", "Sep23 - \nNov23", "Dec23", "Jan24", "Feb24")
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
    geom_vline(xintercept = last_quarter + 3.6, linetype = "dotted", color = SITTMAT_colors[4]) +
    geom_text(aes(x = last_quarter+3, y = 55, label = "Previous \nQuarters"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    geom_text(aes(x = last_quarter+4.1, y = 55, label = "This \nQuarter"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    scale_color_manual(values=c(SITTMAT_colors[1], SITTMAT_colors[1]), labels=MOUD_measure_labels[2]) +
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

# Line plot for percent new patients prescribed MOUD with 2+ clinical visits in 34 days
make_2Visits = function(id, save=F, labels=T){
  start_date <- min(report_data$date)
  
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% c("reaim_c3p", "reaim_c3", "reaim_b5")) %>%
    mutate(
      months_since_start = interval(start_date, date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date)
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
  
  # Spread data to wide format
  wide_data <- pivot_wider(sum_data, names_from = variable, values_from = value)
  # Calculate new percentage
  plot_data <- wide_data %>%
    mutate(
      new_percentage_c3p = ifelse(reaim_c3 == 0 & reaim_b5 == 0, 0, (reaim_c3 / reaim_b5) * 100)
    )
  

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
  
  insert_positions <- c(1, 2, 3, 4)  # Positions to insert NA rows
  num_na_rows <- 1  # Number of NA rows to insert at each position
  
  data_with_na <- insert_na_rows(plot_data, insert_positions, num_na_rows)
  data_with_na$grouping_period <- seq(0, length.out = nrow(data_with_na), by = 1)
  # Interpolate NA values using linear interpolation for new_percentage_c3p
  data_with_na <- data_with_na %>%
    mutate(new_percentage_c3p = na.approx(new_percentage_c3p, na.rm = FALSE))

  data_labels <- data_with_na %>%
    filter(!is.na(reaim_c3) & !is.na(reaim_b5))
  
  x_labels <- c("Sep22 -\nNov22", "",  "Dec22 - \nFeb23", "",   "Mar23 - \nMay23",  "", "Jun23 - \nAug23","", "Sep23 - \nNov23", "Dec23", "Jan24", "Feb24")
  min_x <- min(data_with_na$grouping_period, na.rm = TRUE)
  max_x <- max(data_with_na$grouping_period, na.rm = TRUE)
  

  plot = ggplot(data_with_na, aes(x=grouping_period, y=new_percentage_c3p)) +
    geom_line(aes(color="black"), linewidth=1, show.legend=F) +
    geom_label(data = data_labels, aes(x = grouping_period, y = new_percentage_c3p,
                                       label = paste0(reaim_c3, "/", reaim_b5)), color=SITTMAT_colors[1], label.padding=unit(0.1, "lines"),size=3, show.legend=F)+
    scale_color_manual(values=SITTMAT_colors) +
    geom_vline(xintercept = last_quarter +3.6, linetype = "dotted", color = SITTMAT_colors[4]) +
    geom_text(aes(x = last_quarter+2.7, y = 80, label = "Previous \nQuarters"),  color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    geom_text(aes(x = last_quarter+4.2, y = 80, label = "This \nQuarter"), color = SITTMAT_colors[4], size=2.5, family=font, fontface="bold") +
    scale_x_continuous(labels = x_labels, breaks = seq(min_x, max_x, by = 1)) +
    scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t=5,r=13, b=3),
          axis.text.x = element_text(angle=-12, vjust=0.5, hjust=0.3),
          axis.title = element_blank(),
          axis.text = element_text(size=7.3, family=font, color="black")) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_2Visits_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

# Bar plot for percent of patients discharged by linkage status
make_referralLinkage = function(id, save=F){
  referral_vars = c("reaim_c6p", "reaim_c7p", "reaim_c8p")
  referral_labels = list("reaim_c6p"="Referred and linkage confirmed",
                         "reaim_c7p"="Referred but did not confirm linkage",
                         "reaim_c8p"="No referral")
  
  # plot_data = report_data %>%
  #   filter(program_id==id, variable %in% referral_vars) %>%
  #   mutate(variable = factor(variable, levels=rev(referral_vars), ordered=T))
  # 
  # print(plot_data)
  
  # Create custom quarters
  start_date <- min(report_data$date)
  # Calculate quarter data with an additional 'month' column for later use
  quarter_data <- report_data %>%
    filter(program_id == id, variable %in% referral_vars) %>%
    mutate(
      date = as.Date(date),
      months_since_start = interval(as.Date(start_date), date) / months(1),
      quarter = floor(months_since_start / 3),
      month = month(date)
    )
  
  last_month <- format(max(quarter_data$date), "%m")
  last_quarter <- max(quarter_data$quarter)
  
  # Adjusting summarization based on whether it's the last quarter or not
  sum_data <- quarter_data %>%
    mutate(grouping_period = if_else(quarter == last_quarter, (as.numeric(last_quarter) + (as.numeric(last_month) - as.numeric(month)) ), as.numeric(quarter))) %>%
    group_by(grouping_period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  # Spread data to wide format
  wide_data <- pivot_wider(sum_data, names_from = variable, values_from = value)
  # Calculate new percentages
  wide_data <- wide_data %>%
    mutate(
      new_percentage_c6p = (reaim_c1 / reaim_b2) * 100,
      new_percentage_c7p = (reaim_c1 / reaim_b2) * 100,
      new_percentage_c8p = (reaim_c1 / reaim_b2) * 100,
    )
  
  # Prepare plot data
  plot_data <- wide_data %>%
    pivot_longer(cols = c(new_percentage_c1p),
                 names_to = "variable", values_to = "value") %>%
    filter(variable %in% c("new_percentage_c1p"))
  
  plot = ggplot(plot_data, aes(x=date, y=value)) +
    geom_col(aes(fill=variable), width=10) +
    scale_fill_manual(labels=~referral_labels[.x], values=red_yellow_green, guide=guide_legend(reverse=T, override.aes=list(shape=15))) +
    scale_x_date(date_breaks="month", date_labels="%b-%y") +
    scale_y_continuous(labels=~paste0(.x,"%")) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=8, family=font, color="black"),
          axis.text.x = element_text(angle=-20, vjust=0, hjust=0.3),
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
programs = c("id35")
# programs = c("id26", "id38")
# reaim_dims=c(5.4,2.2)
# Iterate through programs & produce all plots
imat_size="tall"
for(program in programs){
  make_MOUDplot(program, save=T)
  # make_referralLinkage(program, save=T)
  make_2Visits(program, save=T)
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
