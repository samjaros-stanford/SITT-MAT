library(ggdendro)
library(ggplot2)
library(grid)
library(lubridate)
library(tidyverse)

############
# Settings #
############
# Long CDI location
cdi_long_file = "data/current_cdi_item.rds"
# Current RE-AIM/IMAT location
imat_reaim_file = "data/current_reaim-imat.rds"
# Include "Does not apply (8)" in the Likert value regression?
#   NOTE: Barriers/facilitators calculation is unaffected
#           Instead, values from 00_calc_cdi.R are used
#   Accepts 1-7 integer (change to value) or NA_integer_ (exclude)
dna = NA_integer_
# Formatted Names
subgroup_names = c("Enabiling\nExternal\nPolicies", "Community\nSupport", 
                   "Leadership\nSupport", "Health\nEquity", "Staff Perception\nof MOUD", 
                   "Feasibility", "Readiness\nto Change", "Individualized\nCare")
level_names = c("Inner", "Outer", "Patient", "Staff")

###################
# Linear Modeling #
###################

### Data

# Get "exposures" for linear model
#   1) Mean Likert value for each site
#   2) Percent barriers for each site
#   3) Percent facilitators for each site
cdi = readRDS(cdi_long_file) %>%
  ### Get mean Likert values
  select(program_id, value) %>%
  # Correct "Does not apply (8)" answers
  mutate(scaled_value = if_else(value==8, dna, value)) %>%
  # Get mean value in each program
  group_by(program_id) %>%
  summarize(mean_likert = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  ungroup() %>%
  ### Join in mean percent values
  left_join(readRDS(cdi_long_file) %>%
              select(program_id, cat, num) %>%
              # Get percent of each category
              group_by(program_id, cat) %>%
              summarize(num = sum(num),
                        .groups = "keep") %>%
              group_by(program_id) %>%
              mutate(percent = round(num/sum(num)*100, 2)) %>%
              # Flip to get percent of barriers, neutral, and facilitators
              pivot_wider(id_cols = program_id,
                          names_from = cat,
                          values_from = percent,
                          values_fill = 0.00),
            by = "program_id")

# Get "outcomes" for linear model
#   1) Count of MOUD patients in March 2022 (reaim_b4)
#   2) Percent of OUD patients on MOUD in March 2022 (reaim_b4p)
outcomes = readRDS(reaim_imat_file) %>%
  # Get only relevant rows
  filter(date == my("March 2022") & startsWith(variable, "reaim_b4")) %>%
  # Get variables as columns
  pivot_wider(id_cols = program_id,
              names_from = variable,
              values_from = value) %>%
  # Human-friendly names
  rename(count_MOUD = reaim_b4, pct_MOUD = reaim_b4p)

model_data = left_join(cdi, outcomes, by="program_id")

### Analysis

models = expand.grid(outcome = c("count_MOUD", "pct_MOUD"),
                     exposure = c("mean_likert", "barriers", "facilitators"))

# Calculate linear model and report estimate and p-value
do.model = function(x) {
  # Glm using first value of x as the outcome and second values as exposure
  # Extract estimate and p-value of the exposure
  coef(summary(glm(get(x[1])~get(x[2]), data = model_data, family="gaussian")))[2,c(1,4)]
}

results = cbind(models, t(apply(models, 1, do.model)))

####################
# Cluster Analysis #
####################

### Data

# Get subgroup mean Likert values for each site
#   Program IDs will be stored as rownames for easier conversion to matrix
subgroup_likert = readRDS(cdi_long_file) %>%
  select(program_id, value, subscale) %>%
  # Correct "Does not apply (8)" answers
  mutate(scaled_value = if_else(value==8, dna, value)) %>%
  # Get mean for each program & subscale
  group_by(program_id, subscale) %>%
  summarise(mean_likert = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  ungroup() %>%
  # Get variables as columns
  pivot_wider(id_cols = program_id,
              names_from = subscale,
              values_from = mean_likert) %>%
  # Make program_id into the rownames
  column_to_rownames("program_id") %>%
  # Scale columns to have a mean of 0 and a variance of 1
  mutate(across(.fns=scale)) %>%
  # Remove sites with dupliate answers
  distinct()

# Get level mean Likert values for each site
#   Program IDs will be stored as rownames for easier conversion to matrix
level_likert = readRDS(cdi_long_file) %>%
  select(program_id, value, item) %>%
  # Get item value for each question
  mutate(level = sapply(strsplit(item, "_"), "[", 2)) %>%
  # Correct "Does not apply (8)" answers
  mutate(scaled_value = if_else(value==8, dna, value)) %>%
  # Get mean for each program & level
  group_by(program_id, level) %>%
  summarise(mean_likert = mean(scaled_value, na.rm=T),
            .groups = "keep") %>%
  ungroup() %>%
  # Get variables as columns
  pivot_wider(id_cols = program_id,
              names_from = level,
              values_from = mean_likert) %>%
  # Make program_id into the rownames
  column_to_rownames("program_id") %>%
  # Scale columns to have a mean of 0 and a variance of 1
  mutate(across(.fns=scale))

### Analysis

# Subgroup hierarchical clustering
subgL_dd = as.dist(1-cor(t(as.matrix(subgroup_likert)), use="complete.obs"))
subgL_complete = as.dendrogram(hclust(subgL_dd, method="complete"))

## Dendrogram plot
subgL_dendro = ggdendrogram(data = subgL_complete, rotate = T) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30))

## Heat plot
# Get data in long format
long_subgL = subgroup_likert %>%
  rownames_to_column("program_id") %>%
  mutate(program_id = as.factor(program_id)) %>%
  pivot_longer(cols = -program_id,
               names_to = "subgroup",
               values_to = "value")
# Get order of program ID's to match
subgL_order = order.dendrogram(subgL_complete)
long_subgL$program_id = factor(x = long_subgL$program_id,
                               levels = rownames(subgroup_likert)[subgL_order],
                               ordered = T)
# Create plot
subgL_heat = ggplot(long_subgL, aes(x=subgroup, y = program_id)) +
  geom_tile(aes(fill=value)) +
  scale_fill_gradient2(name = "Scaled Mean\nLikert") +
  scale_x_discrete(name = "Subscales", labels = subgroup_names) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.justification = c(0.01, 1),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 26),
        legend.key.size = unit(25, "pt"),
        legend.box.spacing = unit(1, "pt"),
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color=NA),
        text = element_text(size = 28))

# Transparent background
png("figures/cdi_subgroup_clustering_transparent.png", bg=NA, height = 960, width = 1400)
grid.newpage()
print(subgL_heat,
      vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(subgL_dendro,
      vp = viewport(x = 0.89, y = 0.517, width = 0.2, height = 0.838))
grid.text("Hierarchical Clustering of Sites by CDI Subscale",
          x = 0.625, y = 0.97,
          gp = gpar(fontsize = 48))
dev.off()
# White background
png("figures/cdi_subgroup_clustering.png", height = 960, width = 1400)
grid.newpage()
print(subgL_heat,
      vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(subgL_dendro,
      vp = viewport(x = 0.89, y = 0.517, width = 0.2, height = 0.838))
grid.text("Hierarchical Clustering of Sites by CDI Subscale",
          x = 0.625, y = 0.97,
          gp = gpar(fontsize = 48))
dev.off()


# Level heirarchical clustering
levelL_dd = as.dist(1-cor(t(as.matrix(level_likert)), use="complete.obs"))
levelL_complete = as.dendrogram(hclust(levelL_dd, method="complete"))

## Dendrogram plot
levelL_dendro = ggdendrogram(data = levelL_complete, rotate = T) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 20))

## Heat plot
# Get data in long format
long_levelL = level_likert %>%
  rownames_to_column("program_id") %>%
  mutate(program_id = as.factor(program_id)) %>%
  pivot_longer(cols = -program_id,
               names_to = "level",
               values_to = "value")
# Get order of program ID's to match
levelL_order = order.dendrogram(levelL_complete)
long_levelL$program_id = factor(x = long_levelL$program_id,
                               levels = rownames(level_likert)[levelL_order],
                               ordered = T)
# Create plot
levelL_heat = ggplot(long_levelL, aes(x=level, y = program_id)) +
  geom_tile(aes(fill=value)) +
  scale_fill_gradient2(name = "Scaled Mean\nLikert") +
  scale_x_discrete(name = "Question Level", labels = level_names) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color=NA),
        text = element_text(size = 20))

# Transparent background
png("figures/cdi_level_clustering_transparent.png", bg=NA, height = 960, width = 800)
grid.newpage()
print(levelL_heat,
      vp = viewport(x = 0.35, y = 0.5, width = 0.7, height = 1.0))
print(levelL_dendro,
      vp = viewport(x = 0.85, y = 0.49, width = 0.3, height = 0.93))
dev.off()
# White background
png("figures/cdi_level_clustering.png", height = 960, width = 800)
grid.newpage()
print(levelL_heat,
      vp = viewport(x = 0.35, y = 0.5, width = 0.7, height = 1.0))
print(levelL_dendro,
      vp = viewport(x = 0.85, y = 0.49, width = 0.3, height = 0.93))
dev.off()

