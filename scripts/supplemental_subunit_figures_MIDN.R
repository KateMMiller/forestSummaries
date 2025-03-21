#-------------------------------------
# Forest Data Summaries: Supplemental Figures Split by MIDN Subunit
#-------------------------------------

# Source Script -----------------------------------------------------------
# Imports/Libraries
library(forestMIDN)
library(forestTrends)
library(tidyverse)
library(sf)
library(vegan)
library(ggpubr)


if(!exists("path")){path = 'C:/01_NETN/Forest_Health/Data_Summaries/2024 Data Summaries/MIDN/'} #ces path
# if(!exists("path")){path = 'C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'} #kmm path


# Make sure local copy of DB is current or connect to server
importData()

# assign params to global env. for source files to find. Makes iterating easier.
# temp: so can run individual parks w/ .rmd file
##Comment out before running for all parks##
#assign params to global env. for source files to find. Makes iterating easier.
# 
midn_names <- read.csv("MIDN_MetaData.csv")
midn_params <- read.csv("MIDN_params.csv") # !!!! MUST UPDATE EVERY YEAR !!!!
park <<- "PETE"
subunit <<- "PETE_FIVE"
from <<- as.numeric(midn_params$from[midn_params$park == park])
from_4yr <<- as.numeric(midn_params$from_4yr[midn_params$park == park])
to <<- as.numeric(midn_params$to[midn_params$park == park])
cycle_latest <<- as.numeric(midn_params$cycle_latest[midn_params$park == park])

QAQC <<- FALSE
locType <<- 'VS'
#+++ NOTE: IF GET add_header_above error, need to update cycle_latest in MIDN_params.csv  +++
park_long <- midn_names$LongName[midn_names$ParkCode == park]
park_title <- midn_names$LongName_title[midn_names$ParkCode == park]
network_long <- midn_names$Network_long[midn_names$ParkCode == park]



# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$IsCanopyExclusion[VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$Genus == "Fraxinus"] <- TRUE

park_crs = ifelse(park %in% c("APCO", "BOWA"), 26917, 26918)

num_plots = case_when(park == "APCO" ~ 28,
                      park == "ASIS" ~ 24, # Will be 24
                      park == "BOWA" ~ 8,
                      park == "COLO" ~ 48,
                      park == "FRSP" ~ 104,
                      park == "GETT" ~ 33,
                      park == "GEWA" ~ 8,
                      park == "HOFU" ~ 16,
                      park == "PETE" ~ 52,
                      park == "RICH" ~ 31,
                      park == "SAHI" ~ 4,
                      park == "THST" ~ 8,
                      park == "VAFO" ~ 28
)

num_subunit_plots = case_when(subunit == "FRSP_FRED" ~ 28,
                              subunit == "FRSP_SPOT" ~ 28,
                              subunit == "FRSP_CHWILD" ~ 48,
                              subunit == "PETE_FIVE" ~ 24,
                              subunit == "PETE_EAST" ~ 28
)

plot_size = 400

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
parks <- c("APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", 
           "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO")

invisible(lapply(parks, function(x) {
  if(!dir.exists(paste0(path, x))){dir.create(paste0(path, x))}
})
)

new_path = paste0(path, park, "/", as.character(to), "/")

if(!dir.exists(new_path)){dir.create(new_path)}

folders <- c("figures", "tables")

invisible(lapply(folders, function(x) {
  if(!dir.exists(paste0(new_path, x))){dir.create(paste0(new_path, x))}
})
)

# Read in tree species groups for regen and tree pie charts
trspp_grps <- read.csv("NPS_tree_species_groups.csv")

#---- Params: Plot event lists ----
plotevs <- do.call(joinLocEvent, args_all) |> filter(ParkSubUnit == subunit)
plotevs_vs <- do.call(joinLocEvent, args_vs) |> filter(ParkSubUnit == subunit)
plotevs_4yr1 <- plotevs %>% filter(between(SampleYear, from_4yr, to)) |> filter(ParkSubUnit == subunit)

# Take the highest sample year when multiple panels were sampled within a year
# and only one of those panels is the most recent visit to that plot. 
plotevs_4yr <- plotevs_4yr1 |> group_by(ParkUnit, PanelCode, Plot_Name, IsQAQC) |> 
  slice_max(SampleYear) |> ungroup() 

#Reduced columns for Regen Debt code
plotevs_4yrB <- plotevs_4yr |> select(Plot_Name, SampleYear)

# vector of the EventIDs that represent the most recent visit to each plot
evs_4yr <- plotevs_4yr$EventID

#the lower the span the less smoothing, if higher more.
span <- 8/16 #less smoothing for confidence intervals
#span = 4/5

# Forest Summary Code Start ----------------------------------------------
new_path = paste0(path, park, "/", as.character(to), "/")

reg_sz_cols <- c("seed_15_30cm", "seed_30_100cm", "seed_100_150cm", "seed_p150cm", "sap_den") 

#---- Figure 3A Regen trends by size class ----
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
reg_vs <- do.call(joinRegenData, 
                  args = c(args_vs, speciesType = 'native', 
                           canopyForm = 'canopy', units = 'sq.m')) |> filter(!Plot_Name %in% "COLO-380") 

reg_size_cy <- reg_vs %>% group_by(Plot_Name, ParkSubUnit, cycle) %>% 
  summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = TRUE) %>% 
  left_join(plotevs_vs %>% select(Plot_Name, cycle),
            ., by = c("Plot_Name", "cycle")) 

reg_size_cy[reg_sz_cols][reg_size_cy[is.na(reg_sz_cols)]] <- 0

# Use forestTrends to generate loess-smoothed CIs
reg_smooth1 <- map_dfr(reg_sz_cols, 
                       ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                        span = span, num_reps = 250, chatty = TRUE) %>% 
                         mutate(size_class = .x)
)

# Smoother can result in negative values when really low. Converting those to 0 and converting CIs to NA
# A bit hand-wavy, but so is loess smoothing.
reg_smooth <- reg_smooth1
reg_smooth$estimate_orig <- reg_smooth$estimate
reg_smooth$estimate[reg_smooth$estimate_orig < 0] <- 0
reg_smooth$lower95[reg_smooth$estimate_orig < 0] <- NA
reg_smooth$upper95[reg_smooth$estimate_orig < 0] <- NA

# For plot labels
reg_colors <- c("seed_15_30cm" = "#D6D6FF", 
                "seed_30_100cm" = "#8F97E3", 
                "seed_100_150cm" = "#556CC9", 
                "seed_p150cm" = "#244EAD", 
                "sap_den" = "#05e646")

reg_smooth$size_class <- factor(reg_smooth$size_class, 
                                levels = c("seed_15_30cm", "seed_30_100cm", 
                                           "seed_100_150cm", "seed_p150cm", 
                                           "sap_den"))

# Set up cycle labels for figures
#midn1 <- c("FRSP", "PETE", "RICH")
midn1_labs <- c("1" = "Cycle 1: 2007 \u2013 2010",
                "2" = "Cycle 2: 2011 \u2013 2014",
                "3" = "Cycle 3: 2015 \u2013 2018",
                "4" = "Cycle 4: 2019 \u2013 2022",
                "5" = "Cycle 5: 2023 \u2013 2024")
#midn2 <- c("APCO", "BOWA", "GETT", "HOFU", "VAFO")
midn2_labs <- c("1" = "Cycle 1: 2007 \u2013 2010",
                "2" = "Cycle 2: 2011 \u2013 2014",
                "3" = "Cycle 3: 2015 \u2013 2018",
                "4" = "Cycle 4: 2019 \u2013 2023",
                "5" = "Cycle 5: 2024")
#ncbn <- c("GEWA", "THST")
ncbn_labs <- c("1" = "Cycle 1: 2008 \u2013 2011",
               "2" = "Cycle 2: 2012 \u2013 2015",
               "3" = "Cycle 3: 2016 \u2013 2019",
               "4" = "Cycle 4: 2021 \u2013 2023",
               "5" = "Cycle 5: 2024")

colo_labs <- c("1" = "Cycle 1: 2011 \u2013 2014",
               "2" = "Cycle 2: 2015 \u2013 2018",
               "3" = "Cycle 3: 2019 \u2013 2023",
               "4" = "Cycle 4: 2024")

sahi_labs = c("1" = "Cycle 1: 2009",
              "2" = "Cycle 2: 2013",
              "3" = "Cycle 3: 2017",
              "4" = "Cycle 4: 2023")

asis_labs = c("1" = "Cycle 1: 2019 \u2013 2024") #full cycle ends in 2024


cycle_labs <- switch(park,
                     "FRSP" = midn1_labs, 
                     "PETE" = midn1_labs,
                     "RICH" = midn1_labs,
                     "APCO" = midn2_labs,
                     "BOWA" = midn2_labs,
                     "GETT" = midn2_labs,
                     "HOFU" = midn2_labs,
                     "VAFO" = midn2_labs,
                     "GEWA" = ncbn_labs,
                     "THST" = ncbn_labs,
                     "COLO" = colo_labs,
                     "SAHI" = sahi_labs,
                     "ASIS" = asis_labs)

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")
table(reg_smooth$size_class)

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, #color = size_class,#linetype = sign, 
                         group = size_class)) + theme_FVM() +
  geom_bar(stat = 'identity', aes(fill = size_class), color = 'DimGrey') +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8) +
  labs(x  = "Cycle", y = bquote(Stems/m^2)) +
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 5) +
  # scale_color_manual(values = reg_colors, name = "Size Class",
  #                    labels = reg_labels) +
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = reg_labels) +
  scale_x_discrete(name = "Size Class",
                   labels = reg_labels) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot

ggsave(paste0(new_path, "figures/", "Figure_3A_", subunit, "_regen_by_size_class_by_cycle.svg"),
       height = 5.5, width = 7.5, units = 'in')

ggsave(paste0(new_path, "figures/", "Figure_3A_", subunit, "_regen_by_size_class_by_cycle.png"),
       height = 5.5, width = 7.5, units = 'in', dpi = 600)

#---- Figure 3B Diam. dist. trends by size class ----
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
# Including all species and canopy forms
tree_dd <- do.call(sumTreeDBHDist, args = c(args_vs, status = 'live')) |> filter(ParkSubUnit == subunit) 

dbh_cols <- c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9', 
              'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
              'dens_90_99.9', 'dens_100p')

tree_dbh_sm <- map_dfr(dbh_cols, 
                       ~case_boot_loess(tree_dd, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                        span = span, num_reps = 250, chatty = TRUE) %>% 
                         mutate(dbh_class = .x)
)
tree_dbh_sm

tree_dbh_sm$estimate[tree_dbh_sm$estimate < 0] <- 0
# tree_dbh_sm$lower95[tree_dbh_sm$estimate < 0] <- NA
# tree_dbh_sm$upper95[tree_dbh_sm$estimate < 0] <- NA

tree_dbh_sm$dbh_class <- factor(tree_dbh_sm$dbh_class, 
                                levels = dbh_cols)
levels(tree_dbh_sm$dbh_class)

dbh_labels <- c("10 \u2013 19.9 cm", 
                "20 \u2013 29.9 cm", 
                "30 \u2013 39.9 cm", 
                "40 \u2013 49.9 cm", 
                "50 \u2013 59.9 cm", 
                "60 \u2013 69.9 cm", 
                "70 \u2013 79.9 cm", 
                "80 \u2013 89.9 cm", 
                "90 \u2013 99.9 cm",
                ">= 100 cm")

dbh_labels <- c("10", 
                "20", 
                "30", 
                "40", 
                "50", 
                "60", 
                "70", 
                "80", 
                "90",
                "100+")

# check flat diameter distribution
head(tree_dbh_sm)

e <- min(tree_dbh_sm$estimate[tree_dbh_sm$estimate > 0])

tree_dbh_sm <- suppressWarnings(# warning is for 'c', which we're not using
  tree_dbh_sm %>% 
    mutate(dbh_class2 = gsub("p", "", dbh_class)) %>% 
    separate(dbh_class2, into = c("a", "b", "c"), sep = "_", remove = FALSE) %>% 
    mutate(class = as.numeric(b)) %>% select(-a, -b, -c, -dbh_class2))

AIC_test <- map_dfr(seq_along(cycle_labs), function(x){
  df <- tree_dbh_sm %>% filter(cycle == x)
  lin_mod <- lm(estimate ~ class, data = df)
  exp_mod <- lm(log(estimate + e) ~ class, data = df)
  aic_check <- data.frame(cycle = x,
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$estimate + e)))
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp + 4, "linear", "log-linear")
# added 4 to exp because linear is the simpler model. The log-linear model needs
# to be < 4 points lower than linear AIC to count.

cycle_labs_tr = c("1" = ifelse(AIC_test$best_mod[AIC_test$cycle == 1] == 'linear',
                               paste0(cycle_labs[1], "*"), cycle_labs[1]),
                  "2" = ifelse(AIC_test$best_mod[AIC_test$cycle == 2] == 'linear',
                               paste0(cycle_labs[2], "*"), cycle_labs[2]),
                  "3" = ifelse(AIC_test$best_mod[AIC_test$cycle == 3] == 'linear',
                               paste0(cycle_labs[3], "*"), cycle_labs[3]),
                  "4" = ifelse(AIC_test$best_mod[AIC_test$cycle == 4] == 'linear',
                               paste0(cycle_labs[4], "*"), cycle_labs[4]),
                  "5" = ifelse(AIC_test$best_mod[AIC_test$cycle == 5] == 'linear',
                               paste0(cycle_labs[5], "*"), cycle_labs[5]))

dbh_trend_plot <- 
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate)) + theme_FVM() +
  geom_bar(stat = 'identity', fill = "#81B082" , color = 'DimGrey')+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = "Stems/ha")+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs_tr), ncol = 5)+
  # scale_color_manual(values = reg_colors, name = "DBH Size Class",
  #                    labels = reg_labels)+
  # scale_fill_manual(values = reg_colors, name = "DBH Size Class",
  #                   labels = reg_labels)+
  scale_x_discrete(name = "DBH Size Class",
                   labels = dbh_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

dbh_trend_plot

ggsave(paste0(new_path, "figures/", "Figure_3B_", subunit, "_tree_dbh_dist_by_cycle.svg"),
       height = 5, width = 7.5, units = 'in')
ggsave(paste0(new_path, "figures/", "Figure_3B_", subunit, "_tree_dbh_dist_by_cycle.png"),
       height = 5, width = 7.5, units = 'in', dpi = 600)


# Regen Debt Metrics Code Start ------------------------------------------------

# Figure 2: DBI -----------------------------------------------------------
# Deer Browse Index
dbi <- joinStandData(park = park, from = from_4yr, to = to) |> 
  filter(EventID %in% evs_4yr) |> 
  select(Plot_Name, dbi = Deer_Browse_Index)

mean_dbi <- mean(dbi$dbi)
mean_dbi 

# dbiprev <- joinStandData(park = park, from = from_prev, to = to_prev) |> 
#   select(Plot_Name, dbi = Deer_Browse_Index) |> filter(!Plot_Name %in% "COLO-380") 
# dbi_prev <- mean(dbiprev$dbi)
# dbi_prev 

# DBI distribution plot
dbi_all <- joinStandData(park = park, from = from, to = to) |> 
  select(Plot_Name, ParkSubUnit, cycle, dbi = Deer_Browse_Index) |> filter(ParkSubUnit == subunit) 

dbi_sum <- dbi_all |> group_by(cycle, dbi) |> 
  summarize(num_plots = sum(!is.na(dbi)), .groups = 'drop') |>
  filter(!(is.na(dbi)))

# pivot_wider(names_from = dbi, values_from = num_plots, names_glue = "DBI_{.name}", values_fill = 0)

# Update to include all DBI values (1 and 2 lumped)
dbi_grid <- expand.grid(cycle = unique(dbi_sum$cycle), dbi = 2:5)
dbi_grid$dbi_fac <- factor(dbi_grid$dbi, levels = c(2, 3, 4, 5), labels = c("Low", "Medium", "High", "Very High"))

dbi_sum2 <- left_join(dbi_grid, dbi_sum, by = c("cycle", "dbi"))
dbi_sum2$num_plots[is.na(dbi_sum2$num_plots)] <- 0

dbi_plot <- 
  ggplot(dbi_sum2, aes(x = cycle, y = num_plots, fill = dbi_fac, color = dbi_fac)) +
  geom_bar(position = 'fill', stat = 'identity', na.rm = T, color = '#696969') +
  theme_FVM() +
  # scale_color_manual(values = c("Low" = "#05e689", "Medium" = "#efdf00", 
  #                               "High" = "#f94b24", "Very High" = "#a60808"),
  #                    labels = c("Low", "Medium", "High", "Very High"), 
  #                    name = "Deer Browse Impact") +
  scale_fill_manual(values = c("Low" = "#05e689", "Medium" = "#efdf00", 
                               "High" = "#f94b24", "Very High" = "#a60808"),
                    labels = c("Low", "Medium", "High", "Very High"), 
                    name = "Deer Browse Impact") +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00), labels = c(0, 25, 50, 75, 100)) +
  labs(y = "Proportion of Plots")

# svg(paste0(new_path, "figures/", "Figure_6_", park, "_DBI_by_cycle.svg"),
#     height = 6.15, width = 8)
# dbi_plot
# dev.off()

figpath <- paste0(path, park, "/", to, '/figures/')
ggsave(paste0(figpath, "Figure_2_", subunit, "_DBI_by_cycle.svg"), height = 6.15, width = 8, units = 'in')
ggsave(paste0(figpath, "Figure_2_", subunit, "_DBI_by_cycle.png"), height = 6.15, width = 8, units = 'in', dpi = 600)

# Regen densities
# reg is all spp. reg$NatCan is used for metrics that only include native canopy forming spp.
reg <- joinRegenData(park = park, from = from_4yr, to = to, units = 'sq.m') |> 
  filter(EventID %in% evs_4yr) 
table(reg$PanelCode, reg$SampleYear)

reg$CanopyExclusion[reg$ScientificName %in% c("Fraxinus americana", "Fraxinus nigra", 
                                              "Fraxinus pennsylvanica", "Fraxinus")] <- TRUE
reg$NatCan <- ifelse(reg$CanopyExclusion == FALSE & reg$Exotic == FALSE, 1, 0)

table(reg$ScientificName, reg$NatCan) # only native canopy forming included

DBI_threshold <- ifelse(mean_dbi <= 3, 50, 100)

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sapden = sum(sap_den, na.rm = T),
            seedden = sum(seed_den, na.rm = T),
            stock = (sum(stock, na.rm = T)) * (4 * pi), .groups = 'drop') |> # for 100pt scale
  mutate(stocked = ifelse(stock > DBI_threshold, 1, 0)) # DBI > 3, so stocking must be 100

regsum_natcan <- reg_natcan |> 
  summarize(sapden = sum(sapden)/num_subunit_plots,
            seedden = sum(seedden)/num_subunit_plots,
            stock = (sum(stock)/num_subunit_plots), 
            pct_stocked = sum(stocked)/num_subunit_plots*100)

reg_tot <- reg |> group_by(Plot_Name) |> 
  summarize(
    sap_tot = sum(sap_den, na.rm = T),
    seed_tot = sum(seed_den, na.rm = T)
  )

if(length(unique(reg_tot$Plot_Name)) < num_subunit_plots & !park %in% "COLO"){
  warning(paste0("Regen debt metrics don't include the total number of plots for ", park, 
                 ". Compare total number of plots = ", num_subunit_plots, " regen debt plot tally = ", 
                 length(unique(reg_tot$Plot_Name))))}

reg_natcan <- reg |> filter(NatCan == 1) |> 
  group_by(Plot_Name) |> 
  summarize(sap_natcan = sum(sap_den, na.rm = T),
            seed_natcan = sum(seed_den, na.rm = T))

reg_comb <- left_join(reg_tot, reg_natcan, by = "Plot_Name") |> 
  mutate(sap_dens_pct = sap_natcan/sap_tot,
         seed_dens_pct = seed_natcan/seed_tot)
#intersect(names(plotevs), names(reg_comb))
reg_comb <- left_join(plotevs_4yrB, reg_comb, by = "Plot_Name")
#if(length(unique(reg_comb$Plot_Name)) < num_plots){warning("Need to left_join with plotevs_4yrB")} 
head(reg_comb)

reg_comb[,2:ncol(reg_comb)][is.na(reg_comb[,2:ncol(reg_comb)])] <- 0
reg_comb$sap_dens_pct[is.nan(reg_comb$sap_dens_pct)] <- 0
reg_comb$seed_dens_pct[is.nan(reg_comb$seed_dens_pct)] <- 0

reg_pct <- reg_comb |> 
  summarize(sap_pct = mean(sap_dens_pct, na.rm = T) * 100,
            seed_pct = mean(seed_dens_pct, na.rm = T) * 100)

reg_pct
regsum_natcan
sort(unique(reg$ScientificName))

# Sorenson similarity
reg_seed <- reg |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3", 
                             ScientificName == "Quercus (Red group)" ~ "QUESPP", 
                             ScientificName == "Quercus (White group)" ~ "QUESPP",
                             TRUE ~ sppcode1)) |> 
  select(Plot_Name, sppcode, seed_den) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(seed_den = sum(seed_den, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = seed_den, values_fill = 0)

reg_sap <- reg |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = case_when(ScientificName == "Acer saccharum" ~ "ACESAC3", 
                             ScientificName == "Quercus (Red group)" ~ "QUESPP", 
                             ScientificName == "Quercus (White group)" ~ "QUESPP",
                             TRUE ~ sppcode1)) |> 
  select(Plot_Name, sppcode, sap_den) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(sap_den = sum(sap_den, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = sap_den, values_fill = 0)

trees <- joinTreeData(park = park, from = from_4yr, to = to, status = 'live') |> 
  filter(EventID %in% evs_4yr) |> 
  mutate(genus = word(ScientificName, 1),
         species = ifelse(is.na(word(ScientificName, 2)), "SPP", word(ScientificName, 2)),
         sppcode1 = toupper(paste0(substr(genus, 1, 3), substr(species, 1, 3))),
         sppcode = ifelse(ScientificName == "Acer saccharum", "ACESAC3", sppcode1)) |> 
  select(Plot_Name, sppcode, BA_cm2) |> 
  group_by(Plot_Name, sppcode) |> 
  summarize(treeBA = sum(BA_cm2, na.rm = T)/plot_size, .groups = 'drop') |> 
  pivot_wider(names_from = sppcode, values_from = treeBA, values_fill = 0)

all_spp <- c("Plot_Name", sort(unique(c(names(trees[,-1]), names(reg_seed[,-1]), names(reg_sap[,-1])))))

seed_miss <- setdiff(all_spp, names(reg_seed))
reg_seed[seed_miss] <- 0
reg_seed <- reg_seed[all_spp]
reg_seed$strata <- "seedling"

sap_miss <- setdiff(all_spp, names(reg_sap))
reg_sap[sap_miss] <- 0
reg_sap <- reg_sap[all_spp]
reg_sap$strata = "sapling"

tree_miss <- setdiff(all_spp, names(trees))
trees[tree_miss] <- 0
trees <- trees[all_spp]
trees$strata = "tree"

comb <- rbind(reg_seed, reg_sap, trees)

sor_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0) # remove species that sum to 0
  sor <-  betadiver(df2, method = 'sor')
  return(sor)
}

sor_sap <- comb %>% filter(strata %in% c("tree", "sapling")) %>% 
  group_by(Plot_Name) %>% nest() %>% 
  mutate(sap_sor = purrr::map(data, sor_fun)) %>%
  unnest(cols = c(sap_sor)) %>% select(Plot_Name, sap_sor) %>% data.frame() 

sor_sap_mean <- mean(sor_sap$sap_sor, na.rm = T)

sor_seed <- comb %>% filter(strata %in% c("tree", "seedling")) %>% 
  group_by(Plot_Name) %>% nest() %>% 
  mutate(seed_sor = purrr::map(data, sor_fun)) %>% 
  unnest(cols = c(seed_sor)) %>% select(Plot_Name, seed_sor) %>% data.frame()

sor_seed_mean <- mean(sor_seed$seed_sor, na.rm = T)

# Tree DBH distribution
tree_dist <- sumTreeDBHDist(park = park, from = from_4yr, to = to, status = 'live') |> 
  filter(EventID %in% evs_4yr) 

tree_dist2 <- tree_dist |> 
  summarize(dbh_10cm = sum(dens_10_19.9)/num_subunit_plots,
            dbh_20cm = sum(dens_20_29.9)/num_subunit_plots,
            dbh_30cm = sum(dens_30_39.9)/num_subunit_plots,
            dbh_40cm = sum(dens_40_49.9)/num_subunit_plots,
            dbh_50cm = sum(dens_50_59.9)/num_subunit_plots, 
            dbh_60cm = sum(dens_60_69.9)/num_subunit_plots,                                      
            dbh_70cm = sum(dens_70_79.9)/num_subunit_plots,                                 
            dbh_80cm = sum(dens_80_89.9)/num_subunit_plots, 
            dbh_90cm = sum(dens_90_99.9)/num_subunit_plots,
            dbh_100cm = sum(dens_100p)/num_subunit_plots) |> 
  mutate(park = park) |> 
  pivot_longer(cols = -park, names_to = "size_class", values_to = "density") 

tree_dist2$class <- as.numeric(gsub("\\D", "", tree_dist2$size_class))

lin_mod <- lm(density ~ class, data = tree_dist2)
exp_mod <- lm(log(density + 1) ~ class, data = tree_dist2)

aic_check <- data.frame(park = park, 
                        linear = AIC(lin_mod),
                        exp = AIC(exp_mod) + sum(2*log(tree_dist2$density + 1)))
aic_check 
flat_dist <- ifelse(aic_check$linear < aic_check$exp, 1, 0)

#----- Regen Debt Table -----
debt <- data.frame(Metric = c("Sapling Density", "Seedling Density", "% Stocked Plots",
                              "Stocking Index", "Deer Browse Impacts", "Flat Tree Diam. Dist.",
                              "Sapling Composition", "Seedling Comp.", 
                              "Sorensen Sapling", "Sorensen Seedling"),
                   Value = c(regsum_natcan$sapden, regsum_natcan$seedden, regsum_natcan$pct_stocked,
                             regsum_natcan$stock, mean_dbi, flat_dist, 
                             reg_pct$sap_pct, reg_pct$seed_pct, 
                             sor_sap_mean, sor_seed_mean),
                   Units = c("(stems/m^2)", "(stems/m^2)", "%", 
                             "(2m radius scale)", "(range: 1 - 5)", "", "%", "%", 
                             "(range: 0 - 1)", "(range: 0 - 1)"))
debt <- debt |> mutate(
  status = 
    case_when(Metric == "Sapling Density" & Value < 0.1 ~ "Critical",
              Metric == "Sapling Density" & Value >= 0.1 & Value < 0.16 ~ "Caution",
              Metric == "Sapling Density" & Value >= 0.16 ~ "Acceptable",
              
              Metric == "Seedling Density" & Value < 0.25 ~ "Critical",
              Metric == "Seedling Density" & Value >= 0.25 & Value < 2 ~ "Caution",
              Metric == "Seedling Density" & Value >= 2.0 ~ "Acceptable",
              
              Metric == "% Stocked Plots" & Value < 33 ~ "Critical",
              Metric == "% Stocked Plots" & Value >= 33 & Value < 67 ~ "Caution",
              Metric == "% Stocked Plots" & Value >= 67 ~ "Acceptable",
              
              Metric == "Stocking Index" & Value < 25 ~ "Critical",
              Metric == "Stocking Index" & Value >= 25 & Value < 100 ~ "Caution",
              Metric == "Stocking Index" & Value >= 100 ~ "Acceptable",
              
              Metric == "Deer Browse Impacts" & Value >= 4 ~ "Critical",
              Metric == "Deer Browse Impacts" & Value > 3 & Value < 4 ~ "Caution",
              Metric == "Deer Browse Impacts" & Value <= 3 ~ "Acceptable",
              
              Metric == "Flat Tree Diam. Dist." & Value == 1 ~ "Critical",
              Metric == "Flat Tree Diam. Dist." & Value == 0 ~ "Acceptable",
              
              Metric == "Sapling Composition" & Value < 50 ~ "Critical",
              Metric == "Sapling Composition" & Value >= 50 & Value <= 70 ~ "Caution",
              Metric == "Sapling Composition" & Value > 70 ~ "Acceptable",
              
              Metric == "Seedling Comp." & Value < 50 ~ "Critical",
              Metric == "Seedling Comp." & Value >= 50 & Value <= 70 ~ "Caution",
              Metric == "Seedling Comp." & Value > 70 ~ "Acceptable",
              
              Metric == "Sorensen Sapling" & Value < 0.2 ~ "Critical",
              Metric == "Sorensen Sapling" & Value >= 0.2 ~ "Acceptable",
              
              Metric == "Sorensen Seedling" & Value < 0.2 ~ "Critical",
              Metric == "Sorensen Seedling" & Value >= 0.2 ~ "Acceptable",
              
              TRUE ~ "UNKNOWN"
    )
)

debt_final <- 
  rbind(debt,
        data.frame(Metric = 'Regen. Debt Status', 
                   Value = sum(debt$status == "Critical"), 
                   Units = NA,
                   status = case_when(sum(debt$status == "Critical") >= 6 ~ "Imminent Failure",
                                      between(sum(debt$status == "Critical"), 4, 5) ~ "Probable Failure",
                                      between(sum(debt$status == "Critical"), 2, 3) ~ "Insecure",
                                      sum(debt$status == "Critical") <= 1 ~ "Secure",
                                      TRUE ~ "UNK"))
  )

debt_final$rank <- as.numeric(row.names(debt_final))
debt_final$label_order <- reorder(debt_final$Metric, debt_final$rank)
debt_final$status_fac <- factor(debt_final$status, levels = c("Acceptable", "Caution", "Critical"))

debt_final$text_label <- paste0(as.character(paste0(round(debt_final$Value, 2))), " ", debt_final$Units)
debt_final$text_label <- ifelse(debt_final$Metric == "Flat Tree Diam. Dist." & debt_final$Value == 1, 
                                paste0("TRUE"), 
                                ifelse(debt_final$Metric == "Flat Tree Diam. Dist." & debt_final$Value == 0,
                                       paste0("FALSE"), paste0(debt_final$text_label)))
debt_final$text_label <- ifelse(debt_final$Metric == "Regen. Debt Status", 
                                paste0(debt_final$status), 
                                paste0(debt_final$text_label))

debt_final

results_plot <- 
  ggplot(debt_final, 
         aes(x = 1, y = ordered(label_order, levels = rev(label_order)),
             fill = label_order)) +
  geom_tile(aes(fill = status_fac), color = 'black', linewidth = 0.5)+
  geom_text(aes(label = text_label,
                fontface = ifelse(Metric == "Regen. Debt Status", 2, 1))) +
  scale_fill_manual(values = c('Acceptable' = "#BDEBA7", 
                               'Caution' = "#FFFF79",
                               'Critical' = "#FF5B5B"), 
                    na.value = "white",
                    labels = c("Acceptable",
                               "Caution",
                               "Critical"),
                    breaks = c("Acceptable", "Caution", "Critical"),
                    name = NULL, 
                    drop = FALSE)+ #, na.translate = FALSE)+
  labs(x = NULL, y = NULL) + 
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        #text = element_text(size = 9.5),
        legend.position = 'bottom',
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10)
  ) 

results_plot

figpath2 <- paste0(path, park, "/", to, '/figures/') # not hard coded

ggsave(paste0(figpath2, "Figure_1_", subunit, "_Regen_Debt_table.svg"), height = 6, width = 4.5, units = 'in')
ggsave(paste0(figpath2, "Figure_1_", subunit, "_Regen_Debt_table.png"), height = 6, width = 4.5, units = 'in', dpi = 600)

debt_final <- debt_final |> mutate(park = park)

write.csv(debt_final, paste0(new_path, "tables/Regen_Debt_table_", subunit, ".csv"), row.names= F)


# Stem Changes by Species Code Start --------------------------------------
span = 8/12 # more smoothing for line graphs so less wiggles at every cycle

#---- Tree trends by species ----
trees1 <- do.call(joinTreeData, args = c(args_vs, status = 'live')) |> filter(ParkSubUnit == subunit)

plot_evs <- do.call(joinLocEvent, args = args_vs) |> filter(ParkSubUnit == subunit) |> select(Plot_Name, ParkSubUnit, SampleYear, cycle)

trees <- left_join(trees1, plot_evs, by = c("Plot_Name", "ParkSubUnit", "cycle", "SampleYear")) |> 
  mutate(BA_m2ha = BA_cm2/ifelse(ParkUnit == "ACAD", 225, 400),
         stems = 1) |> 
  select(Plot_Name, SampleYear, cycle, ScientificName, 
         stems, BA_m2ha) |> 
  arrange(Plot_Name, SampleYear)  

tree_grps <- left_join(trees, trspp_grps |> select(Species, spp_grp, sppcode), 
                       by = c("ScientificName" = "Species"))

if(nrow(tree_grps[which(is.na(tree_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in tree_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

###Park specific changes to tree species groups###
#Must match listed changes in forest_summary_code_MIDN.R
if(park == "HOFU"){
  tree_grps <- tree_grps %>%
    mutate(sppcode = case_when(ScientificName == "Ulmus americana" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Ulmus americana" ~ "Other Native",
                               TRUE ~ spp_grp))
}
if(park == "COLO"){
  tree_grps <- tree_grps %>%
    mutate(sppcode = case_when(ScientificName == "Morella cerifera" ~ "SUBCAN",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Morella cerifera" ~ "Subcanopy",
                               TRUE ~ spp_grp))
}
if(park == "RICH"){
  tree_grps <- tree_grps %>%
    mutate(sppcode = case_when(ScientificName == "Betula nigra" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Betula nigra" ~ "Other Native",
                               TRUE ~ spp_grp))%>%
    mutate(sppcode = case_when(ScientificName == "Juniperus virginiana" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Juniperus virginiana" ~ "Other Native",
                               TRUE ~ spp_grp))%>%
    mutate(spp_grp = case_when(sppcode == "ULMSPP" ~ "Other Native",
                               TRUE ~ spp_grp))%>%
    mutate(sppcode = case_when(sppcode == "ULMSPP" ~ "OTHNAT",
                               TRUE ~ sppcode)) 
}

tree_grps <- tree_grps %>%  mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                                                       spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                                                       spp_grp == "Other Exotic" ~ "Other exotic spp.",
                                                       TRUE ~ spp_grp))

plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            SampleYear = unique(plot_yr$SampleYear),
                            spp_grp = unique(tree_grps$spp_grp)) |> 
  select(Plot_Name, SampleYear, spp_grp)

#plot_spp_yr1$sppcode[plot_spp_yr1$ScientificName == "Acer saccharum"] <- "ACESAC3"
head(tree_grps)
plot_spp_yr2 <- left_join(plot_spp_yr1, 
                          tree_grps |> select(spp_grp) |> unique(), 
                          by = "spp_grp", relationship = 'many-to-many')

plot_spp_yr3 <- left_join(plot_yr, plot_spp_yr2, by = c("Plot_Name", "SampleYear"))

dup_spp_check <- as.data.frame(table(plot_spp_yr3$spp_grp))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))

# Join group code back in
head(plot_spp_yr3)
#head(trspp_grps) #switched to tree_grps df because losing the edits that were made to species groups 
head(tree_grps)
plot_spp_yr <- left_join(plot_spp_yr3, tree_grps |> select(sppcode, spp_grp) |> unique(), 
                         by = "spp_grp")

tree_spp_sum1 <- left_join(plot_spp_yr, 
                           tree_grps |> select(Plot_Name, SampleYear, spp_grp, sppcode, stems, BA_m2ha), 
                           by = c("Plot_Name", "SampleYear", "spp_grp", 'sppcode'),
                           relationship = 'many-to-many') #|> 

tree_spp_sum1[,c("stems", "BA_m2ha")][is.na(tree_spp_sum1[,c("stems", "BA_m2ha")])] <- 0

conv_to_ha <- 10000/400

tree_spp_sum <- tree_spp_sum1 |> group_by(Plot_Name, SampleYear, spp_grp, sppcode) |> 
  summarize(stems_ha = (sum(stems)) * conv_to_ha,
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

head(tree_spp_sum)
spp_list <- sort(unique(tree_spp_sum$sppcode))
spp_list

length(unique(tree_spp_sum$spp_grp))
table(tree_spp_sum$spp_grp)

#---- Tree Stems
tree_stem_smooth <- purrr::map_dfr(spp_list, 
                                   function(spp){
                                     df <- tree_spp_sum |> filter(sppcode %in% spp)
                                     case_boot_loess(df, x = "SampleYear", y = "stems_ha", ID = "Plot_Name",
                                                     group = "sppcode", 
                                                     span = span, num_reps = 1) |>
                                       mutate(sppcode = spp)
                                   }
)
# Determine if significant based on whether first and last year CIs overlap
tree_stem_smooth2 <- 
  left_join(tree_stem_smooth, 
            tree_stem_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
tree_stem_smooth3 <- left_join(tree_stem_smooth2,
                               plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                               by = c('sppcode'), 
                               relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

write.csv(tree_stem_smooth3, paste0(new_path, "tables/", subunit, 
                                    "_tree_stem_density_estimates.csv"), row.names = FALSE)

#--- Tree BA
tree_BA_smooth <- purrr::map_dfr(spp_list, 
                                 function(spp){
                                   df <- tree_spp_sum |> filter(sppcode %in% spp)
                                   case_boot_loess(df, x = "SampleYear", y = "BA_m2ha", ID = "Plot_Name",
                                                   group = "sppcode", 
                                                   span = span, num_reps = 1) |>
                                     mutate(sppcode = spp)
                                 }
)


tree_BA_smooth2 <- 
  left_join(tree_BA_smooth, 
            tree_BA_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
tree_BA_smooth3 <- left_join(tree_BA_smooth2,
                             plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
                             by = c('sppcode'), 
                             relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

write.csv(tree_BA_smooth3, paste0(new_path, "tables/", subunit, 
                                  "_tree_BA_estimates.csv"), row.names = FALSE)

net_ba_year <- tree_BA_smooth3 |> group_by(term, SampleYear) |> summarize(net_ba = sum(estimate))
net_ba_year # BA overtime, check for decline


table(tree_stem_smooth3$spp_grp)
# Colors to start with. Can change them per park if needed
cols = c(
  "Acer rubrum (red maple)" = "#38A800",
  "Acer platanoides (Norway maple)" = "#8b0000",
  "Acer spp. (maple)" = "#00FF00",
  "Acer saccharum (sugar maple)" = "#009999",
  "Ailanthus altissima (tree-of-heaven)" = "#cd4a8f",
  "Asimina triloba (pawpaw)" = "#FF00C5",
  "Betula lenta (black birch)" = "#ffd8b1", # darkened color for 2024 NETN figs; does not match maps
  "Betula spp. (birch)" = "#ffd8b1", 
  "Carya spp. (hickory)" = "#911eb4",
  "Fagus grandifolia (American beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tulip poplar)" = "#4363d8",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other exotic spp." = "#ca0020",
  "Other native canopy spp." = "#d9d9d9",
  "Pinus spp. (pine)" = "#5A462B",
  "Pinus strobus (eastern white pine)" = "#5A1111",
  "Pinus taeda (loblolly pine)" = "#5A1111", #assumes no overlap in PINSTR and PINTAE
  "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Pinus resinosa (red pine)" = "#E5740D",
  "Prunus spp. (native cherry)" ="#00E6A9", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Robinia pseudoacacia (black locust)" = "#cccc00",
  "Other native subcanopy spp." = "#ffa8b4",
  "Tsuga canadensis (eastern hemlock)" = "#9bd2ef",
  "Ulmus spp. (native elm)" = "#808000", 
  "Unknown spp." = "#CACACA",
  "Diospyros virginiana (persimmon)" = "#006666", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "#ffd8b1", #for ASIS only
  "Sassafras albidum (sassafrass)" = "#59538A", #ASIS only
  "Abies balsamea (balsam fir)" = '#911eb4',#ACAD only
  "Other conifer" = "#42d4f4",#ACAD only
  "Picea spp. (spruce)" = "#000075",#ACAD only
  "Populus spp. (aspen)" = "#FFFF00")#ACAD only


lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer platanoides (Norway maple)" = "solid",
  "Acer saccharum (sugar maple)" = "dotdash",
  "Acer spp. (maple)" = "solid",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Asimina triloba (pawpaw)" = "dashed",
  "Betula lenta (black birch)" = "dashed",
  "Betula spp. (birch)" = "dashed", # Either use BETLEN or BETSPP
  "Carya spp. (hickory)" = "solid",
  "Fagus grandifolia (American beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "dotdash",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tulip poplar)" = "solid",
  "Nyssa sylvatica (black gum)" = "dashed",
  "Other exotic spp." = "dashed",
  "Other native canopy spp." = "solid",
  "Pinus spp. (pine)" = "dotdash",
  "Pinus strobus (eastern white pine)" = "dotdash",
  "Pinus taeda (loblolly pine)" = "dotdash",
  "Pinus virginiana (Virginia pine)" = "dotdash",
  "Pinus resinosa (red pine)" = "dotdash",
  "Prunus spp. (native cherry)" = "dotdash", 
  "Pyrus calleryana (Bradford pear)" = "dotted",
  "Quercus spp. (oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "dashed",
  "Other native subcanopy spp." = "solid",
  "Tsuga canadensis (eastern hemlock)" = "dashed",
  "Ulmus spp. (native elm)" = "dashed", 
  "Unknown spp." = "dotted",
  "Diospyros virginiana (American persimmon)" = "dashed", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "dashed", #for ASIS only
  "Sassafras albidum (sassafrass)" = "dashed", #ASIS only
  "Abies balsamea (balsam fir)" = 'dashed',#ACAD only
  "Other conifer" = "dotdash",#ACAD only
  "Picea spp. (spruce)" = "solid",#ACAD only
  "Populus spp. (aspen)" = "dotted")#ACAD only



#---- Net stem/BA plots by species
#spp_rows <- ifelse(park %in% c("GETT", "RICH", "COLO"), 6, 5) # rows in spp legend
#spp_rows = 7

net_stems <- 
  ggplot(tree_stem_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Density (stems/ha)") +
  # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FVM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines,  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', # b/c shares page with 4B 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm')) #+ 
#guides(color = guide_legend(nrow = spp_rows))

net_stems
ggsave(paste0(new_path, "figures/", "Figure_5A_", subunit, "_smoothed_tree_stems_by_species_cycle.svg"),
       height = 6.15, width = 8, units = 'in')

ggsave(paste0(new_path, "figures/", "Figure_5A_", subunit, "_smoothed_tree_stems_by_species_cycle.png"),
       height = 6.15, width = 8, units = 'in', dpi = 600)


net_ba <- 
  ggplot(tree_BA_smooth3, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Tree Basal Area (sq.m/ha)") +
  # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FVM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+ 
# guides(color = guide_legend(nrow = spp_rows))

net_ba
ggsave(paste0(new_path, "figures/", "Figure_5B_", subunit, "_smoothed_BA_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_5B_", subunit, "_smoothed_BA_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

ggarrange(net_stems, net_ba, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B.")) 

ggsave(paste0(new_path, "figures/Figure_5_", subunit, "_smoothed_tree_dens_BA_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_5_", subunit, "_smoothed_tree_dens_BA_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)

#----- Similar figures for seedlings and saplings -----
reg1 <- do.call(joinRegenData, c(args_all, units = 'sq.m')) |> 
  filter(!ScientificName %in% "None present") |> filter(ParkSubUnit == subunit)

reg <- left_join(reg1, plot_evs, by = c("Plot_Name", "ParkSubUnit", "cycle", "SampleYear")) |> 
  mutate(stock = stock * (4 * pi)) #100pt scale

reg_grps <- left_join(reg, trspp_grps |> select(Species, spp_grp, sppcode), 
                      by = c("ScientificName" = "Species"))

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

if(park == "HOFU"){
  reg_grps <- reg_grps %>%
    mutate(sppcode = case_when(ScientificName == "Ilex opaca" ~ "SUBCAN",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Ilex opaca" ~ "Subcanopy",
                               TRUE ~ spp_grp))
}

if(park == "GETT"){
  reg_grps <- reg_grps %>%
    mutate(sppcode = case_when(ScientificName == "Robinia pseudoacacia" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Robinia pseudoacacia" ~ "Other Native",
                               TRUE ~ spp_grp))
}
if(park == "PETE"){
  reg_grps <- reg_grps %>%
    mutate(sppcode = case_when(ScientificName == "Castanea pumila" ~ "SUBCAN",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Castanea pumila" ~ "Subcanopy",
                               TRUE ~ spp_grp))
}

if(park == "COLO"| park == "GEWA"| park == "THST"){
  reg_grps <- reg_grps %>%
    mutate(sppcode = case_when(ScientificName == "Asimina triloba" ~ "ASITRI",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Asimina triloba" ~ "Asimina triloba",
                               TRUE ~ spp_grp))
}
if(park == "RICH"){
  reg_grps <- reg_grps %>%
    mutate(sppcode = case_when(ScientificName == "Juniperus virginiana" ~ "OTHNAT",
                               TRUE ~ sppcode)) %>%
    mutate(spp_grp = case_when(ScientificName == "Juniperus virginiana" ~ "Other Native",
                               TRUE ~ spp_grp))%>%
    mutate(spp_grp = case_when(sppcode == "ULMSPP" ~ "Other Native",
                               TRUE ~ spp_grp))%>%
    mutate(sppcode = case_when(sppcode == "ULMSPP" ~ "OTHNAT",
                               TRUE ~ sppcode)) 
}

reg_grps <- reg_grps %>% mutate(spp_grp = case_when(spp_grp == "Other Native" ~ "Other native canopy spp.",
                                                    spp_grp == "Subcanopy" ~ "Other native subcanopy spp.",
                                                    spp_grp == "Other Exotic" ~ "Other exotic spp.",
                                                    ScientificName == "Fabaceae" ~ "Other native canopy spp.",
                                                    TRUE ~ spp_grp)) %>% 
  mutate(sppcode = case_when(ScientificName == "Fabaceae" ~ "OTHNAT",
                             TRUE ~ sppcode))



# Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> unique()

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
# Need to then left join to drop unsampled years.
plot_rspp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                             SampleYear = unique(plot_yr$SampleYear),
                             spp_grp = unique(reg_grps$spp_grp)) |> 
  select(Plot_Name, SampleYear, spp_grp)

plot_rspp_yr2 <- left_join(plot_rspp_yr1, 
                           reg_grps |> select(spp_grp) |> unique(), 
                           by = "spp_grp", relationship = 'many-to-many')

plot_rspp_yr3 <- left_join(plot_yr, plot_rspp_yr2, by = c("Plot_Name", "SampleYear"))

dup_rspp_check <- as.data.frame(table(plot_rspp_yr3$spp_grp))

if(length(unique(dup_rspp_check$Freq)) > 1)(stop("Not all regen species have the same frequency in expand grid. Check for duplicate species codes."))

# Join group code back in
head(plot_rspp_yr3)
head(reg_grps)
plot_rspp_yr <- left_join(plot_rspp_yr3, reg_grps |> select(sppcode, spp_grp) |> unique(), 
                          by = "spp_grp")

reg_spp_smooth <- left_join(plot_rspp_yr, reg_grps |> select(Plot_Name, SampleYear, spp_grp, seed_den, sap_den), 
                            by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
# filter(SampleYear > 2006) #dropped first year b/c only 1 microplot

reg_spp_smooth[,c("seed_den", "sap_den")][is.na(reg_spp_smooth[,c("seed_den", "sap_den")])] <- 0

spp_list <- sort(unique(reg_spp_smooth$sppcode))
spp_list

length(spp_list) # may be longer than Map 3 b/c includes all cycles

#span = 4/5
table(reg_spp_smooth$SampleYear, reg_spp_smooth$Plot_Name)

seed_smooth <- purrr::map_dfr(spp_list, 
                              function(spp){
                                df <- reg_spp_smooth |> filter(sppcode %in% spp)
                                case_boot_loess(df, x = "SampleYear", y = "seed_den", ID = "Plot_Name",
                                                group = "sppcode", 
                                                span = span, num_reps = 1) |>
                                  mutate(sppcode = spp)
                              }
)

sap_smooth <- purrr::map_dfr(spp_list, 
                             function(spp){
                               df <- reg_spp_smooth |> filter(sppcode %in% spp)
                               case_boot_loess(df, x = "SampleYear", y = "sap_den", ID = "Plot_Name",
                                               group = "sppcode", 
                                               span = span, num_reps = 1) |>
                                 mutate(sppcode = spp)
                             }
)

# Determine if significant based on whether first and last year CIs overlap
seed_smooth2 <- 
  left_join(seed_smooth, 
            seed_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

# Join full group names back into dataframe
seed_smooth3 <- left_join(seed_smooth2,
                          plot_rspp_yr |> select(spp_grp, sppcode) |> unique(),
                          by = c('sppcode'), 
                          relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |> 
  arrange(spp_grp)

# Saplings
sap_smooth2 <- 
  left_join(sap_smooth, 
            sap_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(sppcode, sign),
            by = "sppcode")

sap_smooth3 <- left_join(sap_smooth2,
                         plot_rspp_yr |> select(spp_grp, sppcode) |> unique(),
                         by = c('sppcode'), 
                         relationship = 'many-to-many') |> 
  mutate(spp_grp = as.character(spp_grp)) |>
  group_by(sppcode) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  arrange(spp_grp)


net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Seedling Density (stems/sq.m)") +
  theme_FVM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm'))# + 
#guides(color = guide_legend(nrow = 5))

net_seeds

ggsave(paste0(new_path, "figures/", "Figure_4A_", subunit, "_net_seedlings_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_4A_", subunit, "_net_seedlings_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

net_saps <- 
  ggplot(sap_smooth3, 
         aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
  labs(x = NULL, y = "Sapling Density (stems/sq.m)") +
  theme_FVM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+
#guides(color = guide_legend(nrow = spp_rows))

net_saps

ggsave(paste0(new_path, "figures/", "Figure_4B_", subunit, "_net_saplings_by_species_cycle.svg"),
       height = 6.15, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_4B_", subunit, "_net_saplings_by_species_cycle.png"),
       height = 6.15, width = 8, dpi = 600)

ggarrange(net_seeds, net_saps, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B."))

ggsave(paste0(new_path, "figures/Figure_4_", subunit, "_smoothed_regen_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_4_", subunit, "_smoothed_regen_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)
#----- Trends in invasive guilds over time -----
guilds <- do.call(sumQuadGuilds, c(args_vs, speciesType = 'invasive', splitHerb = F))
guild_list <- sort(unique(guilds$Group))

guild_smooth <- purrr::map_dfr(guild_list,
                               function(g){
                                 df <- guilds |> filter(Group %in% g)
                                 case_boot_loess(df, x = "SampleYear", y = "quad_pct_cover", 
                                                 ID = "Plot_Name", span = span,
                                                 group = "Group",
                                                 num_reps = 1) |> 
                                   mutate(guild = g)
                               })

#guild_smooth2 <- 
gcols <- c("Tree" = "#4A68BF",
           "Shrub" = "#CD5C5C",
           "Herbaceous" = "#228b22",
           "Graminoid" = "#ffd700")

guild_plot <- 
  ggplot(guild_smooth, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = guild, group = guild), linewidth = 1.5) +
  labs(x = NULL, y = "% Invasive Cover") +
  theme_FVM()+
  scale_color_manual(values = gcols,  name = "Invasive Guild") +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right',  
        legend.key.width = unit(1.5, 'cm'), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), 
        plot.margin = margin(0.4, 0.4, 0.1, 0.4, "cm"))

guild_plot

ggsave(paste0(new_path, "figures/", "Figure_6_", subunit, "_smoothed_invasive_cover_by_guild_cycle.svg"),
       height = 4.6, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_6_", subunit, "_smoothed_invasive_cover_by_guild_cycle.png"),
       height = 4.6, width = 8, dpi = 600)




