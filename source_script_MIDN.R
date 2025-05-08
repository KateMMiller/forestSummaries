#--------------------------------
# Source Script for forestMIDN Data Summaries
# Written by Kate Miller 9/13/2023
#--------------------------------

# Note that the stocking index calculated in both the summary code and regen debt code is on the 
# McWilliams 100 point scale, not the 1m2 scale that MIDN used to use.

# Make sure packages are updated
#devtools::install_github("KateMMiller/forestMIDN")
#devtools::install_github("KateMMiller/forestTrends")

# Imports/Libraries
library(forestMIDN)
library(forestTrends)
library(tidyverse)
library(sf)

# report_year = 2024 # only here for testing. Defined in MIDN_figures_and_tables.RMD params.
if(!exists("path")){path = paste0('./output/', report_year, "/MIDN/")} #general path that should work for everyone

# Make sure local copy of DB is current or connect to server
importData()

# assign params to global env. for source files to find. Makes iterating easier.
# temp: so can run individual parks w/ .rmd file
##Comment out before running for all parks##
#assign params to global env. for source files to find. Makes iterating easier.
#
# midn_names <- read.csv("MIDN_MetaData.csv")
# midn_params <- read.csv("MIDN_params.csv") # !!!! MUST UPDATE EVERY YEAR !!!!
# park <<- "VAFO"
# from <<- as.numeric(midn_params$from[midn_params$park == park])
# from_4yr <<- as.numeric(midn_params$from_4yr[midn_params$park == park])
# to <<- as.numeric(midn_params$to[midn_params$park == park])
# cycle_latest <<- as.numeric(midn_params$cycle_latest[midn_params$park == park])
# 
# QAQC <<- FALSE
# locType <<- 'VS'
# #+++ NOTE: IF GET add_header_above error, need to update cycle_latest in MIDN_params.csv  +++
# park_long <- midn_names$LongName[midn_names$ParkCode == park]
# park_title <- midn_names$LongName_title[midn_names$ParkCode == park]
# network_long <- midn_names$Network_long[midn_names$ParkCode == park]
# report_year <- 2024



# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$IsCanopyExclusion[VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$Genus == "Fraxinus"] <- TRUE

park_crs = ifelse(park %in% c("APCO", "BOWA"), 26917, 26918)

num_plots = case_when(park == "APCO" ~ 28,
                      park == "ASIS" ~ 24, # Will be 24
                      park == "BOWA" ~ 8,
                      park == "COLO" ~ 47, # Plot 380 has not been sampled since 2014. Change to 48, if re sampled?
                      park == "FRSP" ~ 104,
                      park == "GETT" ~ 33,
                      park == "GEWA" ~ 8,
                      park == "HOFU" ~ 16,
                      park == "PETE" ~ 52,
                      park == "RICH" ~ 31, #Should be 32, but plot 219 wasn't sampled in 2021. Plot count was already adjusted by KMM
                      park == "SAHI" ~ 4,
                      park == "THST" ~ 8,
                      park == "VAFO" ~ 28
                      )
plot_size = 400

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
parks <- c("APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", 
           "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO")

if(!dir.exists(paste0("./output/"))){dir.create(paste0("./output/"))}
if(!dir.exists(paste0("./output/", report_year))){dir.create(paste0("./output/", report_year, "/"))}
if(!dir.exists(paste0("./output/", report_year, "/MIDN/"))){dir.create(paste0("./output/", report_year, "/MIDN/"))}

invisible(lapply(parks, function(x) {
  if(!dir.exists(paste0(path, x))){dir.create(paste0(path, x))}
})
)

new_path = paste0(path, park, "/")

if(!dir.exists(new_path)){dir.create(new_path)}

folders <- c("ArcPro_projects", "figures", "map_exports", "shapefiles", "tables")

invisible(lapply(folders, function(x) {
  if(!dir.exists(paste0(new_path, x))){dir.create(paste0(new_path, x))}
})
)

# Read in tree species groups for regen and tree pie charts
trspp_grps <- read.csv("NPS_tree_species_groups.csv")

# Source files
# source('./scripts/forest_summary_code_MIDN.R')
# source('./scripts/regen_debt_metrics_MIDN.R')
# source('./scripts/tree_regen_stem_changes_by_species_loess_MIDN.R')

