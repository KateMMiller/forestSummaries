#--------------------------------
# Source Script for forestMIDN Data Summaries
# Written by Kate Miller 9/13/2023
#--------------------------------

# Note that the stocking index calculated in both the summary code and regen debt code is on the 
# McWilliams 100 point scale, not the 1m2 scale that MIDN used to use.

# Imports/Libraries
library(forestMIDN)
library(forestTrends)
library(tidyverse)
library(sf)

# report_year = 2024 # only here for testing. Defined in MIDN_figures_and_tables.RMD params.
if(!exists("path")){path = paste0('./output/', report_year, "/MIDN/")} #general path that should work for everyone

importData()

# midn_names <- read.csv("MIDN_MetaData.csv")
# midn_params <- read.csv("MIDN_params.csv") # !!!! MUST UPDATE EVERY YEAR !!!!
# park <<- 'ASIS'
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

# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$IsCanopyExclusion[VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$Genus == "Fraxinus"] <- TRUE

park_crs = 26918

num_plots = 24
plot_size = 400

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
#park <- "ASIS"

if(!dir.exists(paste0("./output/", report_year))){dir.create(paste0("./output/", report_year, "/"))}
if(!dir.exists(paste0("./output/", report_year, "/MIDN/"))){dir.create(paste0("./output/", report_year, "/MIDN/"))}
if(!dir.exists(paste0(path, park))){dir.create(paste0(path, park))}

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
# source('./scripts/forest_summary_code_ASIS.R')
# source('./scripts/regen_debt_metrics_ASIS.R')
# source('./scripts/tree_regen_stem_density_by_species_ASIS.R')
