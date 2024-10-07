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

if(!exists("path")){path = 'C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'}

importData()

# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$IsCanopyExclusion[VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN$Genus == "Fraxinus"] <- TRUE

park_crs = 26918

num_plots = 4
plot_size = 400

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
park <- "SAHI"

if(!dir.exists(paste0(path, park))){dir.create(paste0(path, park))}

new_path = paste0(path, park, "/", as.character(to), "/")

if(!dir.exists(new_path)){dir.create(new_path)}

folders <- c("GIS_projects", "figures", "map_exports", "shapefiles", "tables")

invisible(lapply(folders, function(x) {
  if(!dir.exists(paste0(new_path, x))){dir.create(paste0(new_path, x))}
})
)

# Read in tree species groups for regen and tree pie charts
trspp_grps <- read.csv("NPS_tree_species_groups.csv")

# Source files
# source('./scripts/forest_summary_code_SAHI.R')
# source('./scripts/regen_debt_metrics_SAHI.R')
# source('./scripts/tree_regen_stem_changes_by_species_loess_SAHI.R')
