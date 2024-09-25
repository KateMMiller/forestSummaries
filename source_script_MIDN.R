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


if(!exists("path")){path = 'C:/01_NETN/Forest_Health/Data_Summaries/2024 Data Summaries/MIDN/'}
#Kate's path: C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'

#park = "FRSP"
if(!exists("path")){path = 'C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'}

importData()


# assign params to global env. for source files to find. Makes iterating easier.
#temp: so can run individual parks w/ .rmd file
park <<- 'GETT'
midn_names <- read.csv("MIDN_MetaData.csv")
midn_params <- read.csv("MIDN_params.csv") # !!!! MUST UPDATE EVERY YEAR !!!!
path <<- 'C:/01_NETN/Forest_Health/Data_Summaries/2024 Data Summaries/MIDN/'
from <<- as.numeric(midn_params$from[midn_params$park == park])
from_4yr <<- as.numeric(midn_params$from_4yr[midn_params$park == park])
to <<- as.numeric(midn_params$to[midn_params$park == park])
cycle_latest <<- as.numeric(midn_params$cycle_latest[midn_params$park == park])
from_prev <<- as.numeric(midn_params$from_prev[midn_params$park == park])
to_prev <<- as.numeric(midn_params$to_prev[midn_params$park == park])
QAQC <<- FALSE
locType <<- 'VS'

park_long <- midn_names$LongName[midn_names$ParkCode == park]
park_title <- midn_names$LongName_title[midn_names$ParkCode == park]
network_long <- midn_names$Network_long[midn_names$ParkCode == park]



# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN$Taxa_MIDN$IsCanopyExclusion[VIEWS_MIDN$Taxa_MIDN$Genus == "Fraxinus"] <- TRUE

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
