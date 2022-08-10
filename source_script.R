#--------------------------------
# Source Script for forestNETN Data Summaries
# Written by Kate Miller 6/30/2022
#--------------------------------

# Imports/Libraries
library(forestNETN)
library(forestTrends)
library(tidyverse)
library(sf)

importData()

# Downgrade Fraxinus to subcanopy species
VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fraxinus"] <- TRUE

# Set parameters
park = 'MORR'
from = 2006
from_4yr = 2018
to = 2022
QAQC = FALSE
locType = 'all'
cycle_latest = 4
park_crs = ifelse(park %in% c("ACAD", "MIMA"), 26919, 26918)

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
path = 'D:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'
new_path = paste0(path, park, "/", as.character(to), "/")

if(!dir.exists(new_path)){dir.create(new_path)}

folders <- c("ArcMap_projects", "figures", "map_exports", "shapefiles", "tables")

invisible(lapply(folders, function(x) {
         if(!dir.exists(paste0(new_path, x))){dir.create(paste0(new_path, x))}
  })
)

# Source files
source('./scripts/forest_summary_code.R')
