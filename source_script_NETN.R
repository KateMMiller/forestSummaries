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
#VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fagus"] <- FALSE

# Set parameters
park = 'ACAD'
from = 2006
from_4yr = 2021
to = 2023
QAQC = FALSE
locType = 'all'
cycle_latest = 5
park_crs = ifelse(park %in% c("ACAD", "MIMA"), 26919, 26918)
num_plots = case_when(park == "ACAD" ~ 176,
                      park == "MABI" ~ 24,
                      park == "MIMA" ~ 20,
                      park == "MORR" ~ 28,
                      park == "ROVA" ~ 40,
                      park == "SAGA" ~ 21,
                      park == "SARA" ~ 32,
                      park == "WEFA" ~ 10)
plot_size = ifelse(park == "ACAD", 225, 400)
from_prev = 2016
to_prev = 2019

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
path = 'C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'
new_path = paste0(path, park, "/", as.character(to), "/")

if(!dir.exists(new_path)){dir.create(new_path)}

folders <- c("ArcMap_projects", "figures", "map_exports", "shapefiles", "tables")

invisible(lapply(folders, function(x) {
         if(!dir.exists(paste0(new_path, x))){dir.create(paste0(new_path, x))}
  })
)

# Source files
source('./scripts/forest_summary_code_NETN.R')
source('./scripts/regen_debt_metrics.R')

source("./scripts/forest_summary_code_ACAD.R")
