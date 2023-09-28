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

importData()

# Downgrade Fraxinus to subcanopy species
VIEWS_MIDN$Taxa_MIDN$IsCanopyExclusion[VIEWS_MIDN$Taxa_MIDN$Genus == "Fraxinus"] <- TRUE

# Params FRSP, RICH, PETE; from = 2007: from_4yr = 2021; cycle_latest = 5; from_prev = 2016; to_prev = 2019; 
# Params VAFO, HOFU, GETT, APCO, BOWA: from = 2007; from_4yr = 2019; cycle_latest = 4; from_prev = 2015; to_prev = 2018;
# Params GEWA, THST: from = 2008; from_4yr = 2021; cycle_latest = 4; from_prev = 2016; to_prev = 2019;
# Params COLO: from = 2011; from_4yr = 2019; cycle_latest = 3; from_prev = 2015; to_prev = 2018;
# Params SAHI: from = 2009; from_4yr = 2023; cycle_latest = 4; from_prev = 2017; to_prev = 2017;

# Set parameters
#-----MIDN1----
park = 'FRSP' #'RICH' #'PETE'
from = 2007
from_4yr = 2021
to = 2023
QAQC = FALSE
locType = 'VS'
cycle_latest = 5
from_prev = 2016
to_prev = 2019

#-----MIDN2-----
# park = 'BOWA' #'APCO'#'GETT' #'HOFU' #'VAFO'
# from = 2007
# from_4yr = 2019
# to = 2023
# QAQC = FALSE
# locType = 'VS'
# cycle_latest = 4
# from_prev = 2015
# to_prev = 2018

# table(VIEWS_MIDN$Plots_MIDN$ParkUnit, VIEWS_MIDN$Plots_MIDN$ZoneCode)
# locs <- joinLocEvent(park = 'all', from = 2019, to = 2023) |> select(ParkUnit, Plot_Name) |> unique()
# table(locs$ParkUnit)

park_crs = ifelse(park %in% c("APCO", "BOWA"), 26917, 26918)

num_plots = case_when(park == "APCO" ~ 28,
                      park == "ASIS" ~ 12, # Will be 24
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
path = 'C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/'

parks <- c("APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA", 
           "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO")

invisible(lapply(parks, function(x) {
  if(!dir.exists(paste0(path, x))){dir.create(paste0(path, x))}
})
)

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
source('./scripts/forest_summary_code_MIDN.R')
source('./scripts/regen_debt_metrics_MIDN.R')
source('./scripts/tree_regen_stem_changes_by_species_loess_MIDN.R')
