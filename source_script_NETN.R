#--------------------------------
# Source Script for forestNETN Data Summaries
# Written by Kate Miller 6/30/2022
#--------------------------------

# Imports/Libraries
library(forestNETN)
library(forestTrends)
library(tidyverse)
library(sf)
library(ggpubr)

#if(!exists("VIEWS_NETN", envir = VIEWS_NETN)){
#importData()
#   importCSV(path = "../data/", zip_name = 'NETN_Forest_20250926.zip') #DP from 2025
   #importCSV(path = "../data/", zip_name = 'NETN_Forest_20260612.zip') #DP from 2025
#importCSV(path = "../data", zip_name = "NETN_Forest_20260626.zip")   
   #  }

# Fix 2026 data issues until they're resolved in DB
# Downgrade Fraxinus to subcanopy species
VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fraxinus"] <- TRUE
#VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fagus"] <- FALSE

# # Set parameters
# park = 'WEFA'
# from = 2006
# from_4yr = 2023
# to = 2026
# report_year = 2026 # used for file path and output naming, in case differs from last year sampled
# QAQC = FALSE
# locType = 'all'
# cycle_latest = 5
park_crs = ifelse(park %in% c("ACAD", "MIMA"), 26919, 26918)
num_plots = case_when(park == "ACAD" ~ 176,
                      park == "MABI" ~ 24,
                      park == "MIMA" ~ 20,
                      park == "MORR" ~ 29, # including plot 14 in all but regen_debt_metrics_NETN and tree_regen_stem_changes
                      park == "ROVA" ~ 40,
                      park == "SAGA" ~ 21,
                      park == "SARA" ~ 32,
                      park == "WEFA" ~ 10)
plot_size = ifelse(park == "ACAD", 225, 400)
# from_prev = 2023
# to_prev = 2026

args_all = list(park = park, from = from, to = to, QAQC = QAQC, locType = locType)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, locType = locType)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, locType = "VS")

# Set up file structure
# report_year = 2024 # only here for testing. Defined in MIDN_figures_and_tables.RMD params.
if(!exists("path")){path = paste0('./output/', report_year, "/NETN/")} #general path that should work for everyone

invisible(lapply(park, function(x) {
  if(!dir.exists(paste0(path, x))){dir.create(paste0(path, x))}
})
)
# Set up file structure
#parks <- c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
#parks <- c("ACAD", "MABI", "MIMA", "SAGA", "SARA")
parks <- c("ACAD", "MORR", "ROVA", "WEFA")

if(!dir.exists(paste0("./output/", report_year))){dir.create(paste0("./output/", report_year, "/"))}
if(!dir.exists(paste0("./output/", report_year, "/NETN/"))){dir.create(paste0("./output/", report_year, "/NETN/"))}

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

# Set up cycle labels for figures
# netn1+2: SARA, MABI, SAGA, MIMA
netn1_labs = c("1" = "Cycle 1: 2006 & 2008",
               "2" = "Cycle 2: 2010 & 2012", 
               "3" = "Cycle 3: 2014 & 2016", 
               "4" = "Cycle 4: 2018 & 2022",
               "5" = "Cycle 5: 2023 & 2025")
# netn2: MORR, ROVA, WEFA
netn2_labs = c("1" = "Cycle 1: 2007 & 2009",
               "2" = "Cycle 2: 2011 & 2013", 
               "3" = "Cycle 3: 2015 & 2017", 
               "4" = "Cycle 4: 2019 & 2022",
               "5" = "Cycle 5: 2024 & 2026") 

ACAD_labs = c("1" = "Cycle 1: 2006 \u2013 2009",
              "2" = "Cycle 2: 2010 \u2013 2013", 
              "3" = "Cycle 3: 2014 \u2013 2017", 
              "4" = "Cycle 4: 2018 \u2013 2021",
              "5" = "Cycle 5: 2022 \u2013 2025",
              "6" = "Cycle 6: 2026")

cycle_labs <- switch(park,
                     "SARA" = netn1_labs, 
                     "MABI" = netn1_labs,
                     "SAGA" = netn1_labs,
                     "MIMA" = netn1_labs,
                     "MORR" = netn2_labs,
                     "ROVA" = netn2_labs,
                     "WEFA" = netn2_labs,
                     "ACAD" = ACAD_labs)

trspp_grps <- read.csv("NPS_tree_species_groups.csv")

# Source files
# source('./scripts/forest_summary_code_NETN.R')
# source('./scripts/regen_debt_metrics_NETN.R')
# source('./scripts/tree_regen_stem_changes_by_species_loess_NETN.R')

# 
# source("./scripts/forest_summary_code_ACAD.R")


