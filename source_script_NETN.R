#--------------------------------
# Source Script for forestNETN Data Summaries
# Written by Kate Miller 6/30/2022
#--------------------------------

# Imports/Libraries
library(forestNETN)
library(forestTrends)
library(tidyverse)
library(sf)

#importData()
forestNETN::importCSV(path = "C:/Users/KMMiller/OneDrive - DOI/NETN/R_Dev/data/NETN_forest_baks/", 
                      zip_name = "NETN_Forest_20260528.zip")

# Fix 2026 data issues until they're resolved in DB
VIEWS_NETN$QuadSpecies_NETN$TSN[
  VIEWS_NETN$QuadSpecies_NETN$Plot_Name == "MORR-022" &
  VIEWS_NETN$QuadSpecies_NETN$SampleYear == 2026 &  
  VIEWS_NETN$QuadSpecies_NETN$TSN == 504874 &
  VIEWS_NETN$QuadSpecies_NETN$ScientificName == "Rubus pensilvanicus"] <- 25017

VIEWS_NETN$QuadSpecies_NETN$ScientificName[
  VIEWS_NETN$QuadSpecies_NETN$Plot_Name == "MORR-022" &
  VIEWS_NETN$QuadSpecies_NETN$SampleYear == 2026 &  
  VIEWS_NETN$QuadSpecies_NETN$ScientificName == "Rubus pensilvanicus"] <- "Rubus phoenicolasius"

# VIEWS_NETN$MicroplotSaplings_NETN$ScientificName[
#   VIEWS_NETN$MicroplotSaplings_NETN$Plot_Name == "ROVA-038" &
#   VIEWS_NETN$MicroplotSaplings_NETN$SampleYear == 2026 &
#   VIEWS_NETN$MicroplotSaplings_NETN$MicroplotCode == "B" &
#   VIEWS_NETN$MicroplotSaplings_NETN$ScientificName == "Acer saccharinum"] <- "Acer saccharum"
# 
# VIEWS_NETN$MicroplotSaplings_NETN$TSN[
#   VIEWS_NETN$MicroplotSaplings_NETN$Plot_Name == "ROVA-038" &
#     VIEWS_NETN$MicroplotSaplings_NETN$SampleYear == 2026 &
#     VIEWS_NETN$MicroplotSaplings_NETN$MicroplotCode == "B" &
#     VIEWS_NETN$MicroplotSaplings_NETN$TSN == 28757] <- 28731

# Downgrade Fraxinus to subcanopy species
VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fraxinus"] <- TRUE
#VIEWS_NETN$Taxa_NETN$IsCanopyExclusion[VIEWS_NETN$Taxa_NETN$Genus == "Fagus"] <- FALSE

# # Set parameters
# park = 'MORR'#'ACAD'
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
                      park == "MORR" ~ 28,
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

trspp_grps <- read.csv("NPS_tree_species_groups.csv")

# Source files
# source('./scripts/forest_summary_code_NETN.R')
# source('./scripts/regen_debt_metrics_NETN.R')
# source('./scripts/tree_regen_stem_changes_by_species_loess_NETN.R')

# 
# source("./scripts/forest_summary_code_ACAD.R")


