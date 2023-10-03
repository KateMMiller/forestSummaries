#--------------------------------------
# Script to generate MIDN Figures and Tables for all parks
#--------------------------------------
library(purrr)
library(rmarkdown)
library(knitr)

rmd_path <- c("C:/NETN/R_Dev/forestSummaries/")
midn_params <- read.csv("MIDN_params.csv")

end_year = 2023

render_MIDN_reports <- function(park){
  
  outpath = paste0("C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/",
                   park, "/", end_year, "/")
  
    render(input = paste0(rmd_path, "MIDN_figures_and_tables.Rmd"),
           params = list(park = park), 
           output_file = paste0(park, 
                              "_Figures_and_Tables_", 
                              format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = outpath,
           output_options = list(self_contained = TRUE))#,
  #encoding = "UTF-8")
   }

render_poss <- possibly(render_MIDN_reports, otherwise = "Error")

render_MIDN_reports(park = "COLO")

midn_parks <- sort(unique(midn_params$park))

purrr::walk(midn_parks, ~render_MIDN_reports(park = .))
            
purrr::walk(midn_parks[4:11], ~render_poss(park = .))
# Failed on COLO in regen_debt_metrics_MIDN.R 221