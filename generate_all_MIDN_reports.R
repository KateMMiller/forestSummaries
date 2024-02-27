#--------------------------------------
# Script to generate MIDN Figures and Tables for all parks
#--------------------------------------
library(purrr)
library(rmarkdown)
library(knitr)
library(pagedown) # for chrome_print()

rmd_path <- c("C:/NETN/R_Dev/forestSummaries/")
out_path <- paste0(rmd_path, "/output/")
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
           output_dir = out_path,
           output_options = list(self_contained = TRUE))#,
  #encoding = "UTF-8")
}
#render_MIDN_reports(park = "COLO")

pdf_print <- function(park){
  report_dir <- paste0("C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/", 
                       park, "/", end_year, "/")
  report_name <- paste0(park, "_Figures_and_Tables_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, report_name, ".html"), 
               output = paste0(out_path, report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}
#pagedown::chrome_print("MIDN_figures_and_tables.html")

# Set up safe functions that continue with the next park upon error
render_poss <- possibly(render_MIDN_reports, otherwise = "Error")
print_poss <- possibly(pdf_print, otherwise = "Error")


midn_parks <- sort(unique(midn_params$park))
#"APCO" "BOWA" "COLO" "FRSP" "GETT" "GEWA" "HOFU" "PETE" "RICH" "SAHI" "THST" "VAFO"
purrr::walk(midn_parks, ~render_poss(park = .))
purrr::walk(midn_parks, ~print_poss(.))

render_MIDN_reports("SAHI")

