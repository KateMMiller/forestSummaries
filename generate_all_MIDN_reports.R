#--------------------------------------
# Script to generate MIDN Figures and Tables for all parks
#--------------------------------------
library(purrr)
library(rmarkdown)
library(knitr)
library(pagedown) # for chrome_print()

# 20240923: chrome_print is failing to render a .pdf. Need to install servr package for fix:
#devtools::install_github('yihui/servr')
library(servr)

# puts output from data summaries.
report_year = 2024

#rmd_path <- c("C:/01_NETN/Forest_Health/R_Dev/forestSummaries/") # location where .RMD lives (make sure ends with /)
#rmd_path = paste0(getwd(), "/") 
#if(!dir.exists(paste0(rmd_path, "output"))){dir.create(paste0(rmd_path, "output"))}
if(!dir.exists(paste0(getwd(), "/output/", "MIDN/"))){dir.create(paste0(getwd(), "/output/MIDN"))} 
if(!dir.exists(paste0(getwd(), "/output/", "MIDN/", report_year))){dir.create(paste0(getwd(), "/output/MIDN/", report_year))} # year folder

out_path <- paste0(getwd(), "/output/", report_year, "/MIDN/") # make sure there's an output folder in path above
midn_params <- read.csv("MIDN_params.csv")
# report_path <- "C:/NETN/Monitoring_Projects/Forest_Health/Data_Summaries/" #path where source script
report_path <- out_path #'C:/01_NETN/Forest_Health/Data_Summaries/2024 Data Summaries/MIDN/'

render_MIDN_reports <- function(park){
  
  outpath = paste0(report_path, park, "/", report_year, "/")
  
  if(park == "SAHI"){
    
    render(input = "SAHI_figures_and_tables.Rmd",
           output_file = paste0(park, 
                                "_Figures_and_Tables_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))#,
    
  } else if(park == "ASIS"){
    
    render(input = "ASIS_figures_and_tables.Rmd",
           output_file = paste0(park, 
                                "_Figures_and_Tables_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))#,
    
  } else {
  
    render(input = "MIDN_figures_and_tables.Rmd",
           params = list(park = park), 
           output_file = paste0(park, 
                              "_Figures_and_Tables_", 
                              format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))#,
  #encoding = "UTF-8")
  }
}

pdf_print <- function(park){
#  report_dir <- paste0(report_path, park, "/", report_year, "/")
  report_dir <- paste0(out_path)
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


#midn_parks <- sort(unique(midn_params$park))
midn_parks <- c("APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT",
                "GEWA", "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO")

#Test if a few individual parks are working
render_MIDN_reports("BOWA")
pdf_print("BOWA")
render_poss("BOWA")
print_poss(("APCO"))

# print_poss("SAHI")

purrr::walk(midn_parks, ~render_poss(park = .))
purrr::walk(midn_parks, ~print_poss(.))



# Generate reports for MIDN subunits --------------------------------------
render_MIDN_subunit_reports <- function(park, subunit){
  
  outpath = paste0(report_path, park, "/", report_year, "/")
  
  render(input = "MIDN_figures_subunits.Rmd",
           params = list(park = park, subunit = subunit, report_year = report_year), 
           output_file = paste0(subunit, 
                                "_Figures_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))#,
    #encoding = "UTF-8")
}

pdf_subunit_print <- function(park, subunit){
  #  report_dir <- paste0(report_path, park, "/", report_year, "/")
  report_dir <- paste0(out_path)
  report_name <- paste0(subunit, "_Figures_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, report_name, ".html"), 
               output = paste0(out_path, report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}


# Set up safe functions that continue with the next park upon error
render_subunit_poss <- possibly(render_MIDN_subunit_reports, otherwise = "Error")
print_subunit_poss <- possibly(pdf_subunit_print, otherwise = "Error")


#Test if a few individual subunits are working
render_MIDN_subunit_reports("FRSP", "FRSP_FRED")
pdf_subunit_print("FRSP", "FRSP_FRED")

frsp_subunits <- c("FRSP_FRED", "FRSP_SPOT", "FRSP_CHWILD")
pete_subunits <- c("PETE_FIVE", "PETE_EAST") 

#Have to do FRSP and PETE separately
purrr::walk(pete_subunits, ~render_subunit_poss(park = "PETE", subunit = .))
purrr::walk(pete_subunits, ~print_subunit_poss(park = "PETE", subunit = .))

purrr::walk(frsp_subunits, ~render_subunit_poss(park = "FRSP", subunit = .))
purrr::walk(frsp_subunits, ~print_subunit_poss(park = "FRSP", subunit = .))
