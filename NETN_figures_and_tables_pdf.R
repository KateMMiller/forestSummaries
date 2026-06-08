# Code to generate the park-level Figures and Tables as html, then convert to pdf
library(pagedown)
library(purrr)
library(rmarkdown)

park = "MORR"
report_year = 2026
out_path = paste0('./output/', report_year, "/NETN/", park, "/")

render_NETN_report <- function(park){
    render(input = "NETN_figures_and_tables.Rmd",
           output_file = paste0(park, 
                                "_Figures_and_Tables_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))
}

pdf_print <- function(park){
  report_dir <- paste0(out_path)
  report_name <- paste0(park, "_Figures_and_Tables_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, report_name, ".html"), 
               output = paste0(out_path, report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}

render_NETN_report("MORR")
pdf_print("MORR")
