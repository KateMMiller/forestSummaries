# Code to generate the park-level Figures and Tables as html, then convert to pdf
library(pagedown)
library(purrr)
library(rmarkdown)

render_NETN_report <- function(parkcode, year){
    render(input = "NETN_figures_and_tables.Rmd",
           params = list(park = parkcode, report_year = year),
           output_file = paste0(parkcode, 
                                "_Figures_and_Tables_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = paste0(out_path, parkcode, "/"),
           output_options = list(self_contained = TRUE))
}

pdf_print <- function(parkcode){
  report_dir <- paste0(out_path)
  report_name <- paste0(parkcode, "_Figures_and_Tables_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, parkcode, "/", report_name, ".html"), 
               output = paste0(out_path, parkcode, "/", report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}

report_year = 2026
out_path = paste0('./output/', report_year, "/NETN/")

render_NETN_report("ROVA", 2026)
pdf_print("ROVA")

render_NETN_report("WEFA", 2026)
pdf_print("WEFA")

report_year = 2025
out_path = paste0('./output/', report_year, "/NETN/")

# render_NETN_report("ACAD", 2025)
# pdf_print("ACAD")

render_NETN_report("MABI", 2025)
pdf_print("MABI")

render_NETN_report("MIMA", 2025)
pdf_print("MIMA")

render_NETN_report("SAGA", 2025)
pdf_print("SAGA")

render_NETN_report("SARA", 2025)
pdf_print("SARA")

