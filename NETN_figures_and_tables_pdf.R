# Code to generate the park-level Figures and Tables as html, then convert to pdf
library(pagedown)
library(purrr)
library(rmarkdown)

render_NETN_report <- function(parkcode, year){
    render(input = "NETN_figures_and_tables.Rmd",
           params = list(park = parkcode, report_year = year),
           #envir = VIEWS_NETN,
           output_file = paste0(parkcode, 
                                "_Figures_and_Tables_", 
                                format(Sys.time(), '%b_%Y'), ".html"),
           output_dir = out_path,
           output_options = list(self_contained = TRUE))
}

pdf_print <- function(parkcode){
  report_dir <- paste0(out_path)
  report_name <- paste0(parkcode, "_Figures_and_Tables_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, report_name, ".html"), 
               output = paste0(out_path, report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}

# Even year group of parks
report_year = 2026
out_path = paste0('./output/', report_year, "/NETN/")

parks = c("MORR", "ROVA", "WEFA")
years = rep(2026, 3)

#purrr::map2(parks, years, ~render_NETN_report(.x, .y))
render_NETN_report("MORR", 2026)
render_NETN_report("ROVA", 2026)
render_NETN_report("WEFA", 2026)

pdf_print("MORR") # not sure why purrr::map won't iterate on pdf_print
pdf_print("ROVA")
pdf_print("WEFA")

# Odd year group of parks
report_year = 2025
out_path = paste0('./output/', report_year, "/NETN/")

parks = c("MABI", "MIMA", "SAGA", "SARA")
years = rep(2025, 4)

purrr::map2(parks, years, ~render_NETN_report(.x, .y))
pdf_print("MABI") # not sure why purrr::map won't iterate on pdf_print
pdf_print("MIMA")
pdf_print("SAGA")
pdf_print("SARA")


