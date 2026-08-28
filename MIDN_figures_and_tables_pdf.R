# Code to generate the park-level Figures and Tables as html, then convert to pdf
library(pagedown)
library(purrr)
library(rmarkdown)

render_MIDN_report <- function(parkcode, year){
    render(input = "MIDN_figures_and_tables.Rmd",
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
out_path = paste0('./output/', report_year, "/MIDN/")

#--- Couldn't get the code below to work, so generating one at a time ---
# parks = c("VAFO", "HOFU", "GETT", "APCO", "BOWA", "COLO", "PETE")
# years = rep(2026, length(parks))
# purrr::map2(parks, years, ~render_MIDN_report(.x, .y))

render_MIDN_report("VAFO", 2026)
render_MIDN_report("HOFU", 2026)
render_MIDN_report("GETT", 2026)
render_MIDN_report("APCO", 2026)
render_MIDN_report("BOWA", 2026)
render_MIDN_report("COLO", 2026)
render_MIDN_report("PETE", 2026)
render_MIDN_report("GEWA", 2026)
render_MIDN_report("THST", 2026)
#render_MIDN_report("ASIS", 2026)
#render_MIDN_report("FRSP", 2026)
#render_MIDN_report("RICH", 2026)

pdf_print("VAFO") # not sure why purrr::map won't iterate on pdf_print
pdf_print("HOFU")
pdf_print("GETT")
pdf_print("APCO") 
pdf_print("BOWA")
pdf_print("COLO")
pdf_print("PETE")
pdf_print("GEWA")
pdf_print("THST")
# pdf_print("ASIS")
# pdf_print("FRSP")
# pdf_print("RICH")

render_MIDN_report_subunit <- function(parkcode, sub, year){
  render(input = "MIDN_figures_subunits.Rmd",
         params = list(park = parkcode, subunit = sub, report_year = year),
         #envir = VIEWS_NETN,
         output_file = paste0(parkcode, "_", sub,  
                              "_Figures_and_Tables_", 
                              format(Sys.time(), '%b_%Y'), ".html"),
         output_dir = out_path,
         output_options = list(self_contained = TRUE))
}

pdf_print_subunit <- function(parkcode, sub){
  report_dir <- paste0(out_path)
  report_name <- paste0(parkcode, "_", sub, "_Figures_and_Tables_", format(Sys.time(), "%b_%Y"))
  chrome_print(input = paste0(out_path, report_name, ".html"), 
               output = paste0(out_path, report_name, ".pdf"),
               format = 'pdf')
  cat('Report printed to: ', paste0(out_path, report_name, ".pdf"))
}
render_MIDN_report_subunit("PETE", "PETE_FIVE", 2026)
render_MIDN_report_subunit("PETE", "PETE_EAST", 2026)

pdf_print_subunit("PETE", "PETE_FIVE")
pdf_print_subunit("PETE", "PETE_EAST")
