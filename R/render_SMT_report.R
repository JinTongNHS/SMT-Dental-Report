regions <- unique(UDA_calendar_data$region_name)
STPs <- unique(UDA_calendar_data$commissioner_name)



rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                  output_format = "beamer_presentation",
                  params = list(level = "National"),
                  output_file = paste0("../reports/national_pack_Nov/SMT data pres November ","National ", Sys.Date(),"v3.pdf"))



for(r in regions){
  rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                    output_format = "beamer_presentation",
                    params = list(level = "Regional", region_STP_name = r),
                    output_file = paste0("../reports/regional_packs_Nov/SMT data pres November ",r, " ",Sys.Date(),"v3.pdf"))
}


for(s in STPs){
  rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                    output_format = "beamer_presentation",
                    params = list(level = "STP", region_STP_name = s),
                    output_file = paste0("../reports/STP_packs_Nov/SMT data pres November ",s, " ",Sys.Date(),"v3.pdf"))
}