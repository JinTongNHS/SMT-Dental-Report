rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                  output_format = "powerpoint_presentation",
                  output_file = paste0("../reports/SMT-Dental-Report-powerpoint-",Sys.Date(),".pptx"))



rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                  output_format = "beamer_presentation",
                  params = list(level = "Regional", region_STP_name = "London"),
                  output_file = paste0("../reports/SMT data pres October ","London ", Sys.Date(),".pdf"))



for(r in regions){
  rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                    output_format = "beamer_presentation",
                    params = list(level = "Regional", region_STP_name = r),
                    output_file = paste0("../reports/regional_packs/SMT data pres October ",r, Sys.Date(),".pdf"))
}


for(s in STPs){
  rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                    output_format = "beamer_presentation",
                    params = list(level = "STP", region_STP_name = s),
                    output_file = paste0("../reports/STP_packs/SMT data pres October ",s, Sys.Date(),".pdf"))
}