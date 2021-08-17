rmarkdown::render(input = "SMT-Dental-Report-Slides.Rmd",
                  output_format = "beamer_presentation",
                  output_file = paste0("reports/SMT-Dental-Report-",Sys.Date(),".pdf"))
