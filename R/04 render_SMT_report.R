regions <- unique(UDA_calendar_data$region_name)
STPs <- unique(UDA_calendar_data$commissioner_name)



rmarkdown::render(input = "rmarkdown/SMT-Dental-Report-Slides.Rmd",
                  output_format = "beamer_presentation",
                  params = list(level = "National"),
                  output_file = paste0("../reports/national_pack_Apr/SMT data pres April ","National ", Sys.Date()," v2.pdf"))


for(r in regions){
  rmarkdown::render(input = "rmarkdown/SMT_dental_xaringan_ICB.Rmd",
                    output_format = "beamer_presentation",
                    params = list(level = "Regional", region_STP_name = r),
                    output_file = paste0("../reports/regional_packs_Apr/SMT data pres April ",r, " ",Sys.Date()," v1.pdf"))
}

