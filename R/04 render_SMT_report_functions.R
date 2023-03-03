
regions <- unique(UDA_scheduled_data$region_name)

render_report = function(region) {
  rmarkdown::render(
    "rmarkdown/SMT_dental_report_region_ICB_level.rmd", params = list(
      region = region),
    output_file = paste0("03 2023 SMT Dental Pack ", gsub(" ", "", region)," ICBs - reporting up to end of February 2023.html")
  )
}

for(r in regions){
  render_report(r)
}
