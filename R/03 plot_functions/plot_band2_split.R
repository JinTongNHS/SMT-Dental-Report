plot_band2_split <- function(data = band2_split_data,
                             scheduled_data = UDA_scheduled_data,
                             level = "National",
                             region_STP_name = NULL,
                             remove_prototypes = TRUE, 
                             plotChart = TRUE,
                             commissioner_region_lookup = STP_ICB_lookup_codes){
  
  #join in region column and annual contracted UDA column
  commissioner_region_lookup <- commissioner_region_lookup %>%
    select(commissioner_name = commissioner_name_ICB,
           region_name) %>%
    distinct()
  
  annual_contracted_UDA <- scheduled_data %>%
    select(month,
           contract_number,
           annual_contracted_UDA)
  
  data <- data %>%
    left_join(commissioner_region_lookup, by = "commissioner_name") %>%
    left_join(annual_contracted_UDA, by = c("month", "contract_number"))
  
  #exclude prototypes and contracts with <100 UDAs
  if(remove_prototypes == TRUE){
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  data <- data %>%
    select(month, band_2a_UDAs, band_2b_UDAs, band_2c_UDAs) %>%
    group_by(month) %>%
    summarise(band_2a_UDAs = sum(band_2a_UDAs, na.rm = TRUE), 
              band_2b_UDAs = sum(band_2b_UDAs, na.rm = TRUE), 
              band_2c_UDAs = sum(band_2c_UDAs, na.rm = TRUE)) %>%
    pivot_longer(cols = !month, names_to = "band", values_to = "UDAs") %>%
    mutate(band = case_when(band == "band_2a_UDAs" ~ "Band 2A",
                            band == "band_2b_UDAs" ~ "Band 2B",
                            band == "band_2c_UDAs" ~ "Band 2C"))
  
  if(plotChart == TRUE){
    ggplot(data,
           aes(x = month, 
               y = UDAs,
               colour = band)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      labs(title = "Band 2 UDA split",
           subtitle = subtitle,
           x = "Month",
           y = "UDAs",
           colour = "") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                         breaks = scales::breaks_pretty()) +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90))
  }else{
    data
  }
 
}