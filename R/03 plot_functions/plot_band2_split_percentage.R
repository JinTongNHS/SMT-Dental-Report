#graph to plot UDA split band 2
plot_band2_split_percentage <- function(data = band2_split_data,
                                        scheduled_data = UDA_scheduled_data,
                                        level = "National",
                                        region_STP_name = NULL,
                                        remove_prototypes = FALSE, 
                                        plotChart = TRUE,
                                        commissioner_region_lookup = STP_ICB_lookup_codes,
                                        UDA_or_FP17 = "FP17"){
  
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
    
    chartCaption <- "*EXCLUDING contracts with annual contracted UDAs < 100.\n**Some band 2s are still being submitted without the A,B,C distinction and are classed here as 'un-split'"
  }else{
    
    chartCaption <- "*INCLUDING contracts with annual contracted UDAs < 100.\n**Some band 2s are still being submitted without the A,B,C distinction and are classed here as 'un-split'"
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
  
  if(UDA_or_FP17 == "UDA"){
    data <- data %>%
      select(month, band_2a_UDAs, band_2b_UDAs, band_2c_UDAs, total_band2_UDAs_delivered) %>%
      mutate(unsplit_band2s = total_band2_UDAs_delivered - band_2a_UDAs - band_2b_UDAs - band_2c_UDAs) %>%
      group_by(month) %>%
      summarise(band_2a_UDAs = sum(band_2a_UDAs, na.rm = TRUE), 
                band_2b_UDAs = sum(band_2b_UDAs, na.rm = TRUE), 
                band_2c_UDAs = sum(band_2c_UDAs, na.rm = TRUE),
                unsplit_band2s = sum(unsplit_band2s, na.rm = TRUE),
                total_band2_UDAs_delivered = sum(total_band2_UDAs_delivered, na.rm = TRUE)) %>%
      pivot_longer(cols = c(band_2a_UDAs, band_2b_UDAs, band_2c_UDAs, unsplit_band2s), names_to = "band", values_to = "count") %>%
      mutate(percentage = count / total_band2_UDAs_delivered) %>%
      mutate(band = case_when(band == "band_2a_UDAs" ~ "Band 2A",
                              band == "band_2b_UDAs" ~ "Band 2B",
                              band == "band_2c_UDAs" ~ "Band 2C",
                              band == "unsplit_band2s" ~ "Un-split band 2**"))
  }else if(UDA_or_FP17 == "FP17"){
    data <- data %>%
      select(month, band_2a_FP17s, band_2b_FP17s, band_2c_FP17s, total_band2_FP17s_delivered) %>%
      mutate(unsplit_band2s = total_band2_FP17s_delivered - band_2a_FP17s - band_2b_FP17s - band_2c_FP17s) %>%
      group_by(month) %>%
      summarise(band_2a_FP17s = sum(band_2a_FP17s, na.rm = TRUE), 
                band_2b_FP17s = sum(band_2b_FP17s, na.rm = TRUE), 
                band_2c_FP17s = sum(band_2c_FP17s, na.rm = TRUE),
                unsplit_band2s = sum(unsplit_band2s, na.rm = TRUE),
                total_band2_FP17s_delivered = sum(total_band2_FP17s_delivered, na.rm = TRUE)) %>%
      pivot_longer(cols = c(band_2a_FP17s, band_2b_FP17s, band_2c_FP17s, unsplit_band2s), names_to = "band", values_to = "count") %>%
      mutate(percentage = count / total_band2_FP17s_delivered) %>%
      mutate(band = case_when(band == "band_2a_FP17s" ~ "Band 2A",
                              band == "band_2b_FP17s" ~ "Band 2B",
                              band == "band_2c_FP17s" ~ "Band 2C",
                              band == "total_band2_FP17s_delivered" ~ "Total band 2",
                              band == "unsplit_band2s" ~ "Un-split band 2**"))
    
  }
  
  data$band <- factor(data$band, levels = c("Total band 2", "Band 2A", "Band 2B", "Band 2C", "Un-split band 2**"))
  
  if(plotChart == TRUE){
    
    ggplot(data,
           aes(x = month, 
               y = percentage,
               colour = band)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      labs(title = paste0("Percentage of total band 2 ",UDA_or_FP17,"s delivered by band 2 category"),
           subtitle = subtitle,
           x = "Month",
           y = paste0("Percentage of total band 2 ", UDA_or_FP17, "s delivered"),
           colour = "",
           caption = chartCaption) +
      scale_y_continuous(breaks = scales::breaks_pretty(),
                         labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      scale_colour_manual(values = get_colour_palette()) +
      theme(axis.text.x = element_text(angle = 90))
    
  }else if(plotChart == FALSE & UDA_or_FP17 == "UDA"){
    
    data <- data %>%
      mutate(percentage = round(percentage, 1) * 100) %>%
      rename(Month = month,
             `Total band 2 UDAs delivered` = total_band2_UDAs_delivered,
             `Band 2 sub band` = band,
             `Number of UDAs` = count,
             `Percentage of total band 2 UDAs delivered` = percentage)
    
  }else if(plotChart == FALSE & UDA_or_FP17 == "FP17"){
    
    data <- data %>%
      mutate(percentage = round(percentage, 1) * 100) %>%
      rename(Month = month,
             `Total band 2 FP17s delivered` = total_band2_FP17s_delivered,
             `Band 2 sub band` = band,
             `Number of FP17s` = count,
             `Percentage of total band 2 FP17s delivered (%)` = percentage)
    
  }
  
}