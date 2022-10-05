################################################################################
#function to plot delivery profile month by month 
plot_UDA_UOA_delivery_profile <- function(data = UDA_scheduled_data, 
                                          calendar_data = UDA_calendar_data,
                                          historic_data = historical_UDA_scheduled_data,
                                          UDAorUOA = "UDA",
                                          level = "National",
                                          region_STP_name = NULL,
                                          plotChart = TRUE,
                                          all_regions_and_STPs = FALSE,
                                          include_historic = TRUE){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = "commissioner_name")
  
  #bind in historic data if required
  if(include_historic == TRUE & UDAorUOA == "UDA"){
    data <- data %>%
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, UDA_delivered)
    
    historic_data <- historic_data %>%
      mutate(month = as.Date(month)) %>%
      filter(!is.na(annual_contracted_UDA)) %>% #filters out contracts with no data on contracted UDAs
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, 
             UDA_delivered)
    
    data <- bind_rows(data, historic_data)
    
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
  
  #get data into the right format
  data <- get_delivery_profile_data(data = data, UDAorUOA = UDAorUOA, remove_prototypes = T, all_regions_and_STPs = all_regions_and_STPs)
  
  #change title for UDA or UOA
  if(UDAorUOA == "UDA"){
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UDA per month"
    legTitle <- "Performance band of \nUDA delivery"
    captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
  }else{
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UOA per month"
    legTitle <- "Performance band of \nUOA delivery"
    captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
  }
  
  #get bars in the correct order
  data$performance_band <- factor(data$performance_band, levels = c("0-9%","10-19%", 
                                                                    "20-29%", "30-39%", "40-49%", "50-59%",
                                                                    "60-69%", "70-79%", "80-89%", "90-99%",
                                                                    "100% +"))
  #colour blind friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                 "#52854C", "#4E84C4", "#293352", "#FFDB6D")
  
  if(plotChart == TRUE){
    #plot code
    ggplot(data, 
           aes(fill = performance_band, 
               y = perc_of_contracts, 
               x = month)) +
      geom_bar(position = "dodge", 
               stat = "identity") +
      labs(title = title,
           x = "Month",
           y = "Percentage of contracts delivering in this band",
           fill = legTitle,
           subtitle = subtitle,
           caption = captionTitle) +
      #scale_fill_manual(values = cbPalette) +
      scale_x_datetime(breaks = data$month, 
                       labels = scales::date_format("%b-%y")) +
      geom_vline(xintercept = as.Date("2020-07-01"), colour = "black", size = 5) +
      scale_y_continuous(limits = c(0, 55)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }else{
    data %>%
      mutate(month = as.Date(month)) %>%
      rename(Month = "month", 
             `Region Name` = "region_name", 
             `Commissioner Name` = "commissioner_name", 
             `Performance band` = "performance_band", 
             `Number of contracts in performance band`= "n", 
             `Total number of contracts` = "no_of_contracts", 
             `Percentage of contracts in performance band` = "perc_of_contracts")
  }
  
}
