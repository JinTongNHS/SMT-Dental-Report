################################################################################
plot_banded_CoT <- function(data = UDA_scheduled_data, 
                            calendar_data = UDA_calendar_data,
                            historic_data = historical_UDA_scheduled_data, 
                            level = "National",
                            region_STP_name = NULL,
                            plotChart = TRUE, 
                            all_regions_and_STPs = FALSE,
                            asIndex = FALSE,
                            remove_prototypes = TRUE){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))
  
  #toggle subtitle
  if(level == "Regional"){
    #filter for region or STP
    data <- data %>% 
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    #filter for region or STP
    data <- data %>% 
      filter(commissioner_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(commissioner_name == region_STP_name )
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #get data into the right format
  data_to_print <- get_banded_COTs_data(data, historic_data = historic_data, remove_prototypes = remove_prototypes, all_regions_and_STPs = all_regions_and_STPs)
  data <- reshape2::melt(data_to_print, id.vars = "month")
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    rename(band = variable, CoTs = value)
  
  #set everything as index of 2019
  if(asIndex == TRUE){
    
    data <- data %>%
      mutate(month_number = substr(month, 6, 7),
             year_number = substr(month, 1, 4))
    
    data2019 <- data %>%
      filter(year_number == "2019") %>%
      select(month_number,
             band,
             CoTs_2019 = CoTs)
    
    data <- data %>%
      left_join(data2019, by = c("band", "month_number")) %>%
      mutate(CoTs = CoTs * 100 / CoTs_2019)
    
    title <- "Banded Courses of Treatment as a Percentage of corresponding month of 2019 Delivery"
    ylab <- "Percentage of 2019 FP17* forms submitted"
    
  }else{
    title <- "Banded Courses of Treatment"
    ylab <- "Number of FP17* forms submitted"
  }
  
  if(plotChart == TRUE){
    
    #plot code
    ggplot(data) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      geom_line(aes(x = month,
                    y = CoTs,
                    colour = band),
                size = 1) +
      geom_point(aes(x = month,
                     y = CoTs,
                     colour = band),
                 size = 1) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      scale_colour_manual(labels = c("Band 1", "Band 2", "Band 3", "Other", "Urgent"),
                          values = c("coral3",
                                     "orange",
                                     "yellow3",
                                     "green",
                                     "blue")
      ) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      labs(title = title,
           x = "Month",
           y = ylab,
           colour = "Band",
           subtitle = subtitle,
           caption = "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
           **UDA to FP17 conversion has been done assuming a band 1 FP17
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs,
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2
         UDAs and an 'other' FP17 = 0.6 UDAs. Scheduled data used.")
    
  }else{
    data_to_print %>%
      mutate(month = as.Date(month)) %>%
      rename(Month = month,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Band 1` = band1,
             `Band 2` = band2,
             `Band 3` = band3,
             `Other` = other,
             `Urgent` = urgent)
  }
  
}