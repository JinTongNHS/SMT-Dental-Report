plot_YTD_UDA_UOA_delivery <- function(data = UDA_scheduled_data, 
                                      historic_data = historical_UDA_scheduled_data,
                                      UDAorUOA = "UDA",
                                      level = "National",
                                      region_STP_name = NULL,
                                      remove_prototypes = TRUE, 
                                      plotChart = TRUE, 
                                      all_regions_and_STPs = FALSE){

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
  
  if(UDAorUOA == "UDA"){
    #get data into the right format
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UDA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "YTD UDA delivery across all contracts* against annual contracted UDAs (millions)"
    ylab <- "YTD UDA delivery (millions)"
    captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April -
                    21st April therefore the April data has been scaled up by 18 instead of 12."
    
    lineCol <- "#CC79A7"
  }else{
    #get data into the right format
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UOA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "YTD UOA delivery across all contracts* against annual contracted UOAs (millions)"
    ylab <- "YTD UOA delivery (millions)"
    captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April -
                    21st April therefore the April data has been scaled up by 18 instead of 12."
    lineCol <- "steelblue"
  }
  
  #add year to date delivery column
  data <- data %>%
    filter(month >= as.Date("2023-04-01")) %>% ##must be updated each financial year
    mutate(YTD_delivery = cumsum(monthly_UDA_UOAs_delivered))
  
  #get the mean annual contracted UDAs across the months
  mean_annual_contracted_UDA_UOAs <- mean(data$annual_contracted_UDA_UOA)
  
  #add in blanks for the rest of the year
  if(all_regions_and_STPs == FALSE & nrow(data) < 12){
    
    if(!(as.Date("2023-05-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-05-01"))
    }
    
    if(!(as.Date("2023-06-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-06-01"))
    }
    
    if(!(as.Date("2023-07-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-07-01"))
    }
    
    if(!(as.Date("2023-08-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-08-01"))
    }
    
    if(!(as.Date("2023-09-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-09-01"))
    }
    
    if(!(as.Date("2023-10-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-10-01"))
    }
    
    if(!(as.Date("2023-11-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-11-01"))
    }
    
    if(!(as.Date("2023-12-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-12-01"))
    }
    
    if(!(as.Date("2024-01-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2024-01-01"))
    }
    
    if(!(as.Date("2024-02-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2024-02-01"))
    }
    
    if(!(as.Date("2024-03-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2024-03-01"))
    }
  }
  

  #add column for expected delivery
  data_to_print <- data %>%
    mutate(expected_delivery = seq(mean_annual_contracted_UDA_UOAs/12, mean_annual_contracted_UDA_UOAs, mean_annual_contracted_UDA_UOAs/12)) %>%
    select(month, YTD_delivery, expected_delivery) 
  
  #pivot for plotting
  data <- data_to_print %>%
    pivot_longer(cols = c("YTD_delivery", "expected_delivery"), names_to = "measure", values_to = "value")
  
  
  if(plotChart == TRUE){
    #plot code
    p <- ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month,
                    y = value/1000000,
                    colour = measure,
                    linetype = measure),
                size = 1) +
      geom_point(aes(x = month,
                     y = value/1000000,
                     colour = measure#,
                     #shape = measure
                     )
      ) +
      geom_hline(yintercept = mean_annual_contracted_UDA_UOAs/1000000,
                 linetype = "dashed") +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      # scale_y_continuous(limits = c(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5),
      #                    breaks = seq(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5, 10)) +
      labs(title = title,
           x = "Month",
           y = ylab,
           subtitle = subtitle,
           caption = captionTitle,
           colour = "",
           linetype = ""#,
           #shape = ""
           ) +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
      )) +
      annotate(geom = "text",
               x = as.Date("2023-05-01") + lubridate::weeks(2),
               y = mean_annual_contracted_UDA_UOAs/1000000 - mean_annual_contracted_UDA_UOAs/16000000,
               label = "Total annual contracted UDAs",
               size = 3) +
      scale_colour_manual(values = c("grey", lineCol), labels = c("Expected YTD delivery to meet contracted \nUDAs if delivery were equal across the financial year",
                                                                  "Actual YTD delivery")) +
      scale_linetype_manual(values = c("dashed", "solid"), labels = c("Expected YTD delivery to meet contracted UDAs \nif delivery were equal across the financial year",
                                                                  "Actual YTD delivery"),
                            guide = "none") +
      # scale_shape_manual(values = c("4", "1"), labels = c("Expected YTD delivery to meet contracted UDAs \nif delivery were equal across the year",
      #                                                                 "Actual YTD delivery")) +
      theme(legend.position="bottom")
    
    
    p
    
  }else{
    
    data_to_print <- data_to_print %>%
      rename(Month = month,
             `YTD delivery` = YTD_delivery,
             `Required YTD delivery in order to meet contracted UDAs` = expected_delivery)
  }
}