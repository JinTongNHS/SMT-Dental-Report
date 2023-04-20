################################################################################
plot_UDA_UOA_delivery <- function(data = UDA_scheduled_data, 
                                  historic_data = historical_UDA_scheduled_data,
                                  historic_data_UOA = historical_UOA_scheduled_data,
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  remove_prototypes = TRUE, 
                                  plotChart = TRUE, 
                                  all_regions_and_STPs = FALSE,
                                  include_historic = TRUE){
  
  data <- data %>%
    mutate(month = as.Date(month))

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
             UDA_delivered) #%>%
    #filter(month >= as.Date("2019-04-01"))
    
    data <- bind_rows(data, historic_data)
    
  }else if(include_historic == TRUE & UDAorUOA == "UOA"){
    
    data <- data %>%
      select(month, contract_number, commissioner_name, region_name,
             annual_contracted_UOA, UOA_delivered)
    
    historic_data_UOA <- historic_data_UOA %>%
      mutate(month = as.Date(month)) %>%
      select(month, contract_number, commissioner_name, region_name,
             annual_contracted_UOA,
             UOA_delivered) #%>%
    #filter(month >= as.Date("2019-04-01"))
    
    data <- bind_rows(data, historic_data_UOA)
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
  
  if(UDAorUOA == "UDA"){
    #get data into the right format
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UDA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "Scheduled monthly percentage of usual annual contracted UDAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UDAs submitted"
    captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April -
                    21st April therefore the April data has been scaled up by 18 instead of 12."

    lineCol <- "#CC79A7"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
    juneTarget <- 95
  }else{
    #get data into the right format
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UOA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "Scheduled monthly percentage of usual annual contracted UOAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UOAs submitted"
    captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April -
                    21st April therefore the April data has been scaled up by 18 instead of 12."
    lineCol <- "steelblue"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    juneTarget <- 100
  }
  
  if(plotChart == TRUE){
    #plot code
    p <- ggplot(data) +
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_line(aes(x = month,
                    y = perc_UDA_UOA_delivered),
                colour = lineCol,
                size = 1) +
      geom_point(aes(x = month,
                     y = perc_UDA_UOA_delivered),
                 colour = lineCol
      ) 
    
    if(include_historic == FALSE){
      
      p <- p +
      geom_segment(aes(x = as.Date("2021-04-01"), y = septemberTarget, xend = as.Date("2021-09-01"), yend = septemberTarget),
                   colour = "#0072B2",
                   linetype = "dashed") +
      geom_segment(aes(x = as.Date("2021-10-01"), y = decemberTarget, xend = as.Date("2021-12-01"), yend = decemberTarget),
                   colour = "#0072B2",
                   linetype = "dashed") +
      geom_segment(aes(x = as.Date("2022-01-01"), y = marchTarget, xend = as.Date("2022-03-01"), yend = marchTarget),
                   colour = "#0072B2",
                   linetype = "dashed") +
      geom_segment(aes(x = as.Date("2022-04-01"), y = juneTarget, xend = as.Date("2022-06-01"), yend = juneTarget),
                   colour = "#0072B2",
                   linetype = "dashed") +

      annotate(geom = "text",
               x = as.Date("2021-04-01") + lubridate::weeks(2),
               y = septemberTarget - 5,
               label = "H1 threshold",
               size = 3,
               colour = "#0072B2") +
      annotate(geom = "text",
               x = as.Date("2021-10-01") + lubridate::weeks(2),
               y = decemberTarget - 5,
               label = "Q3 threshold",
               size = 3,
               colour = "#0072B2") +
      annotate(geom = "text",
               x = as.Date("2022-01-01") + lubridate::weeks(2),
               y = marchTarget - 5,
               label = "Q4 threshold",
               size = 3,
               colour = "#0072B2") +
      annotate(geom = "text",
               x = as.Date("2022-04-01") + lubridate::weeks(2),
               y = juneTarget - 5,
               label = "Q1 threshold",
               size = 3,
               colour = "#0072B2") +
        annotate(geom = "text",
                 x = data$month,
                 y = data$perc_UDA_UOA_delivered + 5,
                 label = paste0(data$perc_UDA_UOA_delivered, "%"),
                 size = 3) 
    }else{
      
      p <- p +
        annotate(geom = "text",
                 x = data$month[nrow(data)],
                 y = data$perc_UDA_UOA_delivered[nrow(data)] - 3,
                 label = paste0(data$perc_UDA_UOA_delivered[nrow(data)], "%"),
                 size = 3) 
    }
      
    p <- p +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
      scale_y_continuous(limits = c(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5),
                         breaks = seq(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5, 10)) +
      labs(title = title,
           x = "Month",
           y = ylab,
           subtitle = subtitle,
           caption = captionTitle) +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
      ))
    
    p
    
  }else{
    
    if(UDAorUOA == "UDA"){
      new_col_names <- c(`Schedule month` = "month",
                         `Region Name` = "region_name",
                         `Commissioner Name` = "commissioner_name",
                         `Monthly UDAs delivered` = "monthly_UDA_UOAs_delivered",
                         `Annual contracted UDAs` = "annual_contracted_UDA_UOA",
                         `Scaled monthly UDAs delivered` = "scaled_monthly_UDA_UOAs_delivered",
                         `Scaled monthly percentage of contracted UDAs delivered` = "perc_UDA_UOA_delivered")
    }else{
      new_col_names <- c(`Schedule month` = "month",
                         `Region Name` = "region_name",
                         `Commissioner Name` = "commissioner_name",
                         `Monthly UOAs delivered` = "monthly_UDA_UOAs_delivered",
                         `Annual contracted UOAs` = "annual_contracted_UDA_UOA",
                         `Scaled monthly UOAs delivered` = "scaled_monthly_UDA_UOAs_delivered",
                         `Scaled monthly percentage of contracted UOAs delivered` = "perc_UDA_UOA_delivered")
    }
    
    
    data <- data %>%
      rename(any_of(new_col_names))
  }
}