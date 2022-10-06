################################################################################
plot_UDA_UOA_delivery_all_regions <- function(data = UDA_scheduled_data, 
                                              calendar_data = UDA_calendar_data,
                                              historic_data = historical_UDA_scheduled_data,
                                              UDAorUOA = "UDA",
                                              level = "National",
                                              region_STP_name = NULL,
                                              remove_prototypes = TRUE, 
                                              plotChart = TRUE, 
                                              all_regions_and_STPs = FALSE,
                                              include_historic = TRUE){
  
  data <- data %>%
    mutate(month = as.Date(month))
  
  calendar_data <- calendar_data %>%
    mutate(month = as.Date(month))
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))
  
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
    
  }
  #get data into the right format
  data <- get_delivery_data(data, remove_prototypes, UDAorUOA = UDAorUOA, all_regions_and_STPs = TRUE)
  data <- data %>%
    group_by(month, region_name) %>%
    summarise(annual_contracted_UDA_UOA = sum(annual_contracted_UDA_UOA, na.rm = TRUE),
              scaled_monthly_UDA_UOAs_delivered = sum(scaled_monthly_UDA_UOAs_delivered, na.rm = TRUE)) %>%
    mutate(perc_UDA_UOA_delivered = scaled_monthly_UDA_UOAs_delivered * 100 / annual_contracted_UDA_UOA) %>%
    filter(!is.na(region_name))
  
  title <- "Scheduled monthly percentage of usual annual contracted UDAs \nsubmitted across all contracts* scaled up to 12 months**"
  ylab <- "% of contracted UDAs submitted"
  captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
  lineCol <- "coral"
  lineCol <- "#CC79A7"
  septemberTarget <- 60
  decemberTarget <- 65
  marchTarget <- 85
  juneTarget <- 95
  
  if(plotChart == TRUE){
    #plot code
    p <- ggplot(data) +
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_line(aes(x = month, 
                    y = perc_UDA_UOA_delivered, 
                    colour = region_name), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = perc_UDA_UOA_delivered, 
                     colour = region_name)
      ) +
      
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      scale_y_continuous(limits = c(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5),
                         breaks = seq(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5, 10)) +
      labs(title = title, 
           x = "Month",
           y = ylab, 
           caption = captionTitle,
           colour = "Region") +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
      ))
    
    p
    
  }else{
    
    data
  }
  
}