################################################################################
plot_closures_from_contract_end_date <- function(data = UDA_scheduled_data,
                                                 historical_data = historical_UDA_scheduled_data,
                                                 level = "National",
                                                 region_STP_name = NULL,
                                                 all_regions_and_STPs = FALSE,
                                                 plotChart = TRUE){
  
  data <- data %>%
    bind_rows(historical_data) %>%
    filter(!is.na(contract_end_date)) 
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
    y_breaks <- seq(0, 5000, 200)
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
    y_breaks <- seq(0, 1000, 100)
  }else{
    subtitle <- "England"
    y_breaks <- seq(0, 28000, 2000)
  }
  
  data <- data %>%
    mutate(contract_end_date_month = lubridate::floor_date(contract_end_date, unit = "months")) %>%
    group_by(contract_end_date_month) %>%
    count() %>%
    mutate(time_period = case_when(contract_end_date_month <= max(data$month) ~ "Historical",
                                   TRUE ~ "Future"))
  
  if(plotChart == TRUE){
    
    ggplot(data) +
      geom_line(aes(x = as.Date(contract_end_date_month),
                    y = n,
                    linetype = time_period)) +
      theme_bw() +
      scale_linetype_manual(values = c("dashed", "solid")) +
      scale_x_date(date_breaks = "6 month", 
                   date_labels = "%b-%y") +
      scale_y_continuous(breaks = y_breaks) +
      labs(title = "Number of contract closures based on contract end dates",
           subtitle = subtitle,
           x = "Month",
           y = "Number of contracts with closure date \nin given month",
           linetype = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  }else{
    data
  }
  
}
