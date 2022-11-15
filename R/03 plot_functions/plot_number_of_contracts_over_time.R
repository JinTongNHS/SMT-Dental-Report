################################################################################
plot_number_of_contracts_over_time <- function(data = UDA_scheduled_data,
                                               historical_data = historical_UDA_scheduled_data,
                                               level = "National",
                                               region_STP_name = NULL,
                                               all_regions_and_STPs = FALSE,
                                               plotChart = TRUE,
                                               getChange = FALSE){

  data <- data %>%
    bind_rows(historical_data) 
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  data <- data %>%
    group_by(month) %>%
    count() %>%
    mutate(month_number = substr(month, 6, 7)) %>%
    mutate(point_col = case_when(month_number == substr(Sys.Date() - lubridate::weeks(4), 6, 7) ~ "this month",
                                 TRUE ~ "other months"))
  
  data_labels <- data %>%
    # mutate(month_number = substr(month, 6, 7)) %>%
    # filter(month_number == substr(Sys.Date() - lubridate::weeks(4), 6, 7))
    filter(point_col == "this month")
  
  point_label <- format(Sys.Date() - lubridate::weeks(4), "%B")
  
  if(plotChart == TRUE){
    
    ggplot() +
      geom_line(data = data,
                mapping = aes(x = as.Date(month),
                              y = n),
                colour = "steelblue",
                size = 1) +
      geom_point(data = data,
                 mapping = aes(x = as.Date(month),
                               y = n,
                               colour = point_col)) +
      geom_text(data = data_labels,
                mapping = aes(x = as.Date(month),
                              y = n - max(data$n)/30,
                              label = n),
                colour = "red",
                size = 3) +
      theme_bw() +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      scale_colour_manual(values = c("steelblue", "red"), labels = c("Other months", point_label)) +
      labs(title = "Total number of dental contracts that have submitted FP17 data",
           subtitle = subtitle,
           x = "Month",
           y = "Number of contracts",
           caption = "This includes all contracts in BSA data including prototypes and very small contracts.",
           colour = "") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  }else if(plotChart == FALSE & getChange == TRUE){
    data_2022 <- data %>%
      mutate(year_number = substr(month, 1, 4)) %>%
      filter(point_col == "this month" & year_number == "2022")
    
    data_2018 <- data %>%
      mutate(year_number = substr(month, 1, 4)) %>%
      filter(point_col == "this month" & year_number == "2018")
    
    change_in_num_contracts <- data_2022$n[1] - data_2018$n[1]
    perc_change_in_num_contracts <- change_in_num_contracts * 100 / data_2018$n[1]
    round(perc_change_in_num_contracts, 2)
    
  }else{
    data
  }
  
}
