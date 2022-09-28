################################################################################
plot_UDAs_commissioned <- function(data = UDA_scheduled_data,
                                   historical_data = historical_UDA_scheduled_data,
                                   level = "National",
                                   region_STP_name = NULL,
                                   all_regions_and_STPs = FALSE,
                                   plotChart = TRUE,
                                   getChange = FALSE){
  
  STp_region_lookup <- historical_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  data <- data %>%
    left_join(STp_region_lookup, by = "commissioner_name") %>%
    bind_rows(historical_data) #%>%
  #filter(month < as.Date("2020-01-01") | month > as.Date("2020-12-01"))
  
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
    summarise(n = sum(annual_contracted_UDA, na.rm = TRUE)) %>%
    mutate(month_number = substr(month, 6, 7)) %>%
    mutate(point_col = case_when(month_number == substr(Sys.Date() - lubridate::weeks(4), 6, 7) ~ "this month",
                                 TRUE ~ "other months"))
  
  data_labels <- data %>%
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
                              label = format(n, big.mark = ",", scientific = FALSE)),
                colour = "red",
                size = 3) +
      theme_bw() +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y",
                   limits = c(as.Date("2016-01-01"), max(data$month) + lubridate::weeks(12))) +
      scale_y_continuous(breaks = scales::breaks_pretty(),
                         labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      scale_colour_manual(values = c("steelblue", "red"), labels = c("Other months", point_label)) +
      labs(title = "Total annual contracted UDAs across all contracts",
           subtitle = subtitle,
           x = "Month",
           y = "Contracted UDAs",
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
    
    change_in_num_contracted_UDA <- data_2022$n[1] - data_2018$n[1]
    perc_change_in_num_contracted_UDA <- change_in_num_contracted_UDA * 100 / data_2018$n[1]
    round(perc_change_in_num_contracted_UDA, 2)
    
  }else{
    data
  }
  
}