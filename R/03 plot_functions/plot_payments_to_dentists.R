################################################################################
plot_payments_to_dentists <- function(data = payments_to_dentists,
                                      level = "National",
                                      region_STP_name = NULL,
                                      all_regions_and_STPs = FALSE,
                                      plotChart = TRUE){
  
  data <- data %>%
    rename(region_name = Region_Name)
  
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
    filter(!is.na(Contract)) %>%
    mutate(Net_Payment_to_Dental_Contract = as.numeric(Net_Payment_to_Dental_Contract)) %>%
    #mutate(contract_number = str_replace(Contract, "/", "")) %>%
    group_by(year) %>%
    summarise(total_net_payments = sum(Net_Payment_to_Dental_Contract, na.rm = TRUE)) %>%
    ungroup()
  
  if(plotChart == TRUE){
    
    ggplot(data, group = 1) +
      geom_col(aes(x = year,
                   y = total_net_payments),
               fill = "steelblue") +
      # geom_point(aes(x = year,
      #                y = total_net_payments),
      #            colour = "steelblue") +
      theme_bw() +
      scale_y_continuous(breaks = scales::breaks_pretty(),
                         labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(title = "Total net payments to dentists",
           subtitle = subtitle,
           x = "Financial year",
           y = "Net payment (\u00A3)")
  }else{
    data
  }
  
}