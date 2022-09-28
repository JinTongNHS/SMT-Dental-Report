################################################################################
plot_delivery_vs_contract_size_scatter_corporate <- function(data = UDA_scheduled_data,
                                                             calendar_data = UDA_calendar_data,
                                                             demographics_data = contract_demographics,
                                                             remove_prototypes = T,
                                                             plot_month = NULL,
                                                             UDAorUOA = "UDA",
                                                             level = "National",
                                                             region_STP_name = NULL,
                                                             get_num_above = NULL){
  if(is.null(plot_month)){
    #use latest month in data
    plot_month <- max(data$month)
  }
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))
  
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
  
  
  #join in demographics data
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data, by = "contract_number")
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 100)
  }
  
  #scale up April by 18
  scaleFactor <- if_else(plot_month == as.Date("2022-04-01"), 18, 12)
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * scaleFactor / annual_contracted_UDA) 
  
  
  data_to_plot <- data %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Corporate.Status)) %>%
    arrange(Corporate.Status)
  
  
  if(is.null(get_num_above)){
    p <- ggplot(data_to_plot, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
      geom_point(aes(colour = Corporate.Status)) +
      theme_bw() +
      geom_hline(yintercept = 0.95,
                 colour = "orangered4",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA) - 8000,
               y = 0.87,
               label = "95% Q1 threshold",
               size = 3,
               colour = "orangered4") +
      geom_hline(yintercept = 1,
                 colour = "grey40",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA),
               y = 1.08,
               label = "100%",
               size = 3,
               colour = "grey40") +
      scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
      scale_x_continuous(breaks = seq(0, 175000, 20000),
                         limits = c(0, max(data_to_plot$annual_contracted_UDA) + 5000),
                         #labels = scales::percent_format(accuracy = 1)
                         labels=function(x) format(x, big.mark = ",", scientific = FALSE)
      ) +
      scale_y_continuous(breaks = seq(0, 3, 0.5),
                         limits = c(0, 3),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(title = "UDA contract size Vs UDA delivery scaled up 12** months",
           subtitle = paste(format(plot_month, "%B %Y"), "scheduled delivery -", subtitle),
           x = "Annual contracted UDAs",
           y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
           caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         Also excluding contracts with delivery > 300% for plot purposes.
           **April is scaled up by 18 due to short schedule period for April",
           colour = "Corporate status"
      )
    
    p
  }else{
    data <- data %>%
      filter(UDA_delivery >= get_num_above) 
    
    num_contracts_above_threshold <- round(nrow(data))
    mean_contract_size_above_threshold <- round(mean(data$annual_contracted_UDA, na.rm = T))
    
    list(num_contracts_above_threshold = num_contracts_above_threshold,
         mean_contract_size_above_threshold = mean_contract_size_above_threshold)
  }
  
  
}