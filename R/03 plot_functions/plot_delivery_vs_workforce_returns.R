###############################################################################
plot_delivery_vs_workforce_returns <- function(data = UDA_scheduled_data,
                                               remove_prototypes = T,
                                               plot_month = NULL,
                                               level = "National",
                                               region_STP_name = NULL,
                                               workforce_data = dental_workforce_returns){
  if(is.null(plot_month)){
    #use latest month in data
    plot_month <- max(data$month)
  }

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
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  data <- data %>%
    filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
    filter(annual_contracted_UDA > 100)
  
  #scale up April by 18
  scaleFactor <- if_else(plot_month == as.Date("2022-04-01"), 18, 12)
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered) %>%
    mutate(UDA_delivery = UDA_delivered * scaleFactor / annual_contracted_UDA) 
  
  #get workforce returns data
  workforce_data <- clean_workforce_returns(workforce_data)
  workforce_data <- workforce_data  %>%
    mutate(perc_workforce_returns_submitted = total_workforce_returns_submitted / total_workforce_returns_due) %>%
    select(contract_number, perc_workforce_returns_submitted)
  
  data <- data %>%
    left_join(workforce_data, by = "contract_number")
  
  
  p <- ggplot(data) +
    geom_point(aes(x = perc_workforce_returns_submitted, y = UDA_delivery)) +
    theme_bw() +
    
    # scale_x_continuous(breaks = seq(0, 175000, 20000),
    #                    limits = c(0, max(data_to_plot$annual_contracted_UDA) + 5000),
    #                    #labels = scales::percent_format(accuracy = 1)
    #                    labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    # ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Percentage of expected workforce returns submitted \nVs UDA delivery scaled up 12** months",
         subtitle = paste(format(plot_month, "%B %Y"), "scheduled delivery -", subtitle),
         x = "Percentage of expected workforce returns submitted",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         Also excluding contracts with delivery > 300% for plot purposes.
           **April is scaled up by 18 due to short schedule period for April"
    )
  
  p
  
}