################################################################################
get_num_contracts_over_percent <- function(data = UDA_calendar_data, 
                                           remove_prototypes = T,
                                           scheduled_data = UDA_scheduled_data,
                                           UDAorUOA = "UDA",
                                           level = "National",
                                           region_STP_name = NULL,
                                           percent = 0.85,
                                           filterMonth = as.Date("2021-11-01")){
  
  # #filter for STP or region
  # if(level == "Regional"){
  #   data <- data %>% 
  #     filter(region_name == region_STP_name )
  # }else if(level == "STP"){
  #   data <- data %>% 
  #     filter(commissioner_name == region_STP_name)
  #   subtitle <- region_STP_name
  # }
  # 
  # if(UDAorUOA == "UDA"){
  #   #get contracted UDAs
  #   contracted_UDA_UOAs <- scheduled_data %>%
  #     select(month, contract_number, annual_contracted_UDA) %>%
  #     mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.6 * annual_contracted_UDA/4,
  #                                                  month > as.Date("2021-10-01") ~ 0.65 * annual_contracted_UDA/4))
  # }else{
  #   #get contracted UOAs
  #   contracted_UDA_UOAs <- scheduled_data %>%
  #     select(month, contract_number, annual_contracted_UOA, UOA_financial_half_target) %>%
  #     mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
  #                                                  month > as.Date("2021-10-01") ~ 0.85 * annual_contracted_UOA/4))
  # }
  # 
  # 
  # 
  #join in contracted UDA/UOAs from scheduled data
  scheduled_data <- scheduled_data %>%
    #left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month == filterMonth)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    scheduled_data <- scheduled_data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    scheduled_data <- scheduled_data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  

    #count number of contracts meeting target
    data <- scheduled_data %>%
      #group_by(contract_number) %>%
      # summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
      #           YTD_UDA_delivered = sum(UDA_total)) %>%
      # mutate(mean_Q3_UDA_target = mean_annual_contracted_UDA * 0.65 / 4) %>%
      count(UDA_delivered * 12 >= annual_contracted_UDA * percent)

  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)
  
  nrow(scheduled_data)
  
}












################################################################################
#function to plot the density of contracts based on performance 
plot_density <- function(calendar_data = UDA_calendar_data, 
                         scheduled_data = UDA_scheduled_data,
                         calOrSch = "cal",
                         subtitle = "November 2021 - Calendar data",
                         remove_prototypes = T,
                         filterDate = as.Date("2021-11-01")){
  
  if(calOrSch == "cal"){
    #join in contracted UDAs from scheduled data
    contracted_UDAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA)
    
    data <- calendar_data %>%
      left_join(contracted_UDAs, by = c("month", "contract_number")) %>%
      mutate(scaled_perc_UDA_delivered = UDA_total * 1200 / annual_contracted_UDA)
  }else{
    data <- scheduled_data %>%
      mutate(scaled_perc_UDA_delivered = UDA_delivered * 1200 / annual_contracted_UDA)
  }
  
  # data <- data %>%
  #   filter(month == filterDate)
  # 
  # #remove prototype contracts if specified
  # if(remove_prototypes){
  #   data <- data %>%
  #     filter(!(contract_number %in% prototype_contracts$prototype_contract_number)) %>%
  #     filter(annual_contracted_UDA > 100)
  # }
  # 
  # 
  # data <- data %>% 
  #   #filter(month == as.Date("2021-11-01")) %>%
  #   select(scaled_perc_UDA_delivered) #%>%
  #   #reshape2::melt() %>%
  #   #mutate(perf = perf * 100)
  # 
  # data_summary <- summary(data$scaled_perc_UDA_delivered)
  # 
  # 
  # ggplot(data,
  #        aes(x = scaled_perc_UDA_delivered)) +
  #   geom_density(size = 1,
  #                alpha = 0.1,
  #                color = "coral",
  #                fill = "coral") +
  #   #GDS mean
  #   geom_vline(xintercept = as.numeric(data_summary["Mean"]),
  #              size = 0.5,
  #              colour = "coral",
  #              linetype = "solid"
  #   ) +
  #   annotate(geom = "text",
  #            x = as.numeric(data_summary["Mean"] - 3),
  #            y = 0,
  #            label = paste0("mean"),
  #            size = 3,
  #            colour = "coral") +
  #   
  #   #GDS mean
  #   geom_vline(xintercept = 85,
  #              size = 0.5,
  #              colour = "coral",
  #              linetype = "dashed"
  #   ) +
  #   annotate(geom = "text",
  #            x = 85,
  #            y = 0,
  #            label = paste0("85%"),
  #            size = 3,
  #            colour = "coral") +
  #   
  #   #GDS mean
  #   geom_vline(xintercept = 100,
  #              size = 0.5,
  #              colour = "coral",
  #              linetype = "dashed"
  #   ) +
  #   annotate(geom = "text",
  #            x = 100,
  #            y = 0,
  #            label = paste0("100%"),
  #            size = 3,
  #            colour = "coral") +
  #   
  #   
  #   scale_x_continuous(limits = c(0,150),
  #                      breaks = seq(0,150, 10)) +
  #   labs(title = "Performance distribution for UDA delivery",
  #        x = "Delivery in given time period (%)",
  #        y = "Density of contracts",
  #        subtitle = subtitle,
  #        caption = "Prototypes and contracts with annual contracted UDAs < 100 excluded") +
  #   theme_bw()
  # 
  
  data
}



#analysis of proactices likely to hand their contracts back
hand_back <- function(data = UDA_calendar_data,
                      contractor_cats = contractor_categories){
  
  data <- data %>%
    left_join(contractor_cats) %>%
    filter(MY_category == "Cat 2 - closed in-year" |
             MY_category == "Cat 3 - closed in year" |
             MY_category == "Cat 3 - Contract End Date before 01/04/2020" |
             MY_category == "Cat 3 - Contract End Date before 01/04/2021" |
             MY_category == "Cat 3 - Contract End Date before 01/04/2022")
  
}


#hand back scatter
plot_handback_scatter <- function(data = UDA_scheduled_data_protos_removed){
  
  data <- data %>%
    filter(month == as.Date("2021-04-01")) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA,
           handback = if_else(contract_number %in% handbacks, "b - handback", "a - non-handback")) %>%
    arrange(handback)
  

  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(aes(colour = handback)) +
    theme_bw() +
    # scale_x_continuous(breaks = seq(0, 1.2, 0.1),
    #                    limits = c(0, 1.2),
    #                    labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(#breaks = seq(0, 1.6, 0.1),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-handbacks", "handbacks")) +
    labs(title = "UDA contract size Vs UDA delivery highlighting handback contracts",
         subtitle = "April 2021 delivery",
         x = "Contract size",
         y = "UDA delivery",
         #caption = "Wave 1-4 contracts included"
         colour = ""
    )
  
}



plot_regional_hanback_bars <- function(data = PCAR_Closed_Contracts_2021_22){
  
  data <- data %>%
    group_by(region_name) %>%
    count() %>%
    ungroup() %>%
    add_row(region_name = "London", n = 0)
    
  
  data$region_name <- factor(data$region_name,
                        levels = c("East of England",
                                   "North West",
                                   "South West",
                                   "South East",
                                   "North East and Yorkshire",
                                   "Midlands",
                                   "London"
                        ))

  # data <- data %>%
  #   mutate(num_handbacks = if_else(is.na(rebases), num_contracts_handed_back, num_contracts_handed_back - rebases)) %>%
  #   select(Region, num_handbacks, rebases) %>%
  #   reshape2::melt(id = "Region")

  ggplot(data) +
    geom_bar(aes(x = region_name, y = n),
             stat = "identity",
             fill = "steelblue") +
    #scale_fill_manual(values = c("steelblue", "steelblue2"), labels = c("hand-backs", "rebases")) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
    #geom_text(aes(x = Region, y = value), ) +
    labs(title = "Contract hand-backs in each region",
         y = "number of contracts",
         x = "Region"
         ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45
                                     , vjust = 1, hjust=1
                                     )) 
    

  
}


plot_regional_hanback_UDAs_bars <- function(data = handbacks_summary_data){
  
  data$Region <- factor(data$Region,
                        levels = c("East",
                                   "South East",
                                   "Midlands",
                                   "South West",
                                   "NE&Y",
                                   "North West",
                                   "London"
                        ))
  
  # data <- data %>%
  #   mutate(num_handbacks = if_else(is.na(rebases), num_contracts_handed_back, num_contracts_handed_back - rebases)) %>%
  #   select(Region, num_handbacks, rebases) %>%
  #   reshape2::melt(id = "Region")
  
  ggplot(data) +
    geom_bar(aes(x = Region, y =UDAs_handed_back), stat = "identity", fill = "seagreen") +
    #scale_fill_manual(values = c("steelblue", "steelblue2"), labels = c("hand-backs", "rebases")) +
    #scale_y_continuous(breaks = seq(0, 30, 5)) +
    #geom_text(aes(x = Region, y = value), ) +
    labs(title = "UDAs handed back in each region",
         y = "UDAs"#,
         #fill = ""
         ) +
    theme_bw()
  
}


plot_regional_hanback_value_bars <- function(data = handbacks_summary_data){
  
  data$Region <- factor(data$Region,
                        levels = c("East",
                                   "South East",
                                   "Midlands",
                                   "South West",
                                   "NE&Y",
                                   "North West",
                                   "London"
                        ))
  
  # data <- data %>%
  #   mutate(num_handbacks = if_else(is.na(rebases), num_contracts_handed_back, num_contracts_handed_back - rebases)) %>%
  #   select(Region, num_handbacks, rebases) %>%
  #   reshape2::melt(id = "Region")
  
  ggplot(data) +
    geom_bar(aes(x = Region, y =contract_value_handed_back/1000000), stat = "identity", fill = "coral") +
    #scale_fill_manual(values = c("steelblue", "steelblue2"), labels = c("hand-backs", "rebases")) +
    scale_y_continuous(breaks = seq(0, 3, 0.5)) +
    #geom_text(aes(x = Region, y = value), ) +
    labs(title = "Total contract value handed back in each region",
         y = "Million Pounds (£)"#,
         #fill = ""
    ) +
    theme_bw()
  
}
