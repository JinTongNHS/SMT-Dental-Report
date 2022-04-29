################################################################################
#function to create graph on slide 8
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_banded_CoT_proportions <- function(data = UDA_scheduled_data, 
                            calendar_data = UDA_calendar_data,
                            #existing_data = slide8_banded_CoT_historic,
                            historic_data = historical_UDA_scheduled_data, 
                            level = "National",
                            region_STP_name = NULL){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
  #toggle subtitle
  if(level == "Regional"){
    #filter for region or STP
    data <- data %>% 
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    #filter for region or STP
    data <- data %>% 
      filter(commissioner_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(commissioner_name == region_STP_name )
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #get data into the right format
  data <- get_into_slide8_format(data, historic_data = historic_data, remove_prototypes = F)
  data <- reshape2::melt(data, id.vars = "month")
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    rename(band = variable, CoTs = value) %>%
    group_by(month) %>%
    mutate(total_CoTs = sum(CoTs)) %>%
    ungroup() %>%
    mutate(proportion = CoTs * 100 / total_CoTs)
  



  #plot code
  ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month,
                  y = proportion,
                  colour = band),
              size = 1) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    scale_colour_manual(labels = c("Band 1", "Band 2", "Band 3", "Other", "Urgent"),
                        values = c("coral3",
                                   "orange",
                                   "yellow3",
                                   "green",
                                   "blue")
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    labs(title = "Percentage of total courses of treatment for each band",
         x = "Month",
         y = "Percentage of total FP17* forms submitted",
         colour = "Band",
         subtitle = subtitle,
         caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs,
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2
         UDAs and an 'other' FP17 = 0.6 UDAs.")
  #data
}



################################################################################
#function to create graph on slide 8
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_banded_CoT_proportions_urgent <- function(data = UDA_scheduled_data, 
                                        calendar_data = UDA_calendar_data,
                                        #existing_data = slide8_banded_CoT_historic,
                                        historic_data = historical_UDA_scheduled_data, 
                                        level = "National",
                                        region_STP_name = NULL){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
  #toggle subtitle
  if(level == "Regional"){
    #filter for region or STP
    data <- data %>% 
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    #filter for region or STP
    data <- data %>% 
      filter(commissioner_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(commissioner_name == region_STP_name )
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  # #get data into the right format
  # data <- get_into_slide8_format(data, historic_data = historic_data, remove_prototypes = F)
  # data <- reshape2::melt(data, id.vars = "month")
  # data <- data %>%
  #   mutate(month = as.Date(month)) %>%
  #   rename(band = variable, CoTs = value) %>%
  #   group_by(month) %>%
  #   mutate(total_CoTs = sum(CoTs)) %>%
  #   ungroup() %>%
  #   mutate(proportion = CoTs * 100 / total_CoTs)
  
  #get data into the right format
  # data <- data %>%
  #   filter(contract_number %in% under_65s$contract_number) 
  
  data <- get_into_slide8_format(data, historic_data = historic_data, remove_prototypes = F)
  data <- reshape2::melt(data, id.vars = "month")
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    rename(band = variable, CoTs = value) %>%
    mutate(urgent_level = if_else(band == "urgent", "urgent", "non-urgent")) %>%
    group_by(month,urgent_level) %>%
    summarise(CoTs = sum(CoTs)) %>%
    ungroup() %>%
    group_by(month) %>%
    mutate(total_CoTs = sum(CoTs)) %>%
    ungroup() %>%
    mutate(proportion = CoTs * 100 / total_CoTs)#%>%
    #filter(month >= as.Date("2021-04-01"))
  
  
  #plot code
  ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month,
                  y = proportion,
                  colour = urgent_level),
              size = 1) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    # scale_colour_manual(labels = c("Band 1", "Band 2", "Band 3", "Other", "Urgent"),
    #                     values = c("coral3",
    #                                "orange",
    #                                "yellow3",
    #                                "green",
    #                                "blue")
    # ) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    labs(title = "Percentage of total courses of treatment for each band \nfor all contracts",
         # title = "Percentage of total courses of treatment for each band \nfor contracts delivering under 65% of their Q3 contracted value",
         x = "Month",
         y = "Percentage of total FP17* forms submitted",
         colour = "Band",
         subtitle = subtitle,
         caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs,
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2
         UDAs and an 'other' FP17 = 0.6 UDAs.")+
    annotate(geom = "text", 
             x = data$month, 
             y = data$proportion + 2, 
             label = paste0(round(data$proportion, 1), "%"), 
             size = 3) 
  #data
}





################################################################################
get_Q3_num_contracts_on_target_scheduled <- function(data = UDA_calendar_data, 
                                           remove_prototypes = T,
                                           scheduled_data = UDA_scheduled_data,
                                           UDAorUOA = "UDA",
                                           level = "National",
                                           region_STP_name = NULL){
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }
  
  if(UDAorUOA == "UDA"){
    #get contracted UDAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.6 * annual_contracted_UDA/4,
                                                   month > as.Date("2021-10-01") ~ 0.65 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA, UOA_financial_half_target) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month > as.Date("2021-10-01") ~ 0.85 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- scheduled_data %>%
    #left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2021-10-01"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2021-10-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2021-11-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2021-12-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                #mean_UDA_target = mean(UDA_financial_half_target),
                YTD_UDA_delivered = sum(UDA_delivered)) %>%
      mutate(mean_Q3_UDA_target = mean_annual_contracted_UDA * 0.65 / 4) %>%
      count(YTD_UDA_delivered >= (mean_Q3_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                #mean_Q3_UOA_target = mean(UOA_financial_half_target),
                YTD_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q3_UOA_target = mean_annual_contracted_UOA * 0.85 / 4) %>%
      count(YTD_UOA_delivered >= (mean_Q3_UOA_target) * month_factor / 3)
  }
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)
  
}