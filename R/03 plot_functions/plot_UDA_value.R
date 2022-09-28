################################################################################
plot_UDA_value <- function(data = UDA_scheduled_data,
                           historical_data = historical_UDA_scheduled_data,
                           payments_data = payments_to_dentists,
                           UDA_value_data = UDA_values,
                           protos = prototype_contracts$prototype_contract_number,
                           level = "National",
                           region_STP_name = NULL,
                           all_regions_and_STPs = FALSE,
                           asIndex = FALSE,
                           plotChart = TRUE){
  
  payments_data <- payments_data %>%
    select(year, Contract, Total_Contracted_UDA, Baseline_Contract) %>%
    mutate(Baseline_Contract = as.numeric(Baseline_Contract),
           Total_Contracted_UDA = as.numeric(Total_Contracted_UDA)) %>%
    mutate(UDA_value = Baseline_Contract/Total_Contracted_UDA) %>%
    filter(is.finite(UDA_value)) %>%
    mutate(contract_number = as.numeric(str_replace(Contract, "/", ""))) %>%
    select(year, contract_number, UDA_value)
  
  UDA_value_data_2021_22 <- UDA_value_data %>%
    mutate(year = "2021/22") %>%
    select(contract_number, 
           UDA_value = pounds_per_UDA,
           year)
  
  payments_data <- bind_rows(payments_data, UDA_value_data_2021_22)
  
  STp_region_lookup <- historical_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  data <- data %>%
    left_join(STp_region_lookup, by = "commissioner_name") %>%
    bind_rows(historical_data) %>%
    mutate(year = case_when(month >= as.Date("2021-04-01") ~ "2021/22",
                            TRUE ~ year)) %>%
    left_join(payments_data, by = c("contract_number", "year")) %>%
    mutate(UDA_value = case_when(is.na(UDA_value) ~ 28,
                                 TRUE ~ UDA_value)) %>%
    mutate(financial_delivery = UDA_delivered * UDA_value) %>%
    filter(!(contract_number %in% protos) & annual_contracted_UDA > 100)
  
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
    summarise(financial_delivery = sum(financial_delivery, na.rm = TRUE)) %>%
    mutate(financial_delivery_million_pounds = financial_delivery / 1000000)
  
  #set everything as index of 2018
  if(asIndex == TRUE){
    
    data <- data %>%
      mutate(month_number = substr(month, 6, 7),
             year_number = substr(month, 1, 4))
    
    data2018 <- data %>%
      filter(year_number == "2018") %>%
      select(month_number,
             financial_delivery_million_pounds_2018 = financial_delivery_million_pounds)
    
    data <- data %>%
      left_join(data2018, by = c("month_number")) %>%
      mutate(financial_delivery_million_pounds = financial_delivery_million_pounds * 100 / financial_delivery_million_pounds_2018)
    
    title <- "Financial value of UDAs delivered \nas a percentage of corresponding month of 2018 delivery"
    ylab <- "Percentage of 2018 financial value \nof UDAs delivered (%)"
    labelEnd <- "%"
    labelStart <- ""
    
  }else{
    title <- "Financial value of UDAs delivered  (£ Million)"
    ylab <- "Financial value of UDAs delivered \n (£ Million)"
    labelEnd <- " Million"
    labelStart <- "£"
  }
  
  if(plotChart == TRUE){
    ggplot() +
      geom_line(data = data,
                mapping = aes(x = as.Date(month),
                              y = financial_delivery_million_pounds),
                colour = "steelblue") +
      geom_point(data = data,
                 mapping = aes(x = as.Date(month),
                               y = financial_delivery_million_pounds),
                 colour = "steelblue") +
      geom_text(data = filter(data, month == max(data$month)),
                mapping = aes(x = as.Date(month),
                              y = financial_delivery_million_pounds + 10,
                              label = paste0(labelStart, round(financial_delivery_million_pounds), labelEnd)),
                size = 3) +
      theme_bw() +
      scale_x_date(date_breaks = "3 month", 
                   date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = title,
           subtitle = subtitle,
           x = "Month",
           y = ylab,
           caption = "UDA values have be calculated by dividing the baseline contract by the annual contracted UDAs for each contract. \nWhere this data was not availabale, £28 per UDA was used in the calculation. \nIt has been assumed that in 2022/23 the UDA value for each contract will be the same as in 2021/22. \nThis graph excludes prototype contracts and contracts with annual contracted UDAs < 100.")
    
  }else{
    data
  }
  
}