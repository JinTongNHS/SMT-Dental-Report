################################################################################
##pound symbol is a special character in HTML so must use \u00A3 instead
plot_cumulative_UDA_value <- function(data = UDA_scheduled_data,
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
    mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01")  ~ "2022/23 YTD",
                            TRUE ~ year)) %>%
    left_join(payments_data, by = c("contract_number", "year")) %>%
    mutate(UDA_value = case_when(is.na(UDA_value) ~ 28,
                                 TRUE ~ UDA_value)) %>%
    mutate(financial_delivery = UDA_delivered * UDA_value) %>%
    filter(!(contract_number %in% protos & month < as.Date("2022-04-01"))) %>%
    filter(annual_contracted_UDA > 100)
  
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
  
  #get cumulative data for this financial year
  data_this_year <- data %>%
    filter(month >= as.Date("2022-04-01")) %>%
    group_by(month) %>%
    summarise(financial_delivery = sum(financial_delivery, na.rm = TRUE)) %>%
    mutate(financial_delivery_million_pounds = financial_delivery / 1000000) %>%
    mutate(cum_financial_delivery_million_pounds = cumsum(financial_delivery_million_pounds))
  
  #get mean over 2017/18 - 2019/20
  prev_years <- data %>%
    filter(month < as.Date("2022-04-01") & month >= as.Date("2017-04-01")) %>%
    group_by(year) %>%
    summarise(financial_delivery = sum(financial_delivery, na.rm = TRUE)) %>%
    mutate(financial_delivery_million_pounds = financial_delivery / 1000000) %>%
    ungroup() %>%
    summarise(financial_delivery_million_pounds_2017_2020_mean = mean(financial_delivery_million_pounds))
  
  #join in prev year
  data_this_year <- data_this_year %>%
    mutate(financial_delivery_million_pounds_2017_2020_mean = prev_years$financial_delivery_million_pounds_2017_2020_mean[1]) 
  
  data <- data_this_year %>%
    pivot_longer(cols = c("cum_financial_delivery_million_pounds", "financial_delivery_million_pounds_2017_2020_mean"),
                 names_to = "line") %>%
    mutate(month = as.Date(month))
  

  title <- "Cumulative financial value of UDAs delivered  YTD 2022/23 (Million GBP)"
  ylab <- "Financial value of UDAs delivered YTD\n (\u00A3 Million)"
  labelEnd <- " Million"
  labelStart <- "\u00A3"
  
  if(plotChart == TRUE){
    ggplot() +
      geom_line(data = data,
                mapping = aes(x = month,
                              y = value,
                              colour = line),) +
      geom_point(data = filter(data, line == "cum_financial_delivery_million_pounds"),
                mapping = aes(x = month,
                              y = value,
                              colour = line)) +
      geom_text(data = filter(data, month == max(data$month)),
                mapping = aes(x = month,
                              y = value + max(data$value)/25,
                              label = paste0(labelStart, round(value), labelEnd)),
                size = 3) +
      geom_text(data = filter(data, month == max(data$month)),
                mapping = aes(x = month,
                              y = value + max(data$value)/25,
                              label = paste0(labelStart, round(value), labelEnd)),
                size = 3) +
      # annotate(geom = "text",
      #          x = data$month[3],
      #          y = data$financial_delivery_million_pounds_2017_2020_mean - max(data$value)/25,
      #          label = paste0("Mean annual financial delivery over 2017-18 to 2019-20. \u00A3", round(data$financial_delivery_million_pounds_2017_2020_mean[1]), " million"),
      #          size = 3,
      #          colour = "red") +
      scale_colour_manual(values = c("steelblue", "red"), labels = c("YTD cumulative financial delivery",
                                                                     "Mean annual financial delivery \nover 2017/18 to 2019/20")) +
      theme_bw() +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position="bottom") +
      labs(title = title,
           subtitle = subtitle,
           x = "Month",
           y = ylab,
           colour = "",
           caption = "UDA values have be calculated by dividing the baseline contract by the annual contracted UDAs for each contract. \nWhere this data was not availabale, \u00A328 per UDA was used in the calculation. \nIt has been assumed that in 2022/23 the UDA value for each contract will be the same as in 2021/22. \nThis graph excludes contracts with annual contracted UDAs < 100. Prototype contracts are excluded up until April 2022.")
    
  }else{
    data
  }
  
}