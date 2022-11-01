get_DH_summary_data <- function(data = UDA_scheduled_data,
                                historic_data = historical_UDA_scheduled_data,
                                prototypes = prototype_contracts){
  
  #get lookup for region from ICB
  region_ICB_lookup <- historic_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  
  #Join in region column
  data <- data %>% 
    left_join(region_ICB_lookup, by = "commissioner_name") %>%
    bind_rows(historic_data) %>%
    filter(annual_contracted_UDA > 100) %>%
    filter(!(contract_number %in% prototypes$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
    mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01") ~ "2022/23",
                            TRUE ~ year)) %>%
    group_by(year, commissioner_name, contract_number) %>%
    summarise(annual_contracted_UDA = mean(annual_contracted_UDA, na.rm = TRUE),
              UDA_delivered = sum(UDA_delivered, na.rm = TRUE)) %>%
    mutate(performance = UDA_delivered / annual_contracted_UDA) %>%
    mutate(performance = case_when(year == "2022/23" ~ performance * 12 / 6, #scales up to 12 months for this financial year
                                             TRUE ~ performance))
    
  #get UDA summary data
  data_UDAs <- data %>%
    group_by(year, commissioner_name) %>%
    summarise(`Total number of UDAs commissioned` = sum(annual_contracted_UDA, na.rm = TRUE),
              `Total number of UDAs delivered (YTD for 2022/23)` = sum(UDA_delivered, na.rm = TRUE)) %>%
    mutate(`Average performance` = `Total number of UDAs delivered (YTD for 2022/23)` / `Total number of UDAs commissioned`) %>%
    mutate(`Average performance` = case_when(year == "2022/23" ~ `Average performance` * 12 / 6, #scales up to 12 months for this financial year
                                             TRUE ~ `Average performance`))

  #get number of contracts summary data
  data_num_contracts <- data %>%
    group_by(year, commissioner_name) %>%
    count() %>%
    rename(`Total number of contracts` = n)

  #get number of ocntracts delivering under 96%
  data_num_under_96 <- data %>%
    filter(performance < 0.96) %>%
    group_by(year, commissioner_name) %>%
    count() %>%
    rename(`Number of contracts with performance < 96%` = n)

  #get number of contracts delivering over 100%
  data_num_over_100 <- data %>%
    filter(performance > 1) %>%
    group_by(year, commissioner_name) %>%
    count() %>%
    rename(`Number of contracts with performance > 100%` = n)

  #join UDA summary data to number of contracts data
  data_UDAs <- data_UDAs %>%
    left_join(data_num_contracts, by = c("year", "commissioner_name")) %>%
    left_join(data_num_under_96, by = c("year", "commissioner_name")) %>%
    left_join(data_num_over_100, by = c("year", "commissioner_name")) %>%
    replace_na(list(`Number of contracts with performance < 96%` = 0, `Number of contracts with performance > 100%` = 0))

}


################################################################################
get_risk_of_rebasing <- function(data = UDA_scheduled_data,
                                  historic_data = historical_UDA_scheduled_data,
                                  prototypes = prototype_contracts){
  
  region_ICB_lookup <- historic_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  
  data <- data %>% 
    left_join(region_ICB_lookup, by = "commissioner_name") %>%
    bind_rows(historic_data) %>%
    filter(annual_contracted_UDA > 100) %>%
    filter(!(contract_number %in% prototypes$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
    mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01") ~ "2022/23",
                            TRUE ~ year)) %>%
    group_by(year, commissioner_name, contract_number) %>%
    summarise(annual_contracted_UDA = mean(annual_contracted_UDA, na.rm = TRUE),
              UDA_delivered = sum(UDA_delivered, na.rm = TRUE)) %>%
    # mutate(performance = UDA_delivered / annual_contracted_UDA) %>%
    # mutate(performance = case_when(year == "2022/23" ~ performance * 12 / 6, #scales up to 12 months for this financial year
    #                                TRUE ~ performance)) %>%
    filter(year %in% c("2018/19", "2019/20", "2022/23")) %>%
    group_by(year, contract_number) %>%
    arrange(contract_number) %>%
    pivot_wider(names_from = year,
                values_from = c(annual_contracted_UDA, UDA_delivered),
                names_prefix = "year_") %>%
    mutate(performance_2018_19 = `UDA_delivered_year_2018/19` / `annual_contracted_UDA_year_2018/19`,
           performance_2019_20 = `UDA_delivered_year_2019/20` / `annual_contracted_UDA_year_2019/20`,
           performance_2022_23 = `UDA_delivered_year_2022/23` * 2 / `annual_contracted_UDA_year_2022/23`) %>%
    mutate(performance_under_96_last_3_years = case_when(performance_2018_19 < 0.96 & performance_2019_20 < 0.96 & performance_2022_23 < 0.96 ~ TRUE,
                                                         TRUE ~ FALSE)) %>%
    mutate(average_number_of_UDAs_under_delivered = ((`annual_contracted_UDA_year_2018/19` - `UDA_delivered_year_2018/19`) + (`annual_contracted_UDA_year_2019/20` - `UDA_delivered_year_2019/20`) + (`annual_contracted_UDA_year_2022/23` - `UDA_delivered_year_2022/23`)) / 3) %>%
    mutate(number_of_UDAs_released_100_percent_undelivered_UDAs = case_when(performance_under_96_last_3_years ~ average_number_of_UDAs_under_delivered,
                                                                           TRUE ~ 0)) %>%
    mutate(number_of_UDAs_released_75_percent_undelivered_UDAs = case_when(performance_under_96_last_3_years ~ average_number_of_UDAs_under_delivered * 0.75,
                                                                           TRUE ~ 0)) %>%
    mutate(number_of_UDAs_released_50_percent_undelivered_UDAs = case_when(performance_under_96_last_3_years ~ average_number_of_UDAs_under_delivered * 0.50,
                                                                           TRUE ~ 0)) %>%
    mutate(number_of_UDAs_released_25_percent_undelivered_UDAs = case_when(performance_under_96_last_3_years ~ average_number_of_UDAs_under_delivered * 0.25,
                                                                           TRUE ~ 0))

  data_num_contracts_reg <- data %>%
    filter(performance_under_96_last_3_years == TRUE) %>%
    #left_join(region_ICB_lookup, by = "commissioner_name") %>%
    group_by(commissioner_name) %>%
    count() %>%
    rename(num_contracts_at_risk_of_rebasing = n)

  data_reg <- data %>%
    filter(performance_under_96_last_3_years == TRUE) %>%
    #left_join(region_ICB_lookup, by = "commissioner_name") %>%
    group_by(commissioner_name) %>%
    summarise(`total_annual_contracted_UDA_year_2018/19_for_rebased_contracts` = sum(`annual_contracted_UDA_year_2018/19`, na.rm = TRUE),
              `total_annual_contracted_UDA_year_2019/20_for_rebased_contracts` = sum(`annual_contracted_UDA_year_2019/20`, na.rm = TRUE),
              `total_annual_contracted_UDA_year_2022/23_for_rebased_contracts` = sum(`annual_contracted_UDA_year_2022/23`, na.rm = TRUE),
              # `total_annual_contracted_UDA_year_2018/19_for_all_contracts` = sum(`annual_contracted_UDA_year_2018/19`, na.rm = TRUE),
              # `total_annual_contracted_UDA_year_2019/20_for_all_contracts` = sum(`annual_contracted_UDA_year_2019/20`, na.rm = TRUE),
              # `total_annual_contracted_UDA_year_2022/23_for_all_contracts` = sum(`annual_contracted_UDA_year_2022/23`, na.rm = TRUE),
              number_of_UDAs_released_100_percent_undelivered_UDAs = sum(number_of_UDAs_released_100_percent_undelivered_UDAs, na.rm = TRUE)#,
              #number_of_UDAs_released_75_percent_undelivered_UDAs = sum(number_of_UDAs_released_75_percent_undelivered_UDAs, na.rm = TRUE),
              #number_of_UDAs_released_50_percent_undelivered_UDAs = sum(number_of_UDAs_released_50_percent_undelivered_UDAs, na.rm = TRUE),
              #number_of_UDAs_released_25_percent_undelivered_UDAs = sum(number_of_UDAs_released_25_percent_undelivered_UDAs, na.rm = TRUE)
              ) %>%
    left_join(data_num_contracts_reg)

}


get_annual_contracted_regionally <- function(data = UDA_scheduled_data,
                                             historic_data = historical_UDA_scheduled_data,
                                             prototypes = prototype_contracts){
  region_ICB_lookup <- historic_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  
  data <- data %>% 
    left_join(region_ICB_lookup, by = "commissioner_name") %>%
    bind_rows(historic_data) %>%
    filter(annual_contracted_UDA > 100) %>%
    filter(!(contract_number %in% prototypes$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
    mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01") ~ "2022/23",
                            TRUE ~ year)) %>%
    group_by(year, region_name, contract_number) %>%
    summarise(annual_contracted_UDA = mean(annual_contracted_UDA, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, region_name) %>%
    summarise(`total_annual_contracted_UDA_year_2018/19_for_all_contracts` = sum(annual_contracted_UDA, na.rm = TRUE),
      `total_annual_contracted_UDA_year_2019/20_for_all_contracts` = sum(annual_contracted_UDA, na.rm = TRUE),
      `total_annual_contracted_UDA_year_2022/23_for_all_contracts` = sum(annual_contracted_UDA, na.rm = TRUE))
  
}


################################################################################
get_pounds_per_UDA <- function(data = UDA_scheduled_data,
                               historic_data = historical_UDA_scheduled_data,
                               prototypes = prototype_contracts,
                               payments_data = payments_to_dentists){
  
  commissioner_lookup <- data %>% 
    bind_rows(historic_data) %>%
    #filter(annual_contracted_UDA > 100) %>%
    #filter(!(contract_number %in% prototypes$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
    select(contract_number, commissioner_name) %>%
    distinct()
  
  
  payments_data <- payments_data %>%
    filter(year %in% c("2018/19", "2019/20")) %>%
    mutate(Total_Contracted_UDA = as.numeric(Total_Contracted_UDA)) %>%
    filter(Total_Contracted_UDA > 100) %>%
    mutate(contract_number = as.numeric(str_replace(Contract, "/", ""))) %>%
    filter(!(contract_number %in% prototypes$prototype_contract_number)) %>%
    left_join(commissioner_lookup, by = "contract_number") %>%
    group_by(commissioner_name) %>%
    mutate(UDA_value = as.numeric(Baseline_Contract) / as.numeric(Total_Contracted_UDA)) %>%
    summarise(average_pounds_per_UDA = mean(UDA_value, na.rm = TRUE))
  
}


################################################################################
get_yearly_delivered_UDAs <- function(data = UDA_scheduled_data,
                                      historic_data = historical_UDA_scheduled_data,
                                      prototypes = prototype_contracts){
  
    #Join in region column
    data <- data %>% 
      bind_rows(historic_data) %>%
      mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                              month >= as.Date("2022-04-01") ~ "2022/23",
                              TRUE ~ year)) %>%
      filter(annual_contracted_UDA > 100) %>%
      filter(!(contract_number %in% prototypes$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      group_by(year) %>%
      summarise(total_UDA_delivered = sum(UDA_delivered, na.rm = T))
}


################################################################################
get_num_dentists_by_ICB <- function(data = payments_to_dentists,
                                    scheduled_data = UDA_scheduled_data,
                                    historic_data = historical_UDA_scheduled_data){
  
  
  contract_ICB_lookup <- scheduled_data %>%
    bind_rows(historic_data) %>%
    select(contract_number, commissioner_name) %>%
    distinct()
  
  data <- data %>%
    mutate(contract_number = as.numeric(str_replace(Contract, "/", ""))) %>%
    mutate(Number_of_active_dentists = as.numeric(Number_of_active_dentists)) %>%
    left_join(contract_ICB_lookup) %>%
    select(year, contract_number, commissioner_name, STP_Name, Region_Name, Number_of_active_dentists) %>%
    group_by(year, commissioner_name) %>%
    summarise(total_number_of_active_dentists = sum(Number_of_active_dentists, na.rm = TRUE)) %>%
    pivot_wider(names_from = year, names_prefix = "Number of active dentists ", values_from = total_number_of_active_dentists)
  
}