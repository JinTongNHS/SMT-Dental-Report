################################################################################
plot_cumulative_UDA_value_all_regions <- function(data = UDA_scheduled_data,
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
  
  
  #get cumulative data for this financial year
  data_this_year <- data %>%
    filter(month >= as.Date("2022-04-01")) %>%
    group_by(month, region_name, commissioner_name) %>%
    summarise(financial_delivery = sum(financial_delivery, na.rm = TRUE)) %>%
    mutate(financial_delivery_million_pounds = financial_delivery / 1000000) %>%
    mutate(cum_financial_delivery_million_pounds = cumsum(financial_delivery_million_pounds))
  
  #get mean over 2017/18 - 2019/20
  prev_years <- data %>%
    filter(month < as.Date("2022-04-01") & month >= as.Date("2017-04-01")) %>%
    group_by(year, region_name, commissioner_name) %>%
    summarise(financial_delivery = sum(financial_delivery, na.rm = TRUE)) %>%
    mutate(financial_delivery_million_pounds = financial_delivery / 1000000) %>%
    ungroup() %>%
    group_by(region_name, commissioner_name) %>%
    summarise(financial_delivery_million_pounds_2017_2020_mean = mean(financial_delivery_million_pounds))
  
  #join in prev year
  data_this_year <- data_this_year %>%
    left_join(prev_years) %>%
    rename(Month = month,
           `Region Name` = region_name,
           `Commissioner Name` = commissioner_name,
           `Financial delivery (pounds)` = financial_delivery,
           `Financial delivery (million pounds)` = financial_delivery_million_pounds,
           `Cumulative financial delivery (million pounds)` = cum_financial_delivery_million_pounds,
           `2017/18 - 2019/20 mean financial delivery (million pounds)` = financial_delivery_million_pounds_2017_2020_mean)
  
 
  
}