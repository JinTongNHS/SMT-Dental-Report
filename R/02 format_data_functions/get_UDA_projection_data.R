#september_BSA_UDA_value_data <- pull_September_BSA_UDA_value_data()

#What do do with missing UDA delivery data for average

#What to do when contracted UDAs changes month on month for one contract?

get_UDA_projection_data <- function(data = UDA_scheduled_data,
                                    UDA_value_data = september_BSA_UDA_value_data, 
                                    #calendar_data = UDA_calendar_data,
                                    level = "National",
                                    region_STP_name = NULL,
                                    remove_prototypes = TRUE, 
                                    plotChart = TRUE){
  
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #remove prototypes and contracts with annual contracted UDAs < 100 if required
  if(remove_prototypes == TRUE){
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #rename and select column from September UDA value data
  names(UDA_value_data) <- names(UDA_value_data) %>% make.names()
  
  UDA_value_data <- UDA_value_data %>%
    select(contract_number = Contract.Number,
           UDA_finanical_value = UDA.Financial.Value,
           cost_per_UDA = Cost.per.UDA)
  
  #join to UDA scheduled data
  data_wide <- data %>%
    select(month, contract_number, #name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, annual_contracted_UDA, 
           UDA_delivered) %>%
    filter(month >= as.Date("2022-04-01")) %>%
    left_join(UDA_value_data, by = "contract_number") %>%
    arrange(month) %>%
    mutate(month = format(month, "%B-%Y")) %>%
    pivot_wider(names_from = month,
                values_from = UDA_delivered,
                names_prefix = "Delivered UDA in ",
                values_fill = 0) %>%
    mutate_at(vars(starts_with("Delivered UDA")), ~replace_na(.,0))
  
  #get averages
  data_means <- data %>%
    #filter(month >= lubridate::floor_date(max(data$month) - months(6), "month")) %>%
    filter(month > as.Date("2022-04-01")) %>% #removes month of April from calculation
    mutate(UDA_delivered = replace_na(UDA_delivered, 0)) %>%
    group_by(contract_number) %>%
    summarise(avrg_last_6_month_delivered_UDA = mean(UDA_delivered, na.rm = TRUE)) #will need to change name
  
  #get most recent month's contract details
  data_contract_details <- data %>%
    filter(month == max(data$month)) %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, annual_contracted_UDA)

  #get number of months left in financial year
  num_months_left_in_financial_year <- 12 - (as.numeric(substr(max(data$month), 6, 7)) - 3)

  #join in UDA means to rest of data
  data <- data_wide %>%
    left_join(data_contract_details) %>%
    left_join(data_means, by = "contract_number") %>%
    mutate(projected_of_rest_year_delivery = avrg_last_6_month_delivered_UDA * num_months_left_in_financial_year) %>%
    mutate(YTD_delivery = rowSums(across(starts_with("Delivered UDA")), na.rm = TRUE)) %>%
    mutate(projected_total_year_delivery = YTD_delivery + projected_of_rest_year_delivery) %>%
    mutate(projected_percentage_delivery_of_contracted_UDAs = projected_total_year_delivery * 100 / annual_contracted_UDA) %>%
    mutate(performance_category = case_when(is.na(avrg_last_6_month_delivered_UDA) ~ 'ignore',
                                            avrg_last_6_month_delivered_UDA == 0 ~ 'ignore',
                                            projected_percentage_delivery_of_contracted_UDAs < 96 ~ 'Projected to deliver less than 96%',
                                            projected_percentage_delivery_of_contracted_UDAs >= 96 ~ 'Projected to deliver 96% or more')) %>% # changed to >=96 from >95
    select(contract_number, name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, annual_contracted_UDA,
           UDA_finanical_value, cost_per_UDA, everything())
  
}

report <- get_UDA_projection_data()
