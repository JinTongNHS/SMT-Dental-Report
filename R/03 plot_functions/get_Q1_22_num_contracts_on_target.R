################################################################################
get_Q1_22_num_contracts_on_target <- function(data = UDA_scheduled_data, 
                                              calendar_data = UDA_calendar_data,
                                              remove_prototypes = T,
                                              scheduled_data = UDA_scheduled_data,
                                              UDAorUOA = "UDA",
                                              level = "National",
                                              region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = "commissioner_name")
  
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
    data <- data %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.6 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-01-01") ~ 0.65 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-04-01") ~ 0.85 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-07-01") ~ 0.95 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-09-01") ~ 1 * annual_contracted_UDA/4))
  }else{
    data <- data %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-01-01") ~ 0.85 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-04-01") ~ 0.90 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-09-01") ~ 1 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    filter(month >= as.Date("2022-07-01")) ##must update this at start of each quarter
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2022-07-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2022-08-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2022-09-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                quarter_to_date_UDA_delivered = sum(UDA_delivered)) %>%
      mutate(mean_Q4_UDA_target = mean_annual_contracted_UDA * 1 / 4) %>%
      count(quarter_to_date_UDA_delivered >= (mean_Q4_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                quarter_to_date_UOA_delivered = sum(UOA_delivered)) %>%
      mutate(mean_Q4_UOA_target = mean_annual_contracted_UOA * 1 / 4) %>%
      count(quarter_to_date_UOA_delivered >= (mean_Q4_UOA_target) * month_factor / 3)
  }
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)
  
}