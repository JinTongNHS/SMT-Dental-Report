################################################################################
get_Q4_num_contracts_on_target <- function(data = UDA_calendar_data, 
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
                                                   month < as.Date("2022-01-01") ~ 0.65 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-04-01") ~ 0.85 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-01-01") ~ 0.85 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-04-01") ~ 0.90 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2022-01-01"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2022-01-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2022-02-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2022-03-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                #mean_UDA_target = mean(UDA_financial_half_target),
                YTD_UDA_delivered = sum(UDA_total)) %>%
      mutate(mean_Q4_UDA_target = mean_annual_contracted_UDA * 0.85 / 4) %>%
      count(YTD_UDA_delivered >= (mean_Q4_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                #mean_Q3_UOA_target = mean(UOA_financial_half_target),
                quarter_to_date_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q4_UOA_target = mean_annual_contracted_UOA * 0.90 / 4) %>%
      count(quarter_to_date_UOA_delivered >= (mean_Q4_UOA_target) * month_factor / 3)
  }
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)
  
}