################################################################################
get_num_contracts <- function(data = UDA_scheduled_data, 
                              remove_prototypes = TRUE,
                              calendar_data = UDA_calendar_data,
                              UDAorUOA = "UDA",
                              level = "National",
                              region_STP_name = NULL){
  
  #join in region column from calendar data
  region_icb_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    unique()
  
  data <- data %>%
    left_join(region_icb_lookup, by = "commissioner_name")
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }
  
  # if(UDAorUOA == "UDA"){
  #   #get contracted UDAs
  #   contracted_UDA_UOAs <- scheduled_data %>%
  #     select(month, contract_number, annual_contracted_UDA)
  # }else{
  #   #get contracted UOAs
  #   contracted_UDA_UOAs <- scheduled_data %>%
  #     select(month, contract_number, annual_contracted_UOA)
  # }
  # 
  # #join in contracted UDA/UOAs from scheduled data
  # data <- data %>%
  #   left_join(contracted_UDA_UOAs, by = c("month", "contract_number"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
     # filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)
  }
  
  data <- data %>%
    filter(month == max(data$month))
  
  nrow(data)
}
