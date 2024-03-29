################################################################################
table_number_cat_contracts <- function(data = UDA_scheduled_data, 
                                       contractor_cats = contractor_categories,
                                       remove_prototypes = F,
                                       level = NULL,
                                       region_STP_name = NULL){
  
  
  #filters out double mappings for contractor categories - takes first mapping as correct
  contractor_cats <- contractor_cats %>%
    distinct(contract_number, .keep_all = TRUE)
  
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

  
  #remove prototype contracts if specified
  if(remove_prototypes){
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats, by = "contract_number") %>%
    group_by(month) %>%
    count(category_sub_type) %>%
    ungroup() %>%
    filter(month == max(data$month)) %>%
    select(category_sub_type, n) %>%
    rename(number_of_contracts = n) 
  
  data <- data %>%
    mutate(percentage_of_contracts = round(number_of_contracts * 100 / sum(data$number_of_contracts))) %>%
    add_row(category_sub_type = "TOTAL", number_of_contracts = sum(data$number_of_contracts), percentage_of_contracts = 100) %>%
    rename(`category sub type` = category_sub_type,
           `number of contracts` = number_of_contracts,
           `percentage of contracts` = percentage_of_contracts) %>%
    mutate(`category sub type` = if_else(is.na(`category sub type`), "Category not known", `category sub type`))
  
  data

  
}