################################################################################
get_delivery_profile_data_full_year <- function(data = UDA_scheduled_data,
                                      UDAorUOA = "UDA",
                                      remove_prototypes = TRUE,
                                      all_regions_and_STPs = FALSE){
  
  
  #remove prototype contracts if specified
  #only remove prototypes before April 2022
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UOA > 0)
  }

  if(UDAorUOA == "UDA"){
    #get most recent annual contracted UDAs
    most_recent_annual_contracted <- data %>%
      filter(month == max(data$month)) %>%
      select(contract_number, annual_contracted_UDA)
    
    performance_table <- data %>%
      group_by(contract_number) %>%
      summarise(UDA_delivered = sum(UDA_delivered, na.rm = TRUE)) %>%
      left_join(most_recent_annual_contracted, by = "contract_number") %>%
      mutate(monthly_perc_scaled = UDA_delivered * 100 / annual_contracted_UDA) 
    # performance_table <- data %>%
    #   group_by(contract_number) %>%
    #   summarise(UDA_delivered = sum(UDA_delivered, na.rm = TRUE),
    #             annual_contracted_UDA = mean(annual_contracted_UDA)) %>% #######################
    #   mutate(monthly_perc_scaled = UDA_delivered * 100 / annual_contracted_UDA) 
    
  }else{
    
    #get most recent annual contracted UOAs
    most_recent_annual_contracted <- data %>%
      filter(month == max(data$month)) %>%
      select(contract_number, annual_contracted_UOA)
    
    performance_table <- data %>%
      group_by(contract_number) %>%
      summarise(UDA_delivered = sum(UOA_delivered, na.rm = TRUE)) %>%
      left_join(most_recent_annual_contracted, by = "contract_number") %>%
      mutate(monthly_perc_scaled = UOA_delivered * 100 / annual_contracted_UOA)
    
    # performance_table <- data %>%
    #   group_by(contract_number) %>%
    #   summarise(UDA_delivered = sum(UOA_delivered, na.rm = TRUE),
    #             annual_contracted_UDA = mean(annual_contracted_UOA)) %>% #########################
    #   mutate(monthly_perc_scaled = UOA_delivered * 100 / annual_contracted_UOA)
  }
  
  #put into bands then sum across these bands by month
  performance_table <- performance_table %>%
    mutate(performance_band = case_when(monthly_perc_scaled >= 0 & monthly_perc_scaled <10 ~ "0-9%",
                                        monthly_perc_scaled >= 10 & monthly_perc_scaled <20 ~ "10-19%",
                                        monthly_perc_scaled >= 20 & monthly_perc_scaled <30 ~ "20-29%",
                                        monthly_perc_scaled >= 30 & monthly_perc_scaled <40 ~ "30-39%",
                                        monthly_perc_scaled >= 40 & monthly_perc_scaled <50 ~ "40-49%",
                                        monthly_perc_scaled >= 50 & monthly_perc_scaled <60 ~ "50-59%",
                                        monthly_perc_scaled >= 60 & monthly_perc_scaled <70 ~ "60-69%",
                                        monthly_perc_scaled >= 70 & monthly_perc_scaled <80 ~ "70-79%",
                                        monthly_perc_scaled >= 80 & monthly_perc_scaled <90 ~ "80-89%",
                                        monthly_perc_scaled >= 80 & monthly_perc_scaled <90 ~ "80-89%",
                                        monthly_perc_scaled >= 90 & monthly_perc_scaled <100 ~ "90-99%",
                                        monthly_perc_scaled >= 100 ~ "100% +",
                                        is.na(monthly_perc_scaled) ~ "UDA delivery data \ninvalid or not given",
                                        monthly_perc_scaled < 0 ~ "UDA delivery data \ninvalid or not given"
    )) 
  
  if(all_regions_and_STPs == FALSE){
    performance_table <- performance_table 
  }else{
    performance_table <- performance_table %>%
      group_by(region_name, commissioner_name)
  }
  
  # performance_table <- performance_table %>%
  #   count(performance_band) %>%
  #   mutate(no_of_contracts = sum(n)) %>%
  #   ungroup() %>%
  #   mutate(perc_of_contracts = n * 100 / no_of_contracts) 
  
  performance_table
}
