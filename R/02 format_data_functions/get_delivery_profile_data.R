################################################################################
get_delivery_profile_data <- function(data = UDA_scheduled_data,
                                      UDAorUOA = "UDA",
                                      remove_prototypes = T,
                                      all_regions_and_STPs = FALSE){
  
  # #filter out ended contracts
  # data <- data %>%
  #   filter(is.na(contract_end_date) | contract_end_date > month)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(UDAorUOA == "UDA"){
    #create column for 12 month scaled % of UDAs delivered. April must be scaled differently.
    performance_table <- data %>%
      mutate(monthly_perc_scaled = if_else(month != as.Date("2021-04-01") & month != as.Date("2022-04-01"),
                                           round(UDA_delivered * 12 * 100 / annual_contracted_UDA),
                                           round(UDA_delivered * 18 * 100 / annual_contracted_UDA))) 
  }else{
    #create column for 12 month scaled % of UOAs delivered 
    performance_table <- data %>%
      mutate(monthly_perc_scaled = if_else(month != as.Date("2021-04-01") & month != as.Date("2022-04-01"),
                                           round(UOA_delivered * 12 * 100 / annual_contracted_UOA),
                                           round(UOA_delivered * 18 * 100 / annual_contracted_UOA)))
  }
  
  #put into bands then sum across these bands by month
  performance_table <- performance_table %>%
    mutate(performance_band = if_else(monthly_perc_scaled >= 0 & monthly_perc_scaled <10, "0-9%",
                                      if_else(monthly_perc_scaled >= 10 & monthly_perc_scaled <20, "10-19%",
                                              if_else(monthly_perc_scaled >= 20 & monthly_perc_scaled <30, "20-29%",
                                                      if_else(monthly_perc_scaled >= 30 & monthly_perc_scaled <40, "30-39%",
                                                              if_else(monthly_perc_scaled >= 40 & monthly_perc_scaled <50, "40-49%",
                                                                      if_else(monthly_perc_scaled >= 50 & monthly_perc_scaled <60, "50-59%",
                                                                              if_else(monthly_perc_scaled >= 60 & monthly_perc_scaled <70, "60-69%",
                                                                                      if_else(monthly_perc_scaled >= 70 & monthly_perc_scaled <80, "70-79%",
                                                                                              if_else(monthly_perc_scaled >= 80 & monthly_perc_scaled <90, "80-89%",
                                                                                                      if_else( monthly_perc_scaled >= 90 & monthly_perc_scaled <100, "90-99%",
                                                                                                               "100% +")
                                                                                              )
                                                                                      )
                                                                              )
                                                                      )
                                                              )
                                                      )
                                              )
                                      )
    )) %>%
    #exclude NAs
    filter(!is.na(performance_band)) 
  
  if(all_regions_and_STPs == FALSE){
    performance_table <- performance_table %>%
      group_by(month)
  }else{
    performance_table <- performance_table %>%
      group_by(month, region_name, commissioner_name)
  }
  
  performance_table <- performance_table %>%
    count(performance_band) %>%
    mutate(no_of_contracts = sum(n)) %>%
    ungroup() %>%
    mutate(perc_of_contracts = n * 100 / no_of_contracts) %>%
    mutate(month = as.POSIXct(month))
  
  performance_table
}
