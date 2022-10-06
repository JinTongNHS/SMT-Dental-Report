################################################################################
get_banded_COTs_data <- function(data = UDA_scheduled_data, 
                                 historic_data = historical_UDA_scheduled_data,
                                 remove_prototypes = TRUE,
                                 all_regions_and_STPs = FALSE){
  
  #bind old data to new data 
  data <- data %>%
   mutate(total_UDAs = sum(UDA_band_1, UDA_band_2, UDA_band_3, UDA_other, UDA_urgent, na.rm = TRUE))
  
  historic_data <- historic_data %>%
    rename(total_UDAs = UDA_delivered)
  
  data <- bind_rows(data, historic_data)
  
  #remove prototype contracts if specified
  #only removes prototypes before April 2022
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  if(all_regions_and_STPs == FALSE){
    #group by month and sum UDAs delivered
    new_data <- data %>%
      group_by(month)
    
  }else{
    #group by month and sum UDAs delivered
    new_data <- data %>%
      group_by(month, region_name, commissioner_name)
  }
  
  new_data <- new_data %>%
    summarise(band1 = sum(FP17s_band_1, na.rm = T),
              band2 = sum(FP17s_band_2, na.rm = T),
              band3 = sum(FP17s_band_3, na.rm = T),
              other = sum(FP17s_band_other, na.rm = T),
              urgent = sum(FP17s_band_urgent, na.rm = T)
    )
  
}
