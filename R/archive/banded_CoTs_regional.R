get_banded_COTs_data <- function(data = UDA_scheduled_data,
                                 calendar_data = UDA_calendar_data,
                                 historic_data = historical_UDA_scheduled_data,
                                 remove_prototypes = F){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
  #bind old data to new data 
  data <- data %>%
    rename(total_FP17s = general_FP17s, band1_UDA = UDA_band_1, band2_UDA = UDA_band_2, 
           band3_UDA = UDA_band_3, urgent_UDA = UDA_urgent, other_UDA = UDA_other, band1_FP17 = FP17s_band_1, 
           band2_FP17 = FP17s_band_2, band3_FP17 = FP17s_band_3, urgent_FP17 = FP17s_band_urgent, 
           other_FP17 = FP17s_band_other) %>%
    select(month, contract_number, total_FP17s, band1_UDA, band2_UDA, band3_UDA, 
           urgent_UDA, other_UDA, band1_FP17, band2_FP17, band3_FP17, urgent_FP17, other_FP17, 
           region_name) %>%
    mutate(total_UDAs = band1_UDA + band2_UDA + band3_UDA + other_UDA + urgent_UDA)
  
  data <- bind_rows(data, historic_data)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts_orth$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  #group by month and sum UDAs delivered
  new_data <- data %>%
    group_by(month, region_name) %>%
    summarise(band1 = sum(band1_FP17, na.rm = T),
              band2 = sum(band2_FP17, na.rm = T),
              band3 = sum(band3_FP17, na.rm = T),
              other = sum(other_FP17, na.rm = T),
              urgent = sum(urgent_FP17, na.rm = T)
    ) #%>%
  #filter(month == max(month))
  
}