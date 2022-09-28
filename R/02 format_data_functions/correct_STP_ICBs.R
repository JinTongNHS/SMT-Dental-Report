################################################################################
correct_STP_ICBs <- function(data = UDA_scheduled_data, 
                             scheduled_data = UDA_scheduled_data,
                             lookup_STP_ICB = STP_ICB_lookup_codes,
                             output = "UDA scheduled"){
  
  #lookup based on contract number
  lookup <- scheduled_data %>%
    filter(month == as.Date("2022-07-01")) %>%
    select(contract_number, 
           commissioner_name_ICB = commissioner_name)
  
  #lookup for new ICB to region
  lookup_ICB_region <- lookup_STP_ICB %>%
    select(commissioner_name = commissioner_name_ICB,
           region_name)
  
  #lookup based on old STP to new ICB for contracts that ended before June 2022
  lookup_STP_ICB <- STP_ICB_lookup_codes %>%
    select(commissioner_name = commissioner_name_STP,
           commissioner_name_ICB_lookup = commissioner_name_ICB)
  
  
  data <- data %>%
    left_join(lookup, by = "contract_number") %>%
    left_join(lookup_STP_ICB, by = "commissioner_name") %>%
    mutate(commissioner_name = case_when(is.na(commissioner_name_ICB) & month < as.Date("2022-06-01") ~ commissioner_name_ICB_lookup,
                                         !is.na(commissioner_name_ICB) & month < as.Date("2022-06-01") ~ commissioner_name_ICB,
                                         TRUE ~ commissioner_name)) %>%
    select(-commissioner_name_ICB, -commissioner_name_ICB_lookup)
  
  if(output == "UDA calendar" | output == "UOA calendar"){
    data <- data %>%
      left_join(lookup_ICB_region, by = "commissioner_name") %>%
      mutate(region_name.x = region_name.y) %>%
      rename(region_name = region_name.x) %>%
      select(-region_name.y)
  }
  
  data
}
