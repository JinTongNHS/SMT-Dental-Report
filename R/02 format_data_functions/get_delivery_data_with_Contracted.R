################################################################################
get_delivery_data_with_contracted <- function(data = UDA_scheduled_data,
                              remove_prototypes = T,
                              UDAorUOA = "UDA",
                              all_regions_and_STPs = F,
                              contractor_level = F,
                              renameColumns = F){
  
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
  
  #show all ICBs in output data if specified
  if(all_regions_and_STPs == TRUE){
    
    data <- data %>%
      group_by(month, region_name, commissioner_name)
    
    
  }else if(contractor_level == TRUE){
    
    data <- data %>%
      group_by(month, contract_number, commissioner_name)
    
  }else{
    data <- data %>%
      group_by(month)
  }
  
  #group by month and sum UDAs delivered
  UDA_UOAs_delivered <- data %>%
    summarise(monthly_UDA_UOAs_delivered = ifelse(UDAorUOA == "UDA", 
                                                  sum(UDA_delivered, na.rm = T),
                                                  sum(UOA_delivered, na.rm = T)),
              annual_contracted_UDA_UOA = ifelse(UDAorUOA == "UDA",
                                                 sum(annual_contracted_UDA, na.rm = T),
                                                 sum(annual_contracted_UOA, na.rm = T))
    )
  
  new_data <- UDA_UOAs_delivered %>%
    #April data needs scaling differently
    mutate(scaled_monthly_UDA_UOAs_contracted = if_else(month != as.Date("2021-04-01") & month != as.Date("2020-04-01") 
                                                        & month != as.Date("2019-04-01") & month != as.Date("2022-04-01"),
                                                        annual_contracted_UDA_UOA / 12,
                                                        annual_contracted_UDA_UOA / 18)) %>%
    mutate(perc_UDA_UOA_delivered = round(monthly_UDA_UOAs_delivered * 100 / scaled_monthly_UDA_UOAs_contracted)) 
  
  #for PCOR and SOF table
  if(renameColumns){
    
    new_data <- new_data %>%
      select(scheduled_month = month,
             commissioner_name,
             region_name,
             monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
             scaled_monthly_UDAs_contracted = scaled_monthly_UDA_UOAs_contracted,
             annual_contracted_UDAs = annual_contracted_UDA_UOA,
             scaled_perc_UDAs_delivered = perc_UDA_UOA_delivered) %>%
      mutate(threshold_perc = case_when(scheduled_month >= as.Date("2021-04-01") & scheduled_month < as.Date("2021-10-01") ~ "60",
                                        scheduled_month >= as.Date("2021-10-01") & scheduled_month < as.Date("2022-01-01") ~ "65",
                                        scheduled_month >= as.Date("2022-01-01") & scheduled_month < as.Date("2022-04-01") ~ "85",
                                        scheduled_month >= as.Date("2022-04-01") & scheduled_month < as.Date("2022-07-01") ~ "95"))
  }
  
  new_data
  
}
