################################################################################
#function to get dental data into the right format for slide 4
get_delivery_data_MY_cat <- function(calendar_data = UDA_calendar_data,  
                                       scheduled_data = UDA_scheduled_data,
                                       remove_prototypes = T,
                                       UDAorUOA = "UDA",
                                       regional_lines = F,
                                       STP_lines = F,
                                       cat_lines = F, 
                                       renameColumns = F){
  
  data <- scheduled_data
  
  #remove prototype contracts if specified
  #N.B. excludes prototypes for all time periods for comparison purposes
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      # filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number)) %>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      # filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number)) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(regional_lines){
    data <- data %>%
      group_by(month, region_name)
  }else if(STP_lines){
    data <- data %>%
      group_by(month, region_name, commissioner_name)
  }else if(cat_lines){
    data <- data %>%
      group_by(month, category_sub_type)
  }else{
    data <- data %>%
      group_by(month)
  }
  
  data <- data %>%
    summarise(monthly_UDA_UOAs_delivered = ifelse(UDAorUOA == "UDA",
                                                  sum(UDA_delivered, na.rm = T),
                                                  sum(UOA_delivered, na.rm = T)),
              annual_contracted_UDA_UOA = ifelse(UDAorUOA == "UDA",
                                                 sum(annual_contracted_UDA, na.rm = T),
                                                 sum(annual_contracted_UOA, na.rm = T))) %>%
    mutate(scaled_monthly_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 12) %>%
    mutate(scaled_perc_UDA_UOA_delivered = monthly_UDA_UOAs_delivered * 12 * 100 / annual_contracted_UDA_UOA) %>%
    mutate(month = as.Date(month))
  
  
  #for PCOR and SOF table
  if(renameColumns){
    
    data <- data %>%
      select(calendar_month = month,
             commissioner_name,
             region_name,
             monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
             scaled_monthly_UDAs_delivered = scaled_monthly_UDA_UOAs_delivered,
             annual_contracted_UDAs = annual_contracted_UDA_UOA,
             scaled_perc_UDAs_delivered = scaled_perc_UDA_UOA_delivered) %>%
      mutate(threshold_perc = case_when(calendar_month >= as.Date("2021-04-01") & calendar_month < as.Date("2021-10-01") ~ "60",
                                        calendar_month >= as.Date("2021-10-01") & calendar_month < as.Date("2022-01-01") ~ "65",
                                        calendar_month >= as.Date("2022-01-01") & calendar_month < as.Date("2022-04-01") ~ "85",
                                        calendar_month >= as.Date("2022-04-01") & calendar_month < as.Date("2022-07-01") ~ "95"))
  }
  
  data
}