################################################################################
get_data_for_cumulative_plot <- function(data = UDA_scheduled_data, 
                                         calendar_data = UDA_calendar_data,
                                         remove_prototypes = T,
                                         all_regions_and_STPs = F){
  

  #remove prototype contracts if specified
  #only remove prototypes before April 2022
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #option to have data for all regions and STPs in one
  if(all_regions_and_STPs == TRUE){
    UDAs_delivered <- data %>%
      group_by(month, region_name, commissioner_name)
  }else{
    UDAs_delivered <- data %>%
      group_by(month)
  }
  
  #group by month and sum UDAs delivered
  UDAs_delivered <- UDAs_delivered %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UDA_delivered, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UDA)) %>%
    mutate(threshold_perc = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ 0.6,
                                      month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ 0.65,
                                      month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ 0.85,
                                      month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ 0.95)) %>%
    mutate(threshold_period = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ lubridate::interval(as.Date("2021-04-01"), as.Date("2021-09-30")),
                                        month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ lubridate::interval(as.Date("2021-10-01"), as.Date("2021-12-31")),
                                        month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ lubridate::interval(as.Date("2022-01-01"), as.Date("2022-03-31")),
                                        month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ lubridate::interval(as.Date("2022-04-01"), as.Date("2022-06-30")))) %>%
    mutate(threshold_UDA_UOAs_contracted_in_threshold_period = total_annual_UDA_UOAs_contracted * threshold_perc * lubridate::time_length(threshold_period, "month")/ 12) 
  
}