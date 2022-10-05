################################################################################
get_data_for_cumulative_plot_UOA <- function(data = UOA_calendar_data, 
                                             scheduled_data = UOA_scheduled_data,
                                             remove_prototypes = TRUE,
                                             all_regions_and_STPs = FALSE){
  
  #join in contracted UDAs from scheduled data
  contracted_UOAs <- scheduled_data %>%
    select(month, contract_number, annual_contracted_UOA)
  
  data <- data %>%
    left_join(contracted_UOAs, by = c("month", "contract_number"))
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    
    
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(all_regions_and_STPs == FALSE){
    UOAs_delivered <- data %>%
      group_by(month)
  }else{
    UOAs_delivered <- data %>%
      group_by(month, region_name, commissioner_name)
  }
  #group by month and sum UDAs delivered
  UOAs_delivered <- UOAs_delivered %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UOA_total, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UOA, na.rm = T)) %>%
    mutate(threshold_perc = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ 0.8,
                                      month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ 0.85,
                                      month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ 0.90,
                                      month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ 1)) %>%
    mutate(threshold_period = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ lubridate::interval(as.Date("2021-04-01"), as.Date("2021-09-30")),
                                        month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ lubridate::interval(as.Date("2021-10-01"), as.Date("2021-12-31")),
                                        month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ lubridate::interval(as.Date("2022-01-01"), as.Date("2022-03-31")),
                                        month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ lubridate::interval(as.Date("2022-04-01"), as.Date("2022-06-30")))) %>%
    mutate(threshold_UDA_UOAs_contracted_in_threshold_period = total_annual_UDA_UOAs_contracted * threshold_perc * lubridate::time_length(threshold_period, "month")/ 12)
}