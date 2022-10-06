################################################################################
get_num_urgent_forms <- function(data = UDA_scheduled_data, 
                                 historic_data = historical_UDA_scheduled_data,
                                 calendar_data = UDA_calendar_data,
                                 remove_prototypes = TRUE,
                                 level = "National",
                                 region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = "commissioner_name")
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
  }
  
  data <- get_banded_COTs_data(data = data, 
                               historic_data = historic_data,
                               remove_prototypes = remove_prototypes)
  
  data <- data %>% 
    filter(month == max(data$month))
  
  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)
  
}