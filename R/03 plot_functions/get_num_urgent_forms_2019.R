################################################################################
get_num_urgent_forms_2019 <- function(data = UDA_scheduled_data, 
                                      historic_data = historical_UDA_scheduled_data,
                                      remove_prototypes = FALSE,
                                      level = "National",
                                      region_STP_name = NULL){
  

  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    historic_data <- historic_data %>% 
      filter(commissioner_name == region_STP_name )
  }
  
  data <- get_banded_COTs_data(data = data, 
                               historic_data = historic_data,
                               remove_prototypes = remove_prototypes)
  
  data <- data %>%
    filter(month == max(data$month) - lubridate::years(3))
  
  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)
  
}
