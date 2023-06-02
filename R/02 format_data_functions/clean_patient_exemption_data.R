clean_patient_exemption_data <- function(data = patient_exemption_data){
  
  data_long <- gather(data, 
                      key = patient_type, 
                      value = number_uda, 
                      -Treatment_Charge_Band, -Regions, -FY, #-adult_exempt,
                      #-Child, -adult_non_exempt, 
                      factor_key=TRUE)

  ###to remove comma's from data column
  data_long <- data_long %>%
    mutate(number_uda = str_replace(number_uda, ",", "")) %>%
    mutate(number_uda = str_replace(number_uda, ",", "")) %>%
    mutate(number_uda = as.numeric(number_uda))
  
}