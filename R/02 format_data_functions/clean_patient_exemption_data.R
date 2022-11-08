clean_patient_exemption_data <- function(data = patient_exemption_data){
  
  data_long <- gather(data, patient_type, number_uda, ###best to use pivot_longer
                      Child:adult_non_exempt, 
                      factor_key=TRUE)
  
  
  ###to remove comma's from data column
  data_long <- data_long %>%
    mutate(number_uda = str_replace(number_uda, ",", "")) %>%
    mutate(number_uda = str_replace(number_uda, ",", "")) %>%
    mutate(number_uda = as.numeric(number_uda))
  
}