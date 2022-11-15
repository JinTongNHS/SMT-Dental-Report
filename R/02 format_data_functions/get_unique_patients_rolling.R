################################################################################
#function to plot unique patients by band
get_unique_patients_rolling <- function(data = unique_patients_rolling,
                                         scheduled_data = UDA_scheduled_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         plotChart = TRUE,
                                         remove_prototypes = TRUE){

  
  data <- data %>%
    rename(month = month_ending)
  
  #join annual contracted UDAs
  contracted_UDAs <- scheduled_data %>%
    filter(month == max(scheduled_data$month, na.rm = TRUE)) %>%
    select(contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(contracted_UDAs, by = "contract_number")
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(!(contract_number %in% prototype_contracts$prototype_contract_number & month < as.Date("2022-04-01"))) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  
  data <- data %>%
    summarise(unique_patients_rolling_12M = sum(unique_patients_rolling_12M, na.rm = TRUE),
              band1_unique_patients_rolling_12M = sum(band1_unique_patients_rolling_12M, na.rm = TRUE),
              band2_or_3_unique_patients_rolling_12M = sum(band2_or_3_unique_patients_rolling_12M, na.rm = TRUE),
              band1_urgent_unique_patients_rolling_12M = sum(band1_urgent_unique_patients_rolling_12M, na.rm = TRUE),
              band_other_unique_patients_rolling_12M = sum(band_other_unique_patients_rolling_12M, na.rm = TRUE))  %>%
    pivot_longer(c(unique_patients_rolling_12M,
                   band1_unique_patients_rolling_12M,
                   band2_or_3_unique_patients_rolling_12M,
                   band1_urgent_unique_patients_rolling_12M,
                   band_other_unique_patients_rolling_12M)) %>%
    rename(band = name, total_unique_patients = value) %>%
    mutate(band = case_when(band == "unique_patients_rolling_12M" ~ "Any band",
                            band == "band1_unique_patients_rolling_12M" ~ "Band 1",
                            band == "band2_or_3_unique_patients_rolling_12M" ~ "Band 2 or 3",
                            band == "band1_urgent_unique_patients_rolling_12M" ~ "Urgent band 1",
                            band == "band_other_unique_patients_rolling_12M" ~ "Other band")) 
  
  
}
