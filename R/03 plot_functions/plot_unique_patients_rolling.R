################################################################################
#function to plot unique patients by band
plot_unique_patients_rolling <- function(data = unique_patients_rolling,
                                         scheduled_data = UDA_scheduled_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         plotChart = TRUE,
                                         remove_prototypes = FALSE,
                                         get_perc = FALSE,
                                         all_regiona_and_STPs = FALSE,
                                         contract_level = FALSE,
                                         asIndex = FALSE){
  
  #avoid standard form notation
  options(scipen = 5)
  
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
    
    chartCaption <- "*N.B. this analysis uses unique patients per contract** and does not take \ninto account patients who have been seen at more than one dental practice. \n**EXCLUDING contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022."
  }else{
    
    chartCaption <- "*N.B. this analysis uses unique patients per contract** and does not take \ninto account patients who have been seen at more than one dental practice. \n**INCLUDING contracts with annual contracted UDA < 100 and prototype contracts."
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
  
  #group by different levels depending on inputs
  if(all_regiona_and_STPs == TRUE){
    data <- data %>%
      group_by(month, region_name, commissioner_name)
    
  }else if(contract_level == TRUE){
    
    data <- data %>%
      group_by(month, contract_number, region_name, commissioner_name)
    
  }else{
    data <- data %>%
      group_by(month)
  }
  
  data_to_print <- data %>%
    summarise(unique_patients_rolling_12M = sum(unique_patients_rolling_12M, na.rm = TRUE),
              band1_unique_patients_rolling_12M = sum(band1_unique_patients_rolling_12M, na.rm = TRUE),
              band2_or_3_unique_patients_rolling_12M = sum(band2_or_3_unique_patients_rolling_12M, na.rm = TRUE),
              band1_urgent_unique_patients_rolling_12M = sum(band1_urgent_unique_patients_rolling_12M, na.rm = TRUE),
              band_other_unique_patients_rolling_12M = sum(band_other_unique_patients_rolling_12M, na.rm = TRUE)) 
  
  data <- data_to_print %>%
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
                            band == "band_other_unique_patients_rolling_12M" ~ "Other band")) %>%
    mutate(month = as.Date(month))
  
  data$band <- factor(data$band,
                      levels = c(
                        "Any band",
                        "Band 1",
                        "Band 2 or 3",
                        "Urgent band 1",
                        "Other band"
                      ))
  
  #set everything as indext of April 2019
  if(asIndex == TRUE){
    
    april2018 <- data %>%
      filter(month == as.Date("2018-04-01")) %>%
      select(-month,
             april_total_unique_patients = total_unique_patients)
    
    data <- data %>%
      left_join(april2018, by = "band") %>%
      mutate(total_unique_patients = total_unique_patients * 100 / april_total_unique_patients)
    
    title <- "Total number of unique patients seen over a rolling 12 month period by band \nas a percentage of April 2018 figures"
    ylab <- "Percentage of April 2018 unique patients"
    
  }else{
    title <- "Total number of unique patients seen over a rolling 12 month period by band"
    ylab <- "Unique patients"
  }
  
  
  if(plotChart == TRUE & get_perc == FALSE){
    #plot code
    ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month,
                    y = total_unique_patients,
                    colour = band)) +
      geom_point(aes(x = month,
                     y = total_unique_patients,
                     colour = band)) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      scale_colour_manual(values = get_colour_palette()) +
      labs(title = title,
           x = "12 month rolling period end date",
           y = ylab,
           subtitle = subtitle,
           caption = chartCaption
      ) +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90))
    #theme(legend.position = "bottom")
  }else if(plotChart == FALSE & get_perc == FALSE){
    data_to_print <- data_to_print %>%
      mutate(month = as.Date(month)) %>%
      rename(`12 month period end` = month,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Any band unique patients rolling 12 month` = unique_patients_rolling_12M,
             `Band 1 unique patients rolling 12 month` = band1_unique_patients_rolling_12M,
             `Band 2 or 3 unique patients rolling 12 month` = band2_or_3_unique_patients_rolling_12M,
             `Urgent band 1 unique patients rolling 12 month` = band1_urgent_unique_patients_rolling_12M,
             `Other unique patients rolling 12 month` = band_other_unique_patients_rolling_12M
      )
  }else{
    #percentage of pre-covid levels
    data <- data %>%
      filter(band == "Any band") %>%
      arrange(month)
    
    round(data$total_unique_patients[nrow(data)] * 100 / data$total_unique_patients[1])
  }
  
}
