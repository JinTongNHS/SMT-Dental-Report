################################################################################
plot_patient_recalls_facet <- function(data = dental_recalls_STP_2018_22,
                                       ICB_lookup = STP_ICB_lookup_codes,
                                       treatment_band = "Band_1",
                                       level = "National",
                                       region_STP_name = NULL,
                                       calendar_data = UDA_calendar_data,
                                       plotChart = TRUE){
  
  # #add a region column to the data
  # region_STP_lookup <- calendar_data %>%
  #   select(commissioner_name, region_name) %>%
  #   distinct()
  
  #sort out STP ICB issue
  ICB_lookup <- ICB_lookup %>%
    rename(commissioner_name = commissioner_name_STP)
  
  #for output when plotChart == FALSE
  data_output <- data %>%
    left_join(ICB_lookup, by = "commissioner_name") %>%
    mutate(commissioner_name = commissioner_name_ICB) %>%
    select(-commissioner_name_ICB) #%>%
  #left_join(region_STP_lookup, by = "commissioner_name")
  
  data <- clean_dental_recalls(data_output)
  
  data <- data %>% 
    filter(Band == treatment_band) 
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
    
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
    
  }
  
  data <- data %>%
    group_by(Months_Last_Visit, patient_group, financial_year) %>%
    summarise(forms = sum(forms, na.rm = TRUE))
  
  data$Months_Last_Visit <- factor(data$Months_Last_Visit,
                                   levels = c("1 Month",
                                              "2 Months",
                                              "3 Months",
                                              "4 Months",
                                              "5 Months",
                                              "6 Months",
                                              "7 Months",
                                              "8 Months",
                                              "9 Months",
                                              "10 Months",
                                              "11 Months",
                                              "12 Months",
                                              "12-18 Months",
                                              "19-24 Months",
                                              "No Previous Visit"
                                   ))
  
  if(plotChart == TRUE){
    ggplot(data) +
      geom_col(aes(x = Months_Last_Visit,
                   y = forms,
                   fill = patient_group),
               position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
      scale_fill_manual(values = c("royalblue2", "limegreen")) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(title = "Adult and Child Re-attendance Intervals",
           subtitle = paste0(str_replace(treatment_band, "_", " "), "\n", region_STP_name),
           x = "Months since last visit",
           y = "Total form count",
           fill = "") +
      facet_wrap(vars(financial_year),
                 #scales = "free_y",
                 nrow = 2)
  }else{
    
    data_output <- data_output %>%
      select(`Financial Year` = financial_year,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Patient Group` = patient_group,
             `Months Since Last Visit` = Months_Last_Visit,
             `Band 1` = Band_1,
             `Band 2` = Band_2,
             `Band 3` = Band_3,
             Other,
             Urgent
      )
    
  }
  
}