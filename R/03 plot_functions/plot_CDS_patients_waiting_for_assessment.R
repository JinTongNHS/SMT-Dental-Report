################################################################################
plot_CDS_patients_waiting_for_assessment <- function(data = CDS_data,
                                                     calendar_data = UDA_calendar_data,
                                                     level = "National",
                                                     region_STP_name = NULL,
                                                     all_regions_and_STPs = FALSE){
  
  data <- data %>% 
    rename(region_name = NHSREGION,
           quarter = Month)
  
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
    select(quarter,
           region_name,
           routine_num_patients_waiting_for_first_assessment_adult, 
           routine_num_patients_waiting_for_first_assessment_child,
           GA_num_patients_waiting_for_first_assessment_adult,
           GA_num_patients_waiting_for_first_assessment_child,
           SS_num_patients_waiting_for_first_assessment_adult,
           SS_num_patients_waiting_for_first_assessment_child
    ) %>%
    mutate(routine_num_patients_waiting_for_first_assessment_adult = as.numeric(routine_num_patients_waiting_for_first_assessment_adult), 
           routine_num_patients_waiting_for_first_assessment_child = as.numeric(routine_num_patients_waiting_for_first_assessment_child),
           GA_num_patients_waiting_for_first_assessment_adult = as.numeric(GA_num_patients_waiting_for_first_assessment_adult),
           GA_num_patients_waiting_for_first_assessment_child = as.numeric(GA_num_patients_waiting_for_first_assessment_child),
           SS_num_patients_waiting_for_first_assessment_adult = as.numeric(SS_num_patients_waiting_for_first_assessment_adult),
           SS_num_patients_waiting_for_first_assessment_child = as.numeric(SS_num_patients_waiting_for_first_assessment_child)
    ) 
  
  
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(quarter, region_name)
  }else{
    data <- data %>%
      group_by(quarter)
  }
  
  data <- data %>%
    summarise(routine_adult = mean(routine_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
              routine_child = mean(routine_num_patients_waiting_for_first_assessment_child, na.rm = TRUE),
              GA_adult = mean(GA_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
              GA_child = mean(GA_num_patients_waiting_for_first_assessment_child, na.rm = TRUE),
              SS_adult = mean(SS_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
              SS_child = mean(SS_num_patients_waiting_for_first_assessment_child, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = c(routine_adult,
                          routine_child,
                          GA_adult,
                          GA_child,
                          SS_adult,
                          SS_child),
                 names_to = "variable",
                 values_to = "patients_waiting") %>%
    mutate(patient_group = sub(".*\\_", "", variable),
           appointment_type = sub("\\_.*", "", variable))
  
  
  
  ggplot(data) +
    geom_line(aes(x = quarter,
                  y = patients_waiting,
                  colour = appointment_type)) +
    geom_point(aes(x = quarter,
                   y = patients_waiting,
                   colour = appointment_type
    )) +
    ggrepel::geom_text_repel(aes(x = quarter,
                                 y = patients_waiting,
                                 colour = appointment_type,
                                 label = round(patients_waiting)),
                             size=3, box.padding = unit(0.2, "lines")
    ) +
    theme_bw() +
    labs(title = "Quarterly mean number of patients per contract waiting for first assessment",
         subtitle = subtitle,
         x = "Quarter start date",
         y = "Mean number of paitings waiting",
         colour = "") +
    # scale_x_date(date_breaks = "1 month",
    #              date_labels = "%b-%y") +
    facet_wrap(vars(patient_group),
               #scales = "free_y",
               nrow = 1) +
    theme(axis.text.x = element_text(angle = 90))
  
  
  
}