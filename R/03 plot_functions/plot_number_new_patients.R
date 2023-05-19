##data <- new_vs_return_data

plot_number_new_patients <- function(data = new_vs_return_data,
                                     level = "National",
                                     region_STP_name = NULL, 
                                     scheduled_data = UDA_scheduled_data,
                                     as_percentage = FALSE,
                                     plotChart = TRUE){
  
  data <- data %>%
    mutate(month = as.Date(month))
  
  
  #filter for region or STP if specified
  if(level == "National"){
    
    subtitle <- "England"
    
  }else if(level == "Regional"){
    
    data <- data %>%
      filter(Latest_Region_Description == region_STP_name)
    
    subtitle <- region_STP_name
    
  }else{
    
    data <- data %>%
      filter("Latest Commissioner_Name" == region_STP_name)
    
    subtitle <- region_STP_name
    
  }
  
  
  scheduled_data <- scheduled_data %>%
    filter(month >= as.Date("2022-04-01"))
  
  contractors_number_england <- scheduled_data %>%
    dplyr::count(month, name = "total_number_of_contractors_submitted_FP17")
  
  
  ##new_vs_return_data
  
  new_vs_return_data$Contract_Number<- as.numeric(new_vs_return_data$Contract_Number)
  
  new_vs_return_data$month <- as.Date(new_vs_return_data$month)
  
  patients_number_summary <- new_vs_return_data %>% group_by(month) %>% 
    dplyr::summarise (new_adult_patients_number = sum(Adult, na.rm = TRUE),
                      new_child_patients_number = sum(Child, na.rm = TRUE)) 
  
  patients_number_summary_longer <- patients_number_summary %>% 
    pivot_longer (cols = c('new_adult_patients_number', 'new_child_patients_number'),
                  names_to='category',
                  values_to='number')
  
  if(as_percentage == FALSE){
    
    if(plotChart == TRUE){
      new_patient_line_chart_england <- ggplot(patients_number_summary_longer, 
                                               aes(x = month, y = number,
                                                   group =category )) +
        geom_line(aes(color=category),
                  linewidth = 1.2)+
        geom_point(aes(color=category),
                   size = 3) + 
        scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
        scale_y_continuous(breaks = scales::breaks_pretty(),
                           labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        expand_limits(y=0) +
        geom_text(aes(label = number), vjust=-.5)+
        theme_bw() +
        ##theme(legend.position="bottom") +
        theme(legend.position="top") +
        labs(title = "New Patient (no previous in last 24 months or before) Numbers",
             x = "Month",
             y = "Number of new patients",
             subtitle = subtitle,
             caption = "All contracts are included in this graph") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      new_patient_line_chart_england
    }else{
      patients_number_summary_longer
    }
    
    
  }
  else{
    
    ## providers percentage 
    
    new_patients_provider_number <- data %>% 
      group_by (month) %>%
      summarise(served_new_child_patients = sum(!is.na(Child)),
                served_new_adult_patients = sum(!is.na(Adult)))
      
    
    contractors_number_all_patients <- scheduled_data %>%
      group_by(month) %>%
      count(month, name = "total_number_of_contractors_submitted_FP17")
    
    all_provider <- left_join(new_patients_provider_number, 
                              contractors_number_all_patients, by = c("month" = "month"))
    
    
    all_provider <- all_provider %>% 
      mutate(percent_served_new_child_patients = formattable::percent (served_new_child_patients /
                                                                         total_number_of_contractors_submitted_FP17, digits =0),
             percent_served_new_adult_patients = formattable::percent (served_new_adult_patients /
                                                                         total_number_of_contractors_submitted_FP17, digits =0))
    
    all_provider_longer <- all_provider %>% 
      pivot_longer(cols = c("percent_served_new_child_patients", 
                            "percent_served_new_adult_patients"),
                   names_to ='category',
                   values_to='Percentage') %>% 
      select(month, category, Percentage) %>%
      mutate(month = as.Date(month))
    
    if(plotChart == TRUE){
      provider_chart <- ggplot(all_provider_longer, 
                               aes(x = month, y = Percentage,
                                   group =category )) +
        geom_line(aes(color=category),
                  linewidth = 1.5)+
        geom_point(aes(color=category),
                   size = 3) + 
        scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
        scale_y_continuous(breaks = seq(0, 1.1, 0.2),
                           limits = c(0, 1.1),
                           labels = scales::percent_format(accuracy = 1)) +
        expand_limits(y=0) +
        geom_text(aes(label = Percentage), vjust=-.5)+
        theme_bw() +
        ##theme(legend.position="bottom") +
        theme(legend.position="top") +
        labs(title = "% of Providers Served New Patients",
             x = "Month",
             y = "Number of new patients",
             subtitle = subtitle,
             caption = "All contracts are included in this graph") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      provider_chart 
    }else{
      
      all_provider_longer
    }
    
    
    
  }
}

# plot_number_new_patients <- function(data = new_vs_return_data,
#                                      level = "National",
#                                      region_STP_name = NULL, 
#                                      scheduled_data = UDA_scheduled_data,
#                                      as_percentage = FALSE){
#   
#   # #fix column names
#   # data <- data %>%
#   #   rename(Latest_Commissioner_Name = `Latest Commissioner_Name`)
#   # 
#   #This will need to be updated each month of new data
#   #A better suggestion would be to standardise the column names in the source data
#   # colnames(data) <- c(
#     # "Contract_Number", "Latest_Provider_Name", "Latest_Region",       
#     # "Latest_Region_Description", "Latest_Commissioner_Code",  "Latest_Commissioner_Name" ,
#     # "adult_2022-04",             "adult_2022-05" ,            "adult_2022-06",  
#     # "adult_2022-07" ,            "adult_2022-08"  ,           "adult_2022-09" ,    
#     # "adult_2022-10"  ,           "adult_2022-11"   ,          "adult_2022-12"  ,      
#     # "adult_2023-01"   ,          "adult_2023-02"    ,         "child_2022-04"   ,  
#     # "child_2022-05"    ,         "child_2022-06"     ,        "child_2022-07"    ,  
#     # "child_2022-08"     ,        "child_2022-09"      ,       "child_2022-10"     ,  
#     # "child_2022-11"      ,       "child_2022-12"       ,      "child_2023-01"      ,      
#     # "child_2023-02")
#     # 
#   
#   #filter for region or STP if specified
#   if(level == "National"){
# 
#     subtitle <- "England"
# 
#   }else if(level == "Regional"){
# 
#     data <- data %>%
#       filter(Latest_Region_Description == region_STP_name)
# 
#     scheduled_data <- scheduled_data %>%
#       filter(month >= as.Date("2022-04-01")) %>%
#       filter(region_name == region_STP_name)
# 
#     subtitle <- region_STP_name
# 
#   }else{
# 
#     data <- data %>%
#       filter(Latest_Commissioner_Name == region_STP_name)
# 
#     scheduled_data <- scheduled_data %>%
#       filter(month >= as.Date("2022-04-01")) %>%
#       filter(commissioner_name == region_STP_name)
# 
#     subtitle <- region_STP_name
# 
#   }
# 
# 
#   contractors_number_all_patients <- scheduled_data %>%
#     group_by(month) %>%
#     count(month, name = "total_number_of_contractors_submitted_FP17")
# 
#   contractors_number_england <- scheduled_data %>%
#     count(month, name = "total_number_of_contractors_submitted_FP17")
# 
#   data_contract_level <- data %>%
#     mutate(Contract_Number = as.numeric(Contract_Number)) %>%
#     pivot_longer(cols = -c(Contract_Number, Latest_Provider_Name, Latest_Region,
#                            Latest_Region_Description, Latest_Commissioner_Code,  Latest_Commissioner_Name),
#                  names_to = c('.value', 'month'),
#                  names_sep="_") %>%
#     mutate(year_x = substr(month, 1, 4),
#            month_x = substr(month, 6, 7)) %>%
#     mutate(month = as.Date(paste0(year_x, "-", month_x, "-01")))
# 
#   data <- data_contract_level %>%
#     group_by(month) %>%
#     summarise(new_adult_patients_number = sum(adult, na.rm = TRUE),
#               new_child_patients_number = sum(child, na.rm = TRUE)) %>%
#     pivot_longer(cols = c('new_adult_patients_number', 'new_child_patients_number'),
#                  names_to='category', values_to='number')
# 
# 
#   if(as_percentage == FALSE){
# 
#     #plot code
#     ggplot(data,
#            aes(x = month,
#                y = number,
#                group =category )) +
#       geom_line(aes(color=category),
#                 size = 1.2)+
#       geom_point(aes(color=category),
#                  size = 3) +
#       scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
#       scale_y_continuous(breaks = scales::breaks_pretty(),
#                          labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
#       scale_colour_manual(values = c("steelblue", "coral"), labels = c("New adult patients", "New child patients")) +
#       expand_limits(y=0) +
#       geom_text(aes(label = number), vjust=-.5)+
#       theme_bw() +
#       ##theme(legend.position="bottom") +
#       theme(legend.position="top") +
#       labs(title = "Number of New Patients (no previous in last 24 months or before)",
#            x = "Month",
#            y = "Number of new patients",
#            colour = "Category",
#            subtitle = subtitle) +
#       theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))
# 
#   }
#   else{
# 
#     #number of contracts with new patients
#     new_patients_provider_number <- data_contract_level %>%
#       group_by (month) %>%
#       summarise(served_new_child_patients = sum(!is.na(child)),
#                 served_new_adult_patients = sum(!is.na(adult)))
# 
#     all_provider <- left_join(new_patients_provider_number,
#                               contractors_number_all_patients, by = c("month"))
# 
# 
#     all_provider <- all_provider %>%
#       mutate(percent_served_new_child_patients = formattable::percent (served_new_child_patients /
#                total_number_of_contractors_submitted_FP17, digits =0),
#              percent_served_new_adult_patients = formattable::percent (served_new_adult_patients /
#                total_number_of_contractors_submitted_FP17, digits =0))
# 
#     all_provider_longer <- all_provider %>%
#       pivot_longer(cols = c("percent_served_new_child_patients",
#                             "percent_served_new_adult_patients"),
#                    names_to ='category',
#                    values_to='Percentage') %>%
#       select (month, category, Percentage) %>%
#       mutate(month = as.Date(month))
# 
#     #plot code
#     ggplot(all_provider_longer,
#            aes(x = month, y = Percentage,
#                group =category )) +
#       geom_line(aes(color=category),
#                 size = 1.2)+
#       geom_point(aes(color=category),
#                  size = 3) +
#       scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
      # scale_y_continuous(breaks = seq(0, 1.1, 0.2),
      #                    limits = c(0, 1.1),
      #                    labels = scales::percent_format(accuracy = 1)) +
#       expand_limits(y=0) +
#       geom_text(aes(label = Percentage), vjust=-.5)+
#       theme_bw() +
#       ##theme(legend.position="bottom") +
#       theme(legend.position="top") +
#       labs(title = "Percentage of Providers Serving New Patients",
#            x = "Month",
#            subtitle = subtitle) +
#       theme(axis.text.x = element_text(angle = 90, vjust=-0.0001))
# 
#   }
# 
# }