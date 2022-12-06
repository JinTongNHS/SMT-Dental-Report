#DCP_data_October22 <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 

plot_DCP_analysis <- function(data = UDA_scheduled_data,
                              dcp_data = DCP_data_October22,
                              data_month = max(data$month),
                              UDA_or_FP17 = "UDA",
                              level = "National",
                              region_STP_name = NULL){
  
  #filters for specified month of data - make sure to match with the DPC data available
  data <- data %>%
    filter(month == data_month)
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    
    dcp_data <- dcp_data %>%
      filter(Region == region_STP_name)
    
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    
    dcp_data <- dcp_data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  delivery_total <-  data %>% 
    group_by(month) %>%
    dplyr::summarise( total_FP17 = sum(general_FP17s, na.rm = TRUE),
                      total_B1 = sum(UDA_band_1, na.rm = TRUE),
                      total_B2 = sum(UDA_band_2, na.rm = TRUE),
                      total_B3 = sum(UDA_band_3, na.rm = TRUE),
                      total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate(DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    select(DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)

  pull_dcp <- dcp_data %>%
    group_by(DCP_description) %>%
    summarise(total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
               total_B1 = sum(Band_1._UDA, na.rm = TRUE),
               total_B2 = sum(Band_2._UDA, na.rm = TRUE),
               total_B3 = sum(Band_3._UDA, na.rm = TRUE),
               total_urgent = sum(Urgent_UDA, na.rm = TRUE))
  
  dcp_summary <- pull_dcp %>%
    mutate(DCP_description = replace(DCP_description, DCP_description == "Hygienist", "Hygienist_assisted"),
           DCP_description = replace(DCP_description, DCP_description == "Therapist", "Therapist_assisted"),
           DCP_description = replace(DCP_description, DCP_description == "Dental Nurse", "Dental_Nurse_assisted")) %>%
    mutate_if(is.numeric, round, 0)
  
  joined <- rbind(dcp_summary, delivery_total) %>% mutate_if(is.numeric, round, 2)
  
  test_UDA <- group_by(joined, DCP_description) %>%
    mutate (Assisted_FP17_Percentage = 
              formattable::percent (total_FP17 / joined %>% with(total_FP17[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2))%>%
    mutate (Assisted_B1_Percentage = 
              formattable::percent (total_B1 / joined %>% with(total_B1[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
    mutate (Assisted_B2_Percentage = 
              formattable::percent (total_B2  / joined %>% with(total_B2[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
    mutate (Assisted_B3_Percentage = 
              formattable::percent (total_B3  / joined %>% with(total_B3[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
    mutate (Assisted_Urgent_Percentage = 
              formattable::percent (total_urgent   / joined %>% with(total_urgent[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
    select(DCP_description, Assisted_B1_Percentage, Assisted_B2_Percentage, Assisted_B3_Percentage, Assisted_Urgent_Percentage)
  
  
  if(UDA_or_FP17 == "UDA"){
    
    data_long_UDA <- pivot_longer(test_UDA, cols = -DCP_description, 
                                  names_to = 'UDAs_in_Bands', 
                                  values_to = 'UDAs')
    
    filtered_data_UDA <- data_long_UDA %>%
      filter(DCP_description %in% c("Dental_Nurse_assisted", "Hygienist_assisted", "Therapist_assisted")) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))
    
    UDA_plot <-  
      ggplot(filtered_data_UDA, aes(fill = UDAs_in_Bands , y = UDAs, x = DCP_description)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = UDAs),
                colour = "black",
                position = position_dodge(width = 1), vjust=-0.25) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("#009E73", "#F0E442", "#D55E00", "#CC79A7"),
                        labels = c("Band 1", "Band 2", "Band 3", "Urgent")) +
      theme(legend.position="bottom") +
      theme_bw() +
      labs(title = "Percentage of total UDAs delivered which had DCP* assistance by band",
           subtitle = paste0(subtitle, ": ",format(data_month, "%b-%Y")),
           x = "DCP description",
           y = "Percentage of total UDAs delivered",
           fill = "Band",
           caption = "*Dental Care Practitioner")
    
    UDA_plot
  }else if(UDA_or_FP17 == "FP17"){
    
    test_FP17 <- group_by(joined, DCP_description) %>%
      mutate (Assisted_FP17_Percentage =
                formattable::percent (total_FP17 / joined %>% with(total_FP17[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
      select(DCP_description, Assisted_FP17_Percentage)



    data_long_FP17 <- pivot_longer(test_FP17, cols = -DCP_description,
                                   names_to = 'UDAs_in_Bands',
                                   values_to = 'UDAs')

    filtered_data_FP17 <- data_long_FP17 %>%
      filter(DCP_description %in% c("Dental_Nurse_assisted", "Hygienist_assisted", "Therapist_assisted")) %>%
      mutate(DCP_description = str_replace_all(DCP_description, "_", " "))

    FP17_plot <-  
      ggplot(filtered_data_FP17, aes(y = UDAs, x = DCP_description)) +
      geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
      geom_text(aes(label = UDAs),
                colour = "black",
                position = position_dodge(width = 1), vjust=-0.25) +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position="bottom") +
      theme_bw() +
      labs(title = "Percentage of total Courses of Treatment (CoTs) delivered which had DCP* assistance",
           subtitle = paste0(subtitle, ": ", format(data_month, "%b-%Y")),
           x = "DCP description",
           y = "Percentage of total CoTs delivered",
           caption = "*Dental Care Practitioner")

    FP17_plot
  }
  
}