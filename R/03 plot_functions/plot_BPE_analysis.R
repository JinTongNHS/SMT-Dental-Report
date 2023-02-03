
plot_BPE_no_oral_health_risk <- function(data = BPE_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         ICB_lookup = STP_ICB_lookup_codes){
  
  data[is.na(data)] <- 0
  
  #join in standardised ICB column
  ICB_lookup <- ICB_lookup %>%
    select(commissioner_name = commissioner_name_ICB,
           Latest_Commissioner_Code = commissioner_ONS_boundary_code_ICB)
  
  data <- data %>%
    left_join(ICB_lookup, by = "Latest_Commissioner_Code")
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>%
      filter(Latest.Region.Description == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  ###filtering and selecting the right columns
  ########str(data)
  
  data_org <- data %>%
    group_by(Latest.Region.Description) %>%
    rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
             "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>%
    
    # filter (Year_Month == max(data$Year_Month)) %>%
    
    mutate (percentage_low_risk_recall_interval_less_than_1_year	 = 
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year) /
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)) %>%
    
    mutate (percentage_complete_forms	 = 
              as.numeric (Forms_with_Highest_BPE_Sextant_Score) /
              as.numeric (Total.Form.Count)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices
            = round (mean(percentage_low_risk_recall_interval_less_than_1_year, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_risk_score_complete_average_over_practices = 
              round (mean(percentage_complete_forms, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen	 = 
              round (as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year))  /
                       as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_score_complete_average_over_patients_seen = 
              round (as.numeric (sum (Forms_with_Highest_BPE_Sextant_Score)) /
                       as.numeric (sum (Total.Form.Count)), digits = 2)) 
  
  
  
  ##### plot the bar graph
  
  plot_1 <- data_org %>% group_by(Latest.Region.Description, Year_Month ) %>%
    summarise(average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices=
                formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices), digits =  0),
              average_risk_score_complete_average_over_practices =
                formattable::percent (mean (average_risk_score_complete_average_over_practices), digits =  0),
              average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen =
                formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen), digits =  0),
              average_percentage_low_risk_score_complete_average_over_patients_seen = 
                formattable::percent  (mean (average_percentage_low_risk_score_complete_average_over_patients_seen),digits =  0) )
  
  
  plot_2 <- plot_1 %>% pivot_longer(cols=c('average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices', 
                                           'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen',
                                           'average_risk_score_complete_average_over_practices',
                                           'average_percentage_low_risk_score_complete_average_over_patients_seen'),
                                    names_to='Description',
                                    values_to= 'Percentage')
  
  plot_2 [plot_2 == 'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices'] <-
    "Average % of completed record with no indication of oral health risk"
  
  
  plot_2 [plot_2 == 'average_risk_score_complete_average_over_practices'] <-
    "Average % of completed risk scores"
  
  filtered_plot_2_1 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk",
                                                        "Average % of completed risk scores"))
  
  p1<- ggplot(filtered_plot_2_1, aes(x = Latest.Region.Description, 
                                     y = Percentage, fill= Description)) +
    geom_bar(stat="identity", position = "dodge") +
    ###facet_grid(cols = vars(Year_Month), labeller = label_value) +
    geom_text(aes(label = Percentage), 
              check_overlap = TRUE,
              colour = "black", size= 3.5,
              position = position_dodge(width = 1), vjust= -0.5) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Oral Health Risk Assessment",
         subtitle = paste(subtitle, " - December 2022"),
         x = "Region",
         fill = "") +
    scale_fill_manual(values = c("#009E73", "#F0E442"),
                      label = c("% of FP17 forms indicating low oral health risk \nwith recall intervalls < 12 months", 
                                "% of FP17s with complete BPE score data")) +
    theme_bw() +
    theme(legend.position="bottom") 
  
  p1
  
}


