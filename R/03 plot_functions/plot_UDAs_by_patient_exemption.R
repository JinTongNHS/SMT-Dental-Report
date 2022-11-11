# patient_exemption_data <- pull_patient_exemption_data()
# patient_exemption_data <- clean_patient_exemption_data()

################################################################################
plot_UDAs_by_patient_exemption <- function(data = patient_exemption_data,
                                           region_filter = "England"){
  
  #colour blind palette
  colorBlindGrey8   <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                         "darkorchid2", "blue", "chartreuse")
  
  data <- data %>% 
    filter(Regions == region_filter) %>%
    group_by(Treatment_Charge_Band, patient_type) %>%
    summarise(number_uda)
  
  #make subtitle say National when England is in the region_filter
  if(region_filter == "England"){
    subtitle <- "National"
  }else{
    subtitle <- region_filter
  }
  
  ggplot(data = data, 
         aes (x = Treatment_Charge_Band, 
              y = number_uda,
              group = patient_type )) +
    geom_col(aes(fill = patient_type)) +
    geom_text(aes(label = number_uda), 
              position = position_stack(vjust = 0.3),
              size = 3)  + 
    theme(legend.position="top")  + 
    #scale_y_continuous(breaks = seq(0, 78000000, by = 50000)) +
    scale_fill_manual(values = colorBlindGrey8, labels = c("Child", "Adult exempt", "Adult non exempt")) +
    labs(title = "UDAs delivered by Patient Charge Status",
         subtitle = paste(subtitle, "2021/22"),
         x = "Treatment charge band",
         y = "Number of UDAs",
         fill = "Patient charge status") +
    theme_bw()

}