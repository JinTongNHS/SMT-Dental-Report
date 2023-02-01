################################################################################  
plot_breakdown_111_referrals <- function(data = dental_data_111,
                                         plotChart = TRUE){
  
  data <- data %>% 
    mutate(disposition_text = if_else(disposition_text == "Attend Emergency Dental Treatment Centre within 4",
                                      "Attend Emergency Dental Treatment Centre within 4hrs",
                                      disposition_text)) %>%
    mutate(month = as.Date(month)) 
  
  #get lines in the right order
  data$disposition_text <- factor(data$disposition_text,
                                  levels = c("Refer to Dental Treatment Centre within 1 hour",
                                             "Refer to Dental Treatment Centre within 4 hours",
                                             "To Speak to a Dental Service within 1 hour",
                                             "To Speak to a Dental Service within 2 hours",
                                             "Speak to a Dental Service within 2 hours",
                                             "To Speak to a Dental Service within 6 hours",
                                             "To Speak to a Dental Service within 12 hours",
                                             "To Speak to a Dental Service within 24 hours",
                                             "To Speak to a Dental Practice within 7 days",
                                             "Contact Orthodontist next working day"
                                  ))
  
  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month, 
                  y = monthly_case_volume,
                  colour = str_wrap(disposition_text, 35)), 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    # scale_y_continuous(breaks = seq(47000, 
    #                                 max(data$monthly_case_volume, na.rm = T) + 10000,
    #                                 10000)) +
    labs(title = "111 call volume recommended towards dental services", 
         x = "Month",
         y = "Call volume", 
         subtitle = "England",
         colour = "Disposition"#,
         #caption = "*services include: "
    ) #+
  # annotate(geom = "text", 
  #          x = data$month, 
  #          y = data$monthly_case_volume + 1, 
  #          label = data$monthly_case_volume, 
  #          size = 3) 
  
  if(plotChart == TRUE){
    p
  }else{
    data %>%
      select(`Month` = month,
             `Disposition` = disposition_text,
             `Monthly case volume` = monthly_case_volume)
  }
  
}