################################################################################
plot_111_referrals <- function(data = dental_data_111,
                               plotChart = TRUE){
  
  data <- data %>% 
    mutate(month = as.Date(month)) %>%
    group_by(month) %>%
    summarise(monthly_case_volume = sum(monthly_case_volume))
  
  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month, 
                  y = monthly_case_volume), 
              colour = "steelblue", 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, 
                                    max(data$monthly_case_volume, na.rm = T) + 10000,
                                    10000),
                       limits = c(0, max(data$monthly_case_volume))) +
    labs(title = "111 call volume recommended towards dental services", 
         x = "Month",
         y = "Call volume", 
         subtitle = "England"#,
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
    data
  }
  
}
