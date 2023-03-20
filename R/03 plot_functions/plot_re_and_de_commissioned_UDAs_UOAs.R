################################################################################
plot_re_and_de_commissioned_UDAs_UOAs <- function(data = contract_handbacks,
                                                  level = "National",
                                                  region_STP_name = NULL,
                                                  all_regions_and_STPs = FALSE,
                                                  plotChart = TRUE){
  
  data <- data %>%
    mutate(region_name = case_when(region_name == "East" ~ "East of England",
                                   region_name == "NE & Yorkshire" ~ "North East and Yorkshire",
                                   TRUE ~ region_name)) %>%
    mutate(decommissioned_UDAs = -decommissioned_UDAs,
           decommissioned_UOAs = -decommissioned_UOAs) %>%
    select(time_period, region_name, decommissioned_UDAs, recommissioned_UDAs, decommissioned_UOAs, recommissioned_UOAs) %>%
    pivot_longer(cols = c("decommissioned_UDAs", "recommissioned_UDAs", "decommissioned_UOAs", "recommissioned_UOAs"),
                 names_to = "commission_type",
                 values_to = "value")
  
  #set defualt subtitle
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  totals <- data %>%
    group_by(time_period) %>%
    summarise(value = sum(value, na.rm = TRUE))
  
  #group data
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(time_period, region_name, commission_type)
  }else{
    data <- data %>% 
      group_by(time_period, commission_type)
  }
  
  data <- data %>%
    summarise(value = sum(value, na.rm = TRUE))
  
  data$time_period <- factor(data$time_period,
                             levels = c("Apr - Jun 2021",
                                        "Jul - Sep 2021",
                                        "October 2021",
                                        "November 2021",
                                        "December 2021",
                                        "January 2022",
                                        "February 2022",
                                        "March 2022",
                                        "April 2022",
                                        "May 2022",
                                        "June 2022",
                                        "July 2022",
                                        "August 2022",
                                        "September 2022",
                                        "October 2022",
                                        "November 2022",
                                        "December 2022",
                                        "January 2023",
                                        "February 2023",
                                        "March 2023",
                                        "April 2023",
                                        "May 2023",
                                        "June 2023",
                                        "July 2023",
                                        "August 2023",
                                        "September 2023",
                                        "October 2023",
                                        "November 2023",
                                        "December 2023"))
  
  
  if(plotChart == TRUE){
    ggplot() +
      geom_bar(data = data,
               aes(x = time_period,
                   y = value,
                   fill = commission_type),
               position = "dodge",
               stat = "identity") +
      geom_hline(yintercept = 0,
                 colour = "black") +
      # geom_text(data = data,
      #           aes(x = time_period,
      #               y = value + (value/10),
      #               label = value ,
      #               group = commission_type),
      #           position = position_dodge(width = 1),
      #           size = 3) +
      scale_fill_manual(values = c("firebrick3", "coral", "springgreen4", "palegreen1"), 
                        labels = c("De-commissioned UDAs", "De-commissioned UOAs",
                                   "Re-commissioned UDAs", "Re-commissioned UOAs")) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      theme_bw() +
      labs(title = "De-commissioned and re-commissoned UDAs and UOAs \nas a result of practice closures",
           subtitle = subtitle,
           x = "Time period",
           y = "Number of de-commissioned or \nre-commissioned UDAs and UOAs",
           fill = "") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }else{
    data <- data %>%
      rename(`Time period` = time_period,
             `Region name` = region_name,
             `Re- or de- commission` = commission_type,
             `UDAs or UOAs re- or de- commissioned` = value)
  }
  
}
