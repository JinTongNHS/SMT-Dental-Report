################################################################################
plot_practice_closures <- function(data = contract_handbacks,
                                   level = "National",
                                   region_STP_name = NULL,
                                   all_regions_and_STPs = FALSE,
                                   plotChart = TRUE){
  
  data <- data %>%
    mutate(region_name = case_when(region_name == "East" ~ "East of England",
                                   region_name == "NE & Yorkshire" ~ "North East and Yorkshire",
                                   TRUE ~ region_name)) %>%
    select(time_period, region_name, commissioner_termination_notices, contractor_termination_notices,
           retirement_of_contractor, death_of_contractor, other) %>%
    pivot_longer(cols = c("commissioner_termination_notices", "contractor_termination_notices",
                          "retirement_of_contractor", "death_of_contractor", "other"),
                 names_to = "reason",
                 values_to = "closures"
    )
  
  #set defualt subtitle
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  totals <- data %>%
    group_by(time_period) %>%
    summarise(closures = sum(closures, na.rm = TRUE))
  
  #group data
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(time_period, region_name, reason)
  }else{
    data <- data %>% 
      group_by(time_period, reason)
  }
  
  data <- data %>%
    summarise(closures = sum(closures, na.rm = TRUE))
  
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
                                        "October 2022"))
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#CC79A7")
  
  if(plotChart == TRUE){
    ggplot() +
      geom_bar(data = data,
               aes(x = time_period,
                   y = closures,
                   fill = reason),
               position = "stack",
               stat = "identity") +
      geom_text(data = totals,
                aes(x = time_period,
                    y = closures + (closures/20) + 0.3,
                    label = closures),
                size = 3) +
      scale_fill_manual(values = cbPalette, labels = c("Commissioner termination notices", "Contractor termination notices",
                                                       "Death of contractor", "Other reason", "Retirement of contractor")) +
      theme_bw() +
      labs(title = "Practice Closures",
           subtitle = subtitle,
           x = "Time period",
           y = "Number of closures",
           fill = "Closure reason") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }else{
    data <- data %>%
      rename(`Time period` = time_period,
             `Region Name` = region_name,
             `Closure reason` = reason,
             `Number of closures` = closures)
  }
  
}