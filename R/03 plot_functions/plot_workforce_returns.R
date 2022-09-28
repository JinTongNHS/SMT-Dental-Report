################################################################################
plot_workforce_returns <- function(data = dental_workforce_returns,
                                   level = "National",
                                   region_STP_name = NULL,
                                   calendar_data = UDA_calendar_data){
  
  data <- clean_workforce_returns(data)
  
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(region_STP_lookup, by = "commissioner_name")
  
  subtitle <- "England"
  
  #only filter for regional or STP level
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
    group_by(date) %>%
    summarise(total_workforce_returns_due = sum(total_workforce_returns_due, na.rm = TRUE),
              total_workforce_returns_submitted = sum(total_workforce_returns_submitted, na.rm = TRUE),
              workforce_returns = sum(workforce_returns, na.rm = TRUE)
    ) %>%
    mutate(monthly_workforce_returns_due = total_workforce_returns_due / 12) %>%
    select(date, monthly_workforce_returns_due, workforce_returns) %>%
    pivot_longer(cols = c("monthly_workforce_returns_due", "workforce_returns"))
  
  ggplot(data) +
    geom_line(aes(x = date,
                  y = value,
                  colour = name)) +
    theme_bw() +
    scale_colour_manual(values = c("coral", "steelblue"),
                        labels = c("Monthly returns due", "Monthly returns submitted")) +
    labs(title = "Total monthly workforce returns",
         subtitle = subtitle,
         x = "Month",
         y = "Number of returns",
         colour = "") +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) 
}
