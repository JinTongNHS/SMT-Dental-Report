################################################################################
plot_urgent_form_submissions <- function(data = UDA_scheduled_data, 
                                         historic_data = historical_UDA_scheduled_data, 
                                         level = "National",
                                         region_STP_name = "Cheshire and Merseyside STP",
                                         plotChart = TRUE,
                                         all_regions_and_STPs = FALSE,
                                         remove_prototypes = FALSE){
  
  #avoid standard form on axes
  options(scipen = 100)

  #toggle subtitle
  if(level == "Regional"){
    #filter for region or STP
    data <- data %>%
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>%
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    #filter for region or STP
    data <- data %>%
      filter(commissioner_name == region_STP_name )
    historic_data <- historic_data %>%
      filter(commissioner_name == region_STP_name )
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #get data in the right format
  data <- get_banded_COTs_data(data, historic_data = historic_data, remove_prototypes = remove_prototypes, all_regions_and_STPs = all_regions_and_STPs)
  data <- data %>%
    mutate(date = as.Date(month)) %>%
    mutate(year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
                            month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
                            month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
                            month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
                            month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
                            month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23"
    ))
  
  #get caption right for prototypes being reomved or not
  if(remove_prototypes == TRUE){
    
    chartCaption <- "*EXCLUDING contracts with annual contracted UDA < 100. EXCLUDING prototype contracts up until April 2022."
  }else{
    
    chartCaption <- "*Contracts with annual contracted UDAs < 100 and prototype contracts are INCLUDED in this graph."
  }
  
  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    geom_line(aes(x = factor(lubridate::month(date, label=TRUE, abbr=TRUE),
                             levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
                  y = urgent,
                  group = factor(year),
                  colour = factor(year)),
              size = 1) +
    geom_point(aes(x = factor(lubridate::month(date, label=TRUE, abbr=TRUE),
                              levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
                   y = urgent,
                   group = factor(year),
                   colour = factor(year))) +
    scale_y_continuous(breaks = scales::breaks_pretty(),
                       limits = c(0, max(data$urgent))) +
    scale_colour_manual(values = get_colour_palette()) +
    labs(title = "Urgent treatment form submissions",
         x = "Month",
         y = "Number of urgent FP17* forms submitted",
         colour = "Financial year",
         subtitle = subtitle,
         caption = chartCaption)
  
  if(plotChart == TRUE){
    p
  }else{
    
    new_cols <- c(Month = "month",
                  `Region Name` = "region_name",
                  `Commissioner Name` = "commissioner_name",
                  `Urgent forms` = "urgent",
                  `Financial year` = "year")
    
    data %>% 
      mutate(month = as.Date(month)) %>%
      select(-c(band1, band2, band3, other, date)) %>%
      rename(any_of(new_cols))
  }
  
  
}