#CAT 1s only non FDs
################################################################################
plot_historic_UDA_UOA_delivery_treatment_month <- function(data = UDA_treatment_month_non_FD,
                                                           scheduled_data = UDA_scheduled_data, 
                                                           historic_data = historical_UDA_scheduled_data,
                                                           contractor_cats = contractor_categories,
                                                           UDAorUOA = "UDA",
                                                           level = "National",
                                                           region_STP_name = NULL,
                                                           cat_1s_only = TRUE,
                                                           plotChart = TRUE, 
                                                           all_regions_and_STPs = FALSE,
                                                           include_historic = TRUE){
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name )
    
    scheduled_data <- scheduled_data %>%
      filter(region_name == region_STP_name )
    
    historic_data <- historic_data %>%
      filter(region_name == region_STP_name )
    
    subtitle <- region_STP_name
    
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    
    scheduled_data <- scheduled_data %>%
      filter(commissioner_name == region_STP_name )
    
    historic_data <- historic_data %>%
      filter(commissioner_name == region_STP_name )
    
    subtitle <- region_STP_name
    
  }else{
    subtitle <- "England"
  }
  
 #get historic data into single lines for each financial year##################
  #bind in historic data if required
  scheduled_data <- scheduled_data %>%
    select(month, contract_number, commissioner_name, region_name,
           annual_contracted_UDA, UDA_delivered)
  
  historic_data <- historic_data %>%
    mutate(month = as.Date(month)) %>%
    filter(month < as.Date("2023-04-01")) %>%
    filter(!is.na(annual_contracted_UDA)) %>% #filters out contracts with no data on contracted UDAs
    select(month, contract_number, commissioner_name, region_name,
           annual_contracted_UDA,
           UDA_delivered) #%>%
  #filter(month >= as.Date("2019-04-01"))
  
  historic_data <- scheduled_data %>%
    bind_rows(historic_data) %>%
    left_join(contractor_cats, by = "contract_number")
  
  if(cat_1s_only == TRUE){
    
    historic_data <- historic_data  %>%
      filter(category_sub_type == "CDR CAT 1")
    
    data <- data %>%
      left_join(contractor_cats, by = "contract_number") %>%
      filter(category_sub_type == "CDR CAT 1")
    
    subtitle_addition <- "CAT 1 contracts only"
  }else{
    
    subtitle_addition <- "All contracts"
  }
  
  #get historic data ready
    historic_data <- historic_data  %>%
      mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
                                        month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
                                        month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
                                        month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
                                        month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
                                        month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                                        month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
                                        month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
                                        month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
      group_by(financial_year) %>%
      summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
                full_year_contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE) / 12) %>%
      mutate(perc_UDA_deliverd = full_year_UDA_delivery * 100 / full_year_contracted_UDAs)
      
  

  #get treatment month data ready
    data <- data %>%
      group_by(month) %>%
      summarise(UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
                contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE) / 12) %>%
      mutate(perc_UDA_deliverd = UDA_delivery * 100 / contracted_UDAs) %>%
      add_row(month = seq(as.Date("2016-04-01"), as.Date("2023-03-01"), lubridate::month(1))) %>%
      mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
                                        month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
                                        month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
                                        month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
                                        month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
                                        month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                                        month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
                                        month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
                                        month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
      left_join(historic_data, by = "financial_year") %>%
      mutate(perc_UDA_delivered = if_else(!is.na(perc_UDA_deliverd.x), perc_UDA_deliverd.x, perc_UDA_deliverd.y)) 
    
    data <- data %>%
      mutate(year_type = if_else(financial_year == max(data$financial_year), "Current Financial Year", "Previous Financial Years (Averages)")) %>%
      #get one ro wper prev financial year
      filter((substr(month, 6, 7) == "04" & substr(month, 9, 10) == "01")| year_type == "Current Financial Year")
      
    
    data$year_type <- factor(data$year_type, levels = c("Previous Financial Years (Averages)",
                                                        "Current Financial Year"))

    ggplot() +
      geom_line(data = data, aes(x = month,
                          y = perc_UDA_delivered),
                colour = "steelblue") +
      geom_point(#data = filter(data, year_type == "Current Financial Year"),
        data = data,
                 aes(x = month,
                     y = perc_UDA_delivered),
        colour = "steelblue") +
      geom_text(data = filter(data, year_type == "Previous Financial Years (Averages)"),
                aes(x = month + 120,
                    y = perc_UDA_delivered - 4,
                    label = financial_year),
                size = 3) +
      geom_text(data = filter(data, year_type == "Previous Financial Years (Averages)"),
                aes(x = month + 120,
                    y = perc_UDA_delivered + 4,
                    label = paste0(round(perc_UDA_delivered), "%")),
                size = 3) +
      geom_text(data = filter(data, year_type == "Current Financial Year"),
                aes(x = month,
                    y = perc_UDA_delivered + 4,
                    label = paste0(round(perc_UDA_delivered), "%")),
                size = 3) +
      facet_wrap(vars(year_type),
                 scales = "free_x") +
      theme_bw() +
      #scale_x_date(breaks = scales::breaks_width(1)) +
      scale_y_continuous(limits = c(0, max(c(data$perc_UDA_delivered, 95), na.rm = T) + 5),
                         breaks = seq(0, max(c(data$perc_UDA_delivered, 95), na.rm = T) + 5, 10)#,
                         #labels = scales::percent_format(accuracy = 1)
                         ) +
      labs(title = "Treatment month data: Monthly percentage of usual annual contracted UDAs \ndelivered scaled up to 12 months*, Foundation Dentists Excluded",
           x = "Time period",
           y = "Percentage of contracted UDAs delivered",
           subtitle = paste0(subtitle, ": ", subtitle_addition),
           caption = "*The monthly delivery figure is scaled up by 12 in order to compare to the annual contracted UDA figure.") +
      theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
      )) 
    
}