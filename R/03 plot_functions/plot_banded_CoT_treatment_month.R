#CAT 1s only non FDs
################################################################################
plot_historic_band_treatment_month_me <- function(data = UDA_treatment_month_non_FD,
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
  ##colnames(scheduled_data)
  scheduled_data <- scheduled_data %>%
    filter(month < as.Date("2023-04-01")) %>% 
    select(month, contract_number, commissioner_name, region_name,
           annual_contracted_UDA, UDA_delivered,  
           FP17s_band_1, FP17s_band_2, FP17s_band_3, FP17s_band_urgent,
           FP17s_band_other)
  
  historic_data <- historic_data %>%
    mutate(month = as.Date(month)) %>%
    filter(month < as.Date("2023-04-01")) %>%
    filter(!is.na(annual_contracted_UDA)) %>% #filters out contracts with no data on contracted UDAs
    select(month, contract_number, commissioner_name, region_name,
           annual_contracted_UDA, UDA_delivered,  
           FP17s_band_1, FP17s_band_2, FP17s_band_3, FP17s_band_urgent,
           FP17s_band_other) #%>%
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
    summarise(full_year_FP17s_band_1 = sum(FP17s_band_1, na.rm = TRUE),
              full_year_FP17s_band_2 = sum(FP17s_band_2, na.rm = TRUE),
              full_year_FP17s_band_3 = sum(FP17s_band_3, na.rm = TRUE),
              full_year_FP17s_band_urgent = sum(FP17s_band_urgent, na.rm = TRUE),
              full_year_FP17s_band_other = sum(FP17s_band_other, na.rm = TRUE)) ##%>%
    # mutate(perc_UDA_deliverd = full_year_UDA_delivery * 100 / full_year_contracted_UDAs)
    # 
  
  
  #get treatment month data ready
  #data = UDA_treatment_month_non_FD
  data <- data %>%
    group_by(month) %>%
    summarise(full_year_FP17s_band_1 = sum(FP17s_band_1, na.rm = TRUE),
      full_year_FP17s_band_2 = sum(FP17s_band_2, na.rm = TRUE),
      full_year_FP17s_band_3 = sum(FP17s_band_3, na.rm = TRUE),
      full_year_FP17s_band_urgent = sum(FP17s_band_urgent, na.rm = TRUE),
      full_year_FP17s_band_other = sum(FP17s_band_other, na.rm = TRUE))%>%
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
    left_join(historic_data, by = "financial_year")
   ## mutate(perc_UDA_delivered = if_else(!is.na(perc_UDA_deliverd.x), perc_UDA_deliverd.x, perc_UDA_deliverd.y)) 
  
  data <- data %>%
    mutate(year_type = if_else(financial_year == max(data$financial_year), 
                               "Current Financial Year", 
                               "Previous Financial Years")) %>%
    #get one ro wper prev financial year
    filter((substr(month, 6, 7) == "04" & substr(month, 9, 10) == "01")| year_type == "Current Financial Year") %>% 
    mutate (FP17s_band_1 = if_else(!is.na(full_year_FP17s_band_1.x), full_year_FP17s_band_1.x, full_year_FP17s_band_1.y),
            FP17s_band_2 = if_else(!is.na(full_year_FP17s_band_2.x), full_year_FP17s_band_2.x, full_year_FP17s_band_2.y),
            FP17s_band_3 = if_else(!is.na(full_year_FP17s_band_3.x), full_year_FP17s_band_3.x, full_year_FP17s_band_2.y),
            FP17s_band_urgent = if_else(!is.na(full_year_FP17s_band_urgent.x), full_year_FP17s_band_urgent.x, full_year_FP17s_band_urgent.y),
            FP17s_band_other = if_else(!is.na(full_year_FP17s_band_other.x), full_year_FP17s_band_other.x, full_year_FP17s_band_other.y)) %>% 
    select(month, financial_year, year_type, FP17s_band_1, FP17s_band_2, FP17s_band_3, FP17s_band_urgent, FP17s_band_other)

  
  data_longer<- data %>% 
    pivot_longer(cols = starts_with("FP17s"),
                        names_to = "band",                              
                        # names_prefix = "wk_FP17s",
                        values_to = "CoTs",
                        values_drop_na = TRUE)
  data<-data_longer %>% 
    mutate(year_type = ifelse(is.na(year_type), "Previous Financial Years", year_type))
  
  data$year_type
  
  
  #plot code
  ggplot() +
    geom_line(data = data, aes(x = month,
                               y = CoTs, 
                               colour = band)) +
    geom_point(#data = filter(data, year_type == "Current Financial Year"),
      data = data,
      aes(x = month,
          y = CoTs,
          colour = band)) +
    geom_text(data = filter(data, year_type == "Previous Financial Years"),
              aes(x = month + 120,
                  y = CoTs - 4,
                  label = financial_year),
              size = 3) +
    # geom_text(data = filter(data, year_type == "Previous Financial Years"),
    #           aes(x = month + 120,
    #               y = CoTs + 4,
    #               label = CoTs),
    #           size = 3) +
    # geom_text(data = filter(data, year_type == "Current Financial Year"),
    #           aes(x = month,
    #               y = CoTs + 4,
    #               label = CoTs),
    #           size = 3) +
    facet_wrap(vars(year_type),
               scales = "free_x") +
    theme_bw() +
    #scale_x_date(breaks = scales::breaks_width(1)) +
    # scale_y_continuous(limits = c(0, max(c(data$CoTs, 95), na.rm = T) + 5),
    #                    breaks = seq(0, max(c(data$CoTs, 95), na.rm = T) + 5, 10)#,
    #                    #labels = scales::percent_format(accuracy = 1)
    # ) +
    labs(title = "Banded Course of Treatment, Foundation Dentists Excluded",
         x = "Time period",
         y = "Number of CoTs in Bands",
         subtitle = paste0(subtitle, ": ", subtitle_addition),
         caption = "*XXXXXXXXXXXXX") +
    theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
    )) 
  
  
}