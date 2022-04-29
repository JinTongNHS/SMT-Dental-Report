get_pre_release_table1a <- function(data = UDA_scheduled_data,
                                    remove_prototypes = T){
  
  if(remove_prototypes){
    #create not in function•
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  data <- data %>%
    mutate(quarter = case_when(month >= as.Date("2021-04-01") & month <= as.Date("2021-06-01") ~ "Q1",
                               month >= as.Date("2021-07-01") & month <= as.Date("2021-09-01") ~ "Q2",
                               month >= as.Date("2021-10-01") & month <= as.Date("2021-12-01") ~ "Q3",
                               month >= as.Date("2022-01-01") & month <= as.Date("2022-03-01") ~ "Q4",)) %>%
    group_by(quarter) %>%
    summarise(UDA_delivered = round(sum(UDA_delivered, na.rm = T)/1000),
              UDA_band_1 = round(sum(UDA_band_1, na.rm = T)/1000),
              UDA_band_2 = round(sum(UDA_band_2, na.rm = T)/1000),
              UDA_band_3 = round(sum(UDA_band_3, na.rm = T)/1000),
              UDA_urgent = round(sum(UDA_urgent, na.rm = T)/1000),
              UDA_other = round(sum(UDA_other, na.rm = T)/1000),
              general_FP17s = round(sum(general_FP17s, na.rm = T)/1000),
              FP17s_band_1 = round(sum(FP17s_band_1, na.rm = T)/1000),
              FP17s_band_2 = round(sum(FP17s_band_2, na.rm = T)/1000),
              FP17s_band_3 = round(sum(FP17s_band_3, na.rm = T)/1000),
              FP17s_band_urgent = round(sum(FP17s_band_urgent, na.rm = T)/1000),
              FP17s_band_other = round(sum(FP17s_band_other, na.rm = T)/1000)
              )
  
}


get_pre_release_table1a_calendar <- function(scheduled_data = UDA_scheduled_data,
                                             data = UDA_calendar_data,
                                             remove_prototypes = T){
  
  
  #join in contracted UDAs from scheduled data
  contracted_UDAs <- scheduled_data %>%
    select(month, contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(contracted_UDAs, by = c("month", "contract_number"))
  
  if(remove_prototypes){
    #create not in function•
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  data <- data %>%
    mutate(quarter = case_when(month >= as.Date("2021-04-01") & month <= as.Date("2021-06-01") ~ "Q1",
                               month >= as.Date("2021-07-01") & month <= as.Date("2021-09-01") ~ "Q2",
                               month >= as.Date("2021-10-01") & month <= as.Date("2021-12-01") ~ "Q3",
                               month >= as.Date("2022-01-01") & month <= as.Date("2022-03-01") ~ "Q4",)) %>%
    group_by(quarter) %>%
    summarise(UDA_total = round(sum(UDA_total, na.rm = T)/1000),
              UDA_band_1_total = round(sum(UDA_band_1_total, na.rm = T)/1000),
              UDA_band_2_total = round(sum(UDA_band_2_total, na.rm = T)/1000),
              UDA_band_3_total = round(sum(UDA_band_3_total, na.rm = T)/1000),
              UDA_urgent_total = round(sum(UDA_urgent_total, na.rm = T)/1000),
              UDA_other_total = round(sum(UDA_other_total, na.rm = T)/1000),
              total_FP17s = round(sum(total_FP17s, na.rm = T)/1000),
              total_band_1_FP17s = round(sum(total_band_1_FP17s, na.rm = T)/1000),
              total_band_2_FP17s = round(sum(total_band_2_FP17s, na.rm = T)/1000),
              total_band_3_FP17s = round(sum(total_band_3_FP17s, na.rm = T)/1000),
              total_urgent_FP17s = round(sum(total_urgent_FP17s, na.rm = T)/1000),
              total_other_FP17s = round(sum(total_other_FP17s, na.rm = T)/1000)
    )
  
}




get_pre_release_table1a_prev_year  <- function(data = UDA_scheduled_data){
  
  
  data <- data %>%
    mutate(quarter = case_when(month >= as.Date("2021-04-01") & month <= as.Date("2021-06-01") ~ "Q1",
                               month >= as.Date("2021-07-01") & month <= as.Date("2021-09-01") ~ "Q2",
                               month >= as.Date("2021-10-01") & month <= as.Date("2021-12-01") ~ "Q3",
                               month >= as.Date("2022-01-01") & month <= as.Date("2022-03-01") ~ "Q4",)) %>%
    group_by(quarter) %>%
    summarise(UDA_delivered_prev_year = sum(UDA_delivered_prev_year, na.rm = T),
              UDA_band_1_prev_year = sum(UDA_band_1_prev_year, na.rm = T),
              UDA_band_2_prev_year = sum(UDA_band_2_prev_year, na.rm = T),
              UDA_band_3_prev_year = sum(UDA_band_3_prev_year, na.rm = T),
              UDA_urgent_prev_year = sum(UDA_urgent_prev_year, na.rm = T),
              UDA_other_prev_year = sum(UDA_other_prev_year, na.rm = T),
              general_FP17s_prev_year = sum(general_FP17s_prev_year, na.rm = T),
              FP17s_band_1_prev_year = sum(FP17s_band_1_prev_year, na.rm = T),
              FP17s_band_2_prev_year = sum(FP17s_band_2_prev_year, na.rm = T),
              FP17s_band3_prev_year = sum(FP17s_band3_prev_year, na.rm = T),
              FP17s_urgent_prev_year = sum(FP17s_urgent_prev_year, na.rm = T),
              FP17s_other_prev_year = sum(FP17s_other_prev_year, na.rm = T)
    )
  
}



get_pre_release_table1a_prev_year_historical  <- function(data = historical_UDA_scheduled_data){
  
  
  data <- data %>%
    # mutate(quarter = case_when(month >= as.Date("2020-04-01") & month <= as.Date("2020-06-01") ~ "Q1",
    #                            month >= as.Date("2020-07-01") & month <= as.Date("2020-09-01") ~ "Q2",
    #                            month >= as.Date("2020-10-01") & month <= as.Date("2020-12-01") ~ "Q3",
    #                            month >= as.Date("2021-01-01") & month <= as.Date("2021-03-01") ~ "Q4",)) %>%
    mutate(quarter = case_when(month >= as.Date("2019-04-01") & month <= as.Date("2019-06-01") ~ "Q1",
                               month >= as.Date("2019-07-01") & month <= as.Date("2019-09-01") ~ "Q2",
                               month >= as.Date("2019-10-01") & month <= as.Date("2019-12-01") ~ "Q3",
                               month >= as.Date("2020-01-01") & month <= as.Date("2020-03-01") ~ "Q4",)) %>%
    filter(!is.na(quarter)) %>%
    #group_by(quarter) %>%
    summarise(total_UDAs = round(sum(total_UDAs, na.rm = T)/1000),
              band1_UDA = round(sum(band1_UDA, na.rm = T)/1000),
              band2_UDA = round(sum(band2_UDA, na.rm = T)/1000),
              band3_UDA = round(sum(band3_UDA, na.rm = T)/1000),
              urgent_UDA = round(sum(urgent_UDA, na.rm = T)/1000),
              other_UDA = round(sum(other_UDA, na.rm = T)/1000),
              total_FP17s = round(sum(total_FP17s, na.rm = T)/1000),
              band1_FP17 = round(sum(band1_FP17, na.rm = T)/1000),
              band2_FP17 = round(sum(band2_FP17, na.rm = T)/1000),
              band3_FP17 = round(sum(band3_FP17, na.rm = T)/1000),
              urgent_FP17 = round(sum(urgent_FP17, na.rm = T)/1000),
              other_FP17 = round(sum(other_FP17, na.rm = T)/1000)
    )
  
}