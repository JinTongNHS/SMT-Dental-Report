get_ICB_delivery <- function(data = UDA_scheduled_data,
                             historic_data = historical_UDA_scheduled_data){

  data <- data %>%
    mutate(year = case_when(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                            month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23"))

  data <- bind_rows(data, historical_UDA_scheduled_data)

  data_monthly <- data %>%
    group_by(year, month, commissioner_ods_code_icb) %>%
    summarise(monthly_UDs_delivered = sum(UDA_delivered, na.rm = TRUE),
              total_annual_contracted_UDA = sum(annual_contracted_UDA, na.rm = TRUE))


  data_yearly <- data_monthly %>%
    group_by(year, commissioner_ods_code_icb) %>%
    summarise(yearly_UDAs_delivered = sum(monthly_UDs_delivered, na.rm = TRUE),
              annual_contracted_UDAs = mean(total_annual_contracted_UDA, na.rm = TRUE)) %>%
    filter(year %in% c("2018/19", "2019/20", "2020/21", "2021/22")) %>%
    mutate(year = case_when(year == "2018/19" ~ "2018-19",
                            year == "2019/20" ~ "2019-20",
                            year == "2020/21" ~ "2020-21",
                            year == "2021/22" ~ "2021-22"))

}