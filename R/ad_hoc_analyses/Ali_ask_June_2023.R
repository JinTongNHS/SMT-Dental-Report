data <- bind_rows(UDA_scheduled_data, historical_UDA_scheduled_data)

data <- get_delivery_data(data = data, contractor_level = TRUE, remove_prototypes = FALSE)

data <- data %>%
  ungroup() %>%
  mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
                                    month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
                                    month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
                                    month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
                                    month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
                                    month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                                    month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
                                    month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24")) %>%
  group_by(financial_year, commissioner_name, contract_number) %>%
  summarise(UDA_delivered = sum(monthly_UDA_UOAs_delivered, na.rm = TRUE),
            annual_contracted_UDAs = sum(annual_contracted_UDA_UOA / 12, na.rm = TRUE))


data2 <- data %>%
  filter(annual_contracted_UDAs > 100) %>%
  mutate(perc_UDA_delivery = UDA_delivered * 100 / annual_contracted_UDAs) %>%
  mutate(performance_band = case_when(perc_UDA_delivery >= 0 & perc_UDA_delivery <10 ~ "0% <= x < 10%",
                                      perc_UDA_delivery >= 10 & perc_UDA_delivery <20 ~ "10% <= x < 20%",
                                      perc_UDA_delivery >= 20 & perc_UDA_delivery <30 ~ "20% <= x < 30%",
                                      perc_UDA_delivery >= 30 & perc_UDA_delivery <40 ~ "30% <= x < 40%",
                                      perc_UDA_delivery >= 40 & perc_UDA_delivery <50 ~ "40% <= x < 50%",
                                      perc_UDA_delivery >= 50 & perc_UDA_delivery <60 ~ "50% <= x < 60%",
                                      perc_UDA_delivery >= 60 & perc_UDA_delivery <70 ~ "60% <= x < 70%",
                                      perc_UDA_delivery >= 70 & perc_UDA_delivery <80 ~ "70% <= x < 80%",
                                      perc_UDA_delivery >= 80 & perc_UDA_delivery <90 ~ "80% <= x < 90%",
                                      perc_UDA_delivery >= 90 & perc_UDA_delivery <100 ~ "90% <= x < 100%",
                                      perc_UDA_delivery >= 100 ~ "x >= 100%",
                                      is.na(perc_UDA_delivery) ~ "UDA delivery data \ninvalid or not given",
                                      is.infinite(perc_UDA_delivery) ~ "UDA delivery data \ninvalid or not given",
                                      perc_UDA_delivery < 0 ~ "UDA delivery data \ninvalid or not given"))

data2$performance_band <- factor(data2$performance_band, levels = c("0% <= x < 10%",
                                                                    "10% <= x < 20%",
                                                                    "20% <= x < 30%",
                                                                    "30% <= x < 40%",
                                                                    "40% <= x < 50%",
                                                                    "50% <= x < 60%",
                                                                    "60% <= x < 70%",
                                                                    "70% <= x < 80%",
                                                                    "80% <= x < 90%",
                                                                    "90% <= x < 100%",
                                                                    "x >= 100%",
                                                                    "UDA delivery data \ninvalid or not given"))

data_summary <- data2 %>%
  group_by(financial_year, performance_band) %>%
  count()