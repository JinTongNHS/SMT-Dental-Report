##need to uncomment

# #get historic data ready
# historic_data <- historical_UDA_scheduled_data  %>%
#   mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
#                                     month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
#                                     month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
#                                     month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
#                                     month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
#                                     month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
#                                     month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
#                                     month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
#                                     month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
#   group_by(financial_year) %>%
#   summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE),
#             full_year_contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE) / 12) %>%
#   mutate(perc_UDA_deliverd = full_year_UDA_delivery * 100 / full_year_contracted_UDAs) %>% 
#   select(financial_year, full_year_UDA_delivery)
# 
# 
# 
# UDA_scheduled_data_time_series<- UDA_scheduled_data %>% 
#   mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
#                                     month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
#                                     month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
#                                     month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
#                                     month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
#                                     month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
#                                     month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
#                                     month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
#                                     month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
#   ##filter(financial_year !="2023/24") %>% 
#   group_by(financial_year) %>% 
#   summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE))
# 
# a<- max(UDA_scheduled_data_time_series$financial_year)
# 
# year_end_data_for_plot <- historic_data %>% 
#   rbind(UDA_scheduled_data_time_series) %>% 
#   mutate(year_type = if_else(financial_year == a, 
#                              "Current Financial Year", 
#                              "Previous Financial Years")) %>%
#   filter((substr(month, 6, 7) == "04" & substr(month, 9, 10) == "01")| year_type == "Current Financial Year")
# 
# 
# 
# historic_data_1 <- historical_UDA_scheduled_data  %>%
#   mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
#                                     month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
#                                     month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
#                                     month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
#                                     month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
#                                     month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
#                                     month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
#                                     month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
#                                     month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
#   group_by(financial_year) %>%
#   summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE))
# 
# 
# 
# 
# 
# historic_data_2 <- scheduled_data  %>%
#   mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
#                                     month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
#                                     month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
#                                     month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
#                                     month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
#                                     month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
#                                     month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
#                                     month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
#                                     month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
#   group_by(financial_year) %>%
#   summarise(full_year_UDA_delivery = sum(UDA_delivered, na.rm = TRUE))
# 
# historic_data <- historic_data_1 %>% 
#   rbind(historic_data_2)
# 
# #get treatment month data ready
# data <- UDA_treatment_month_non_FD %>%
#   group_by(month) %>%
#   summarise(UDA_delivery = sum(UDA_delivered, na.rm = TRUE)) %>% 
#             ##contracted_UDAs = sum(annual_contracted_UDA, na.rm = TRUE) / 12) %>%
#   ##mutate(perc_UDA_deliverd = UDA_delivery * 100 / contracted_UDAs) %>%
#   add_row(month = seq(as.Date("2016-04-01"), as.Date("2023-03-01"), lubridate::month(1))) %>%
#   mutate(financial_year = case_when(month >= as.Date("2016-04-01") & month < as.Date("2017-04-01") ~ "2016/17",
#                                     month >= as.Date("2017-04-01") & month < as.Date("2018-04-01") ~ "2017/18",
#                                     month >= as.Date("2018-04-01") & month < as.Date("2019-04-01") ~ "2018/19",
#                                     month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
#                                     month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
#                                     month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
#                                     month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23",
#                                     month >= as.Date("2023-04-01") & month < as.Date("2024-04-01") ~ "2023/24",
#                                     month >= as.Date("2024-04-01") & month < as.Date("2025-04-01") ~ "2024/25")) %>%
#   left_join(historic_data, by = "financial_year") %>%
#   mutate(plot_UDA_delivery = if_else(!is.na(UDA_delivery), UDA_delivery , full_year_UDA_delivery)) 
# 
# 
# data <- data %>%
#   mutate(year_type = if_else(financial_year == max(data$financial_year), 
#                              "Current Financial Year", 
#                              "Previous Financial Years")) %>%
#   #get one ro wper prev financial year
#   filter((substr(month, 6, 7) == "04" & substr(month, 9, 10) == "01")| year_type == "Current Financial Year")
# 
# 
# data$year_type <- factor(data$year_type, levels = c("Previous Financial Years",
#                                                     "Current Financial Year"))
# 
# 
# subtitle_addition <- "All contracts"
# 
# ggplot() +
#   geom_line(data = data, aes(x = month,
#                              y = plot_UDA_delivery),
#             colour = "steelblue") +
#   geom_point(#data = filter(data, year_type == "Current Financial Year"),
#     data = data,
#     aes(x = month,
#         y = plot_UDA_delivery),
#     colour = "steelblue") +
#   geom_text(data = filter(data, year_type == "Previous Financial Years"),
#             aes(x = month + 120,
#                 y = plot_UDA_delivery - 4,
#                 label = financial_year),
#             size = 3,vjust=-1) +
#   # geom_text(data = filter(data, year_type == "Previous Financial Years"),
#   #           aes(x = month + 120,
#   #               y = plot_UDA_delivery + 4,
#   #               label = plot_UDA_delivery),
#   #           size = 3) +
#   geom_text(data = filter(data, year_type == "Current Financial Year"),
#             aes(x = month,
#                 y = plot_UDA_delivery + 4,
#                 label = plot_UDA_delivery,
#             size = 2,vjust=-1)) +
#   facet_wrap(vars(year_type),
#              scales = "free_x") +
#   theme_bw() +
#   #scale_x_date(breaks = scales::breaks_width(1)) +
#   # scale_y_continuous(limits = c(0, max(c(data$plot_UDA_delivery, 95), na.rm = T) + 5),
#   #                    breaks = seq(0, max(c(data$plot_UDA_delivery, 95), na.rm = T) + 5, 10)#,
#   #                    #labels = scales::percent_format(accuracy = 1)
#   # ) +
#   
#   labs(title = "Time series of banded Courses of Treatment*",
#        x = "Time period",
#        y = "UDAs delivered",
#        subtitle = paste0(subtitle, ": ", subtitle_addition),
#        caption = "* Foundation Dentists Excluded") +
#   theme(axis.text.x = element_text(angle = 90, vjust=-0.0001
#   )) 
