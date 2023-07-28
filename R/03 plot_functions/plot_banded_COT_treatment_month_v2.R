# ################################################################################
# plot_banded_CoT_facet <- function(data = UDA_treatment_month_non_FD, 
#                             historic_data = historical_UDA_scheduled_data, 
#                             level = "National",
#                             region_STP_name = NULL,
#                             plotChart = TRUE, 
#                             all_regions_and_STPs = FALSE,
#                             asIndex = FALSE,
#                             remove_prototypes = FALSE){
#   
#   #avoid standard form on axes
#   options(scipen = 100)
#   
#   #toggle subtitle
#   if(level == "Regional"){
#     #filter for region or STP
#     data <- data %>% 
#       filter(region_name == region_STP_name )
#     historic_data <- historic_data %>% 
#       filter(region_name == region_STP_name )
#     subtitle <- region_STP_name 
#   }else if(level == "STP"){
#     #filter for region or STP
#     data <- data %>% 
#       filter(commissioner_name == region_STP_name )
#     historic_data <- historic_data %>% 
#       filter(commissioner_name == region_STP_name )
#     subtitle <- region_STP_name
#   }else{
#     subtitle <- "England"
#   }
#   
#   #get data into the right format
#   data_to_print <- get_banded_COTs_data(data, historic_data = historic_data, remove_prototypes = remove_prototypes, all_regions_and_STPs = all_regions_and_STPs)
#   data <- reshape2::melt(data_to_print, id.vars = "month")
#   data <- data %>%
#     mutate(month = as.Date(month)) %>%
#     rename(band = variable, CoTs = value)
#   
#   #set everything as index of 2019
#   if(asIndex == TRUE){
#     
#     data <- data %>%
#       mutate(month_number = substr(month, 6, 7),
#              year_number = substr(month, 1, 4))
#     
#     data2019 <- data %>%
#       filter(year_number == "2019") %>%
#       select(month_number,
#              band,
#              CoTs_2019 = CoTs)
#     
#     data <- data %>%
#       left_join(data2019, by = c("band", "month_number")) %>%
#       mutate(CoTs = CoTs * 100 / CoTs_2019)
#     
#     title <- "Banded Courses of Treatment as a Percentage of corresponding month of 2019 Delivery"
#     ylab <- "Percentage of 2019 FP17* forms submitted"
#     
#   }else{
#     title <- "Banded Courses of Treatment"
#     ylab <- "Number of FP17* forms submitted"
#   }
#   
#   #get caption right for prototypes being removed or not
#   if(remove_prototypes == TRUE){
#     
#     chartCaption <- "*EXCLUDING contracts with annual contracted UDA < 100. EXCLUDING prototype contracts up until April 2022."
#   }else{
#     
#     chartCaption <- "*Contracts with annual contracted UDAs < 100 and prototype contracts are INCLUDED in this graph."
#   }
#   
#   if(plotChart == TRUE){
#     
#     
#     data <- data %>%
#       mutate(year_type = if_else(financial_year == max(data$financial_year), "Current Financial Year", "Previous Financial Years (Averages)")) %>%
#       #get one ro wper prev financial year
#       filter((substr(month, 6, 7) == "04" & substr(month, 9, 10) == "01")| year_type == "Current Financial Year")
#     
#     data$year_type <- factor(data$year_type, levels = c("Previous Financial Years (Averages)",
#                                                         "Current Financial Year"))
#     
#     
#     #plot code
#     ggplot(data) +
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 90)) +
#       geom_line(aes(x = month,
#                     y = CoTs,
#                     colour = band),
#                 size = 1) +
#       geom_point(aes(x = month,
#                      y = CoTs,
#                      colour = band),
#                  size = 1) +
#       scale_x_date(date_breaks = "1 month",
#                    date_labels = "%b-%y") +
#       scale_colour_manual(labels = c("Band 1", "Band 2", "Band 3", "Other", "Urgent"),
#                           values = get_colour_palette()#c("coral3",
#                           # "orange",
#                           # "yellow3",
#                           # "green",
#                           # "blue")
#       ) +
#       scale_y_continuous(breaks = scales::breaks_pretty(),
#                          labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
#       labs(title = title,
#            x = "Month",
#            y = ylab,
#            colour = "Band",
#            subtitle = subtitle,
#            caption = chartCaption) +
#       facet_wrap(vars(year_type),
#                  scales = "free_x") +
#       theme_bw() +
#       #scale_x_date(breaks = scales::breaks_width(1)) +
#       scale_y_continuous(limits = c(0, max(c(data$variable, 95), na.rm = T) + 5),
#                          breaks = seq(0, max(c(data$variable, 95), na.rm = T) + 5, 10))#,
#     #labels = scales::percent_format(accuracy = 1)
#     
#     
#   }else{
#     data_to_print %>%
#       mutate(month = as.Date(month)) %>%
#       rename(Month = month,
#              `Region Name` = region_name,
#              `Commissioner Name` = commissioner_name,
#              `Band 1` = band1,
#              `Band 2` = band2,
#              `Band 3` = band3,
#              `Other` = other,
#              `Urgent` = urgent)
#   }
#   
# }