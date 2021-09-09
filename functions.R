library(tidyverse)
#plot functions for SMT report and more



#function to plot first chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either dental_data_combined_calendar or orthodontic_data_combined_calendar
plot_UDA_UOA_to_target <- function(data = dental_data_combined_calendar, UDAorUOA = "UDA"){
  
  if(UDAorUOA == "UDA"){
    septemberTarget <- 60
    title <- "Monthly % of Apr-Sept target (60%) UDAs delivered"
    ylab <- "% of target UDAs delivered"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_into_slide4_format_calendar(data, remove_prototypes = T)
    
  }else{
    septemberTarget <- 80
    title <- "Monthly % of Apr-Sept target (80%) UOAs delivered"
    ylab <- "% of target UOAs delivered"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_into_slide6_format_calendar(data, remove_prototypes = F)
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / target_UDA_UOAs_delivered_by_sept) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1))
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(perc_of_UDA_UOA_target_delivered = if_else(is.na(perc_of_UDA_UOA_target_delivered)
                                                  , 0, 
                                                  perc_of_UDA_UOA_target_delivered)) 
  
  #plot code
  ggplot(data_to_plot, 
         aes(x = month, 
             y = perc_of_UDA_UOA_target_delivered)) +
    geom_bar(stat = "identity", 
             fill = barCol, 
             width = 10) +
    theme_bw() +
    geom_hline(yintercept = 16.7, 
               colour = "blue", 
               linetype = "dashed") +
    annotate(geom = "text", 
             x = as.Date("2021-09-01"),
             y = 17.7, 
             label = "16.7% target", 
             size = 3) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_of_UDA_UOA_target_delivered + 1, 
             label = round(data$perc_of_UDA_UOA_target_delivered, 2), 
             size = 3) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    labs(title = title, 
         x = "Month", 
         y = ylab)
}

################################################################################
#function to plot second chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either dental_data_combined_calendar or orthodontic_data_combined_calendar
plot_cumulative_UDA_UOA_to_target <- function(data = dental_data_combined_calendar, UDAorUOA = "UDA"){
  
  if(UDAorUOA == "UDA"){
    septemberTarget <- 60 
    title <- "Year to date monthly % of Apr-Sept target (60%) UDAs delivered"
    ylab <- "Cumulative % of target UDAs delivered \nfrom Apr-21"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_into_slide4_format_calendar(data, remove_prototypes = T)
    
  }else{
    septemberTarget <- 80
    title <- "Year to date monthly % of Apr-Sept target (80%) UOAs delivered"
    ylab <- "Cumulative % of target UOAs delivered \nfrom Apr-21"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_into_slide6_format_calendar(data, remove_prototypes = F)
    
  }
  
  if(nrow(data) < 6){
    data <- data %>% add_row(month = as.Date("2021-09-01"))
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / target_UDA_UOAs_delivered_by_sept) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1))
  
  #cumulative sum column
  data <- data %>%
    mutate(cumulative_perc_of_UDA_UOA_target_delivered = cumsum(perc_of_UDA_UOA_target_delivered)) 
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(cumulative_perc_of_UDA_UOA_target_delivered = if_else(is.na(monthly_UDA_UOAs_delivered),
                                                             0, 
                                                             cumulative_perc_of_UDA_UOA_target_delivered)) %>%
    mutate(target = seq(from = 16.7, to = 100, length.out = 6))
  
  
  #plot code
  ggplot(data_to_plot) +
    theme_bw() +
    geom_bar(aes(x = month, 
                 y = cumulative_perc_of_UDA_UOA_target_delivered),
             stat = "identity", 
             fill = barCol, 
             width = 10) +
    geom_line(aes(x = month, 
                  y = target), 
              colour = "blue", 
              linetype = "dashed") +
    geom_point(aes(x = month, 
                   y = target), 
               colour = "blue", 
               shape = 4, 
               size = 2) +
    geom_hline(yintercept = 100, 
               colour = "grey", 
               linetype = "dashed") +
    annotate(geom = "text", 
             x = as.Date("2021-09-01"), 
             y = 90, 
             label = "Expected monthly \nactivity to achieve \ntarget by Sep-21", 
             size = 3) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$cumulative_perc_of_UDA_UOA_target_delivered + 3, 
             label = round(data$cumulative_perc_of_UDA_UOA_target_delivered, 2), 
             size = 3) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    labs(title = title, 
         x = "Month", 
         y = ylab)
  
}




################################################################################
#function to create graph on slide 8
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_banded_CoT <- function(data = slide8_data){
  
  #get data into the right format  
  data <- reshape2::melt(data, id.vars = "month")
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    rename(band = variable, CoTs = value)

  
  #plot code
  ggplot(data) +    
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_line(aes(x = month, 
                  y = CoTs, 
                  colour = band), 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    labs(title = "Banded Courses of Treatment 2019/20 to 2021/22", 
         x = "Month",
         y = "Number of FP17 forms submitted")
    
  
}



################################################################################
#function to create graph on slide 5
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
#current data is in teams folder "Monthly performance data\April 21 to September 21 data\Scheduled data\April to Sept UDA 2020-2021 (May).xlsx"
plot_UDA_UOA_delivery <- function(data = slide5_UDA, UDAorUOA = "UDA"){
  
  if(UDAorUOA == "UDA"){
    title <- "Average percentage of contracted UDAs delivered \nscaled up to 12 months"
    ylab <- "% of contracted UDAs delivered"
    lineCol <- "coral"
  }else{
    title <- "Average percentage of contracted UOAs delivered \nscaled up to 12 months"
    ylab <- "% of contracted UOAs delivered"
    lineCol <- "seagreen3"
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_UDA_UOA_delivered = perc_UDA_UOA_delivered * 100)
  
  #plot code
  ggplot(data) +
    theme_bw() +
    geom_line(aes(x = month, 
                  y = perc_UDA_UOA_delivered), 
              colour = lineCol, 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(limits = c(0, max(data$perc_UDA_UOA_delivered, na.rm = T) + 5)) +
    labs(title = title, 
         x = "Month",
         y = ylab) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_UDA_UOA_delivered + 1, 
           label = paste0(data$perc_UDA_UOA_delivered, "%"), 
           size = 3) 
}




################################################################################
#function to create graph on slide 9
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_urgent_form_submissions <- function(data = slide8_data){
  
  #get data in the right format
  data <- data %>%
    mutate(date = as.Date(month)) %>%
    mutate(financial_year = if_else(month >= as.Date("2019-04-01") & month < as.Date("2020-04-01"),
                                    "2019/20", 
                                    if_else(month >= as.Date("2020-04-01") & month < as.Date("2021-04-01"),
                                            "2020/21",
                                            "2021/22"))) 
  
  
  #plot code
  ggplot(data) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    geom_line(aes(x = factor(lubridate::month(date, label=TRUE, abbr=TRUE), 
                             levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")), 
                  y = urgent, 
                  group = factor(financial_year),
                  colour = factor(financial_year)), 
              size = 1) +
    scale_y_continuous(breaks = seq(0, 550000, 50000)) +
    labs(title = "Urgent treatment form submissions",
         x = "Month",
         y = "Number of urgent FP17 forms submitted")
    
    
}


################################################################################
#function to plt delivery profile month by month 
plot_UDA_delivery_profile <- function(data = UDA_performance_bands){
  
  data$performance_band <- factor(data$performance_band, levels = c("0-9%","10% to 19%", 
                                      "20% to 29%", "30% to 39%", "40% to 49%", "50 to 59%",
                                      "60% to 69%", "70% to 79%", "80% to 89%", "90% to 99%",
                                      "100% +"))
  
  ggplot(data, 
         aes(fill = performance_band, 
             y = perc_of_contracts*100, 
             x = month)) +
    geom_bar(position = "dodge", 
             stat = "identity") +
    labs(title = "Proportion of contracts delivering in each performance band \nof total contracted UDA per month",
         x = "Month",
         y = "Percentage of contracts delivering in this band") +
    scale_x_datetime(breaks = data$month, 
                     labels = scales::date_format("%b-%y")) +
    geom_vline(xintercept = as.Date("2020-07-01"), colour = "black", size = 5)
}







################################################################################
#ADJUSTED
#function to plot first chart on slide 4 adjusted for working day
#current data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
plot_UDA_UOA_to_target_adjusted <- function(data = get_into_slide4_format_calendar(), UDAorUOA = "UDA"){
  
  data <- data %>% mutate(working_day_adjustment = c(0.158730159, 0.150793651, 0.174603175, 0.182539683, 0.158730159)) %>%
    mutate(adjusted_monthly_target = target_UDA_UOAs_delivered_by_sept * working_day_adjustment) %>%
    mutate(adjusted_september_target = adjusted_monthly_target * 6)
  
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    #mutate(target_UDA_UOAs_delivered_by_sept = (annual_contracted_UDA_UOA) * septemberTarget / 100) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / adjusted_september_target) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1))
  
  if(UDAorUOA == "UDA"){
    septemberTarget <- 60
    title <- "Monthly % of Apr-Sept target (60%) UDAs delivered \nAdjusted for working days per month"
    ylab <- "% of target UDAs delivered"
    barCol <- "coral"
  }else{
    septemberTarget <- 80
    title <- "Monthly % of Apr-Sept target (80%) UOAs delivered"
    ylab <- "% of target UOAs delivered"
    barCol <- "seagreen3"
  }
  
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(perc_of_UDA_UOA_target_delivered = if_else(is.na(perc_of_UDA_UOA_target_delivered)
                                                      , 0, 
                                                      perc_of_UDA_UOA_target_delivered)) 
  
  #plot code
  ggplot(data_to_plot, 
         aes(x = month, 
             y = perc_of_UDA_UOA_target_delivered)) +
    geom_bar(stat = "identity", 
             fill = barCol, 
             width = 10) +
    theme_bw() +
    geom_hline(yintercept = 16.7, 
               colour = "blue", 
               linetype = "dashed") +
    annotate(geom = "text", 
             x = as.Date("2021-09-01"),
             y = 17.7, 
             label = "16.7% target", 
             size = 3) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_of_UDA_UOA_target_delivered + 1, 
             label = round(data$perc_of_UDA_UOA_target_delivered, 2), 
             size = 3) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    labs(title = title, 
         x = "Month", 
         y = ylab)
}


################################################################################
#unfinished
get_adjusted_number_reaching_target <- function(){
  
  #need to bind in annual contracted UDA from another table
  
  dental_data_combined_calendar_contracted4 <- group_by(dental_data_combined_calendar_contracted3, Month)
  dental_data_combined_calendar_contracted4 <- group_by(dental_data_combined_calendar_contracted4, Contract.Number)
  dental_data_combined_calendar_contracted5 <- summarise(dental_data_combined_calendar_contracted4)
  dental_data_combined_calendar_contracted5 <- summarise(dental_data_combined_calendar_contracted4, UDA.Total = sum(UDA.Total), Annual.contracted.UDA = sum(Annual.contracted.UDA), monthly_adjusted_contracted_UDA = sum(monthly_adjusted_contracted_UDA))
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, perc_adjusted_target_deliverd = UDA.Total * 100 / monthly_adjusted_contracted_UDA)
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, monthly_contracted_UDA = Annual.contracted.UDA / 12)
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, perc_contracted_delivered = UDA.Total * 100 / monthly_contracted_UDA)
  count(dental_data_combined_calendar_contracted5, perc_contracted_delivered >= 50)
}