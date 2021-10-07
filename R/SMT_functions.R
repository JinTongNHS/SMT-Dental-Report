library(tidyverse)
#plot functions for SMT report and more



#function to plot first chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either UDA_calendar_data or orthodontic_data_combined_calendar
plot_UDA_UOA_to_target <- function(data = UDA_calendar_data, UDAorUOA = "UDA", 
                                   level = "National",
                                   region_STP_name = NULL){
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  
  #change titles and colours for UDA or UOA
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
         y = ylab,
         subtitle = subtitle)
}

################################################################################
#function to plot second chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either UDA_calendar_data or orthodontic_data_combined_calendar
plot_cumulative_UDA_UOA_to_target <- function(data = UDA_calendar_data, 
                                              UDAorUOA = "UDA", 
                                              level = "National",
                                              region_STP_name = NULL){
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #change titles and colours for UDA or UOA
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
         y = ylab,
         subtitle = subtitle)
  
}




################################################################################
#function to create graph on slide 8
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_banded_CoT <- function(data = UDA_scheduled_data, 
                            calendar_data = UDA_calendar_data,
                            #existing_data = slide8_banded_CoT_historic,
                            historic_data = historical_UDA_scheduled_data, 
                            level = "National",
                            region_STP_name = NULL){
  
  #avoid standard form on axes
  options(scipen = 100)

  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
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

  #get data into the right format
  data <- get_into_slide8_format(data, historic_data = historic_data, remove_prototypes = F)
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
    scale_colour_manual(labels = c("Band 1", "Band 2", "Band 3", "Other", "Urgent"), 
                        values = c("coral3",
                                   "orange",
                                   "yellow3",
                                   "green",
                                   "blue")
                        ) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    labs(title = "Banded Courses of Treatment", 
         x = "Month",
         y = "Number of FP17* forms submitted",
         colour = "Band",
         subtitle = subtitle,
         caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17 
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs, 
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2 
         UDAs and an 'other' FP17 = 0.6 UDAs.")
  
}



################################################################################
#function to create graph on slide 5
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
#current data is in teams folder "Monthly performance data\April 21 to September 21 data\Scheduled data\April to Sept UDA 2020-2021 (May).xlsx"
plot_UDA_UOA_delivery <- function(data = UDA_scheduled_data, 
                                  existing_data = slide5_UDA_delivery_historic,
                                  calendar_data = UDA_calendar_data,
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  if(UDAorUOA == "UDA"){
    #get data into the right format
    data <- get_into_slide5_7_format(data, existing_data, remove_prototypes = F, UDAorUOA = "UDA")
    title <- "Average percentage of contracted UDAs delivered \nscaled up to 12 months"
    ylab <- "% of contracted UDAs delivered"
    lineCol <- "coral"
  }else{
    #get data into the right format
    data <- get_into_slide5_7_format(data, existing_data, remove_prototypes = F, UDAorUOA = "UOA")
    title <- "Average percentage of contracted UOAs delivered \nscaled up to 12 months"
    ylab <- "% of contracted UOAs delivered"
    lineCol <- "seagreen3"
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_UDA_UOA_delivered = perc_UDA_UOA_delivered)
  
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
         y = ylab, 
         subtitle = subtitle) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_UDA_UOA_delivered + 1, 
           label = paste0(data$perc_UDA_UOA_delivered, "%"), 
           size = 3) 
}




################################################################################
#function to create graph on slide 9
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_urgent_form_submissions <- function(data = UDA_scheduled_data, 
                                         #existing_data = slide8_banded_CoT_historic,
                                         calendar_data = UDA_calendar_data,
                                         historic_data = historical_UDA_scheduled_data, 
                                         level = "National",
                                         region_STP_name = "Cheshire and Merseyside STP"){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
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
  data <- get_into_slide8_format(data, historic_data = historic_data, remove_prototypes = F)
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
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    labs(title = "Urgent treatment form submissions",
         x = "Month",
         y = "Number of urgent FP17* forms submitted",
         colour = "Financial year",
         subtitle = subtitle,
         caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17 
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs, 
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2 
         UDAs and an 'other' FP17 = 0.6 UDAs.")
    
    
}


################################################################################
#function to plt delivery profile month by month 
plot_UDA_UOA_delivery_profile <- function(data = UDA_scheduled_data, 
                                          calendar_data = UDA_calendar_data,
                                          UDAorUOA = "UDA",
                                          level = "National",
                                          region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #get data into the right format
  data <- get_slide5_table(data = data, UDAorUOA = UDAorUOA, remove_prototypes = F)
  
  #change title for UDA or UOA
  if(UDAorUOA == "UDA"){
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UDA per month"
    legTitle <- "Performance band of \nUDA delivery"
  }else{
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UOA per month"
    legTitle <- "Performance band of \nUOA delivery"
  }
  
  #get bars in the correct order
  data$performance_band <- factor(data$performance_band, levels = c("0-9%","10-19%", 
                                      "20-29%", "30-39%", "40-49%", "50-59%",
                                      "60-69%", "70-79%", "80-89%", "90-99%",
                                      "100% +"))
  #colour blind friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                 "#52854C", "#4E84C4", "#293352", "#FFDB6D")
  
  #plot code
  ggplot(data, 
         aes(fill = performance_band, 
             y = perc_of_contracts, 
             x = month)) +
    geom_bar(position = "dodge", 
             stat = "identity") +
    labs(title = title,
         x = "Month",
         y = "Percentage of contracts delivering in this band",
         fill = legTitle,
         subtitle = subtitle) +
    #scale_fill_manual(values = cbPalette) +
    scale_x_datetime(breaks = data$month, 
                     labels = scales::date_format("%b-%y")) +
    geom_vline(xintercept = as.Date("2020-07-01"), colour = "black", size = 5) +
    theme_bw()
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




################################################################################
#function to plot the density of contracts based on performance 
plot_density <- function(data = density_data){
  
  ggplot(data, 
         aes(x = perc_performance_delivered)) +
    geom_density(size = 1,
                 aes(colour = timePeriod,
                     fill = timePeriod),
                 alpha = 0.1) +
    geom_vline(xintercept = 45, 
               size = 0.5, 
               colour = "blue",
               linetype = "dashed"
    ) +
    geom_vline(xintercept = 60, 
               size = 0.5, 
               colour = "red",
               linetype = "dashed"
    ) +
    annotate(geom = "text", 
             x = 40, 
             y = 0, 
             label = "45% target", 
             size = 3,
             colour = "blue") +
    annotate(geom = "text", 
             x = 65, 
             y = 0, 
             label = "60% target", 
             size = 3,
             colour = "red") +
    scale_x_continuous(limits = c(0,150),
                       breaks = seq(0,150, 10)) +
    labs(title = "UDA performance distribution of contracts\nMarch 21 compared with Apr-Aug 2021",
         x = "Percentage of contracted UDAs delivered in given time period",
         y = "Density of contracts",
         colour = "Time Period",
         fill = "Time Period")
  
  
}

################################################################################
get_num_contracts <- function(data = UDA_calendar_data, 
                                    remove_prototypes = T,
                                    scheduled_data = UDA_scheduled_data,
                                    UDAorUOA = "UDA",
                              level = "National",
                              region_STP_name = NULL){
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }
  
  if(UDAorUOA == "UDA"){
    #get contracted UDAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA)
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA)
  }
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$proto_contracts)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$proto_contracts)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == max(data$month))
  
  nrow(data)
}

################################################################################
get_num_contracts_on_target <- function(data = UDA_calendar_data, 
                                       remove_prototypes = T,
                                       scheduled_data = UDA_scheduled_data,
                                       UDAorUOA = "UDA",
                                       level = "National",
                                       region_STP_name = NULL){
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }
  
  if(UDAorUOA == "UDA"){
    #get contracted UDAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA, UDA_target_60_for_Apr_to_Sept)
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA, UOA_target_80_for_Apr_to_Sept)
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) 
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$proto_contracts)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$proto_contracts)%>%
      filter(annual_contracted_UOA > 100)
  }

  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                mean_UDA_target_60_for_Apr_to_Sept = mean(UDA_target_60_for_Apr_to_Sept),
                YTD_UDA_delivered = sum(UDA_total)) %>%
      count(YTD_UDA_delivered >= (mean_UDA_target_60_for_Apr_to_Sept))
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                mean_UOA_target_80_for_Apr_to_Sept = mean(UOA_target_80_for_Apr_to_Sept),
                YTD_UOA_delivered = sum(UOA_total)) %>%
      count(YTD_UOA_delivered >= (mean_UOA_target_80_for_Apr_to_Sept))
  }
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)

}


################################################################################
get_num_urgent_forms <- function(data = UDA_scheduled_data, 
                                 existing_data = slide8_banded_CoT_historic,
                                 historic_data = historical_UDA_scheduled_data,
                                 calendar_data = UDA_calendar_data,
                                 remove_prototypes = F,
                                 level = "National",
                                 region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
  }
  
  data <- get_into_slide8_format(data = data, 
                                 existing_data = existing_data,
                                 historic_data = historic_data,
                                 remove_prototypes = remove_prototypes)
  
  data <- data %>% 
    filter(month == max(data$month))
  
  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)
  
}

################################################################################
# get_num_urgent_forms_2019 <- function(data = UDA_scheduled_data, 
#                                  existing_data = slide8_banded_CoT_historic,
#                                  historic_data = historical_UDA_scheduled_data,
#                                  calendar_data = UDA_calendar_data,
#                                  remove_prototypes = F,
#                                  level = "National",
#                                  region_STP_name = NULL){
#   
#   #add a region column to the data
#   region_STP_lookup <- calendar_data %>%
#     select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
#     distinct()
#   
#   data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
#   
#   #filter for STP or region
#   if(level == "Regional"){
#     data <- data %>% 
#       filter(region_name == region_STP_name )
#   }else if(level == "STP"){
#     data <- data %>% 
#       filter(commissioner_name == region_STP_name)
#   }
#   
#   data <- get_into_slide8_format(data, 
#                                  existing_data = existing_data,
#                                  historic_data = historic_data,
#                                  remove_prototypes = remove_prototypes)
#   
#   data <- data %>% 
#     filter(month == max(data$month) - lubridate::years(2))
#   
#   num_urgent_forms <- data[1, "urgent"]
#   as.integer(num_urgent_forms)
#   
# }

get_num_urgent_forms_2019 <- function(data = UDA_scheduled_data, 
                                 existing_data = slide8_banded_CoT_historic,
                                 historic_data = historical_UDA_scheduled_data,
                                 calendar_data = UDA_calendar_data,
                                 remove_prototypes = F,
                                 level = "National",
                                 region_STP_name = NULL){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    historic_data <- historic_data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    historic_data <- historic_data %>% 
      filter(commissioner_name == region_STP_name )
  }
  
  data <- get_into_slide8_format(data = data, 
                                 existing_data = existing_data,
                                 historic_data = historic_data,
                                 remove_prototypes = remove_prototypes)
  
  data <- data %>%
    filter(month == max(data$month) - lubridate::years(2))

  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)
  
}

  
    
  
  
  
