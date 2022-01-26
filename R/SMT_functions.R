library(tidyverse)
#plot functions for SMT report and more



#function to plot first chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either UDA_calendar_data or orthodontic_data_combined_calendar
plot_UDA_UOA_to_target <- function(data = UDA_calendar_data, UDAorUOA = "UDA", 
                                   level = "National",
                                   region_STP_name = NULL,
                                   contractor_cats = contractor_categories,
                                   cat = NULL){
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats)

  if(!is.null(cat)){
    data <- filter(data, category_sub_type == cat)
    
    numOfCats <- count(contractor_cats, category_sub_type)
    numOfCats <- numOfCats %>%
      filter(category_sub_type == cat)
    numOfCats <- numOfCats[1,2]
    cat_sub <- paste0(" (", cat, " contracts only", " - ", numOfCats,"/6906)")
    
  }else{
    cat_sub <- " (All categories)"
  }

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
   decemberTarget <- 65
    title <- "Monthly % of Q1, Q2 and Q3 contracted UDAs delivered"
    ylab <- "% of quarterly \ncontracted UDAs delivered"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_into_slide4_format_calendar(data, remove_prototypes = T)
    
  }else{
    septemberTarget <- 80
   decemberTarget <- 85
    title <- "Monthly % of Q1, Q2 and Q3 contracted UOAs delivered"
    ylab <- "% of quarterly \ncontracted UDAs delivered"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_into_slide6_format_calendar(data, remove_prototypes = F)
  }
  
  #add blanks for future dates
  if(nrow(data) < 9){
    if(!(as.Date("2021-09-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-09-01"))
    }
    if(!(as.Date("2021-10-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-10-01"))
    }
    if(!(as.Date("2021-11-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-11-01"))
    }
    if(!(as.Date("2021-12-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-12-01"))
    }
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / target_UDA_UOAs_delivered_in_target_period) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1)) %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 100 * 4/ total_annual_UDA_UOAs_contracted) %>%
    mutate(financial_quarter = if_else(month < as.Date("2021-07-01"), "Apr-Jun (Q1)", 
                                       if_else(month < as.Date("2021-10-01"), "Jul-Sep (Q2)",
                                               "Oct-Dec (Q3)")))
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = if_else(is.na(perc_of_contracted_UDA_UOAs_delivered)
                                                  , 0, 
                                                  perc_of_contracted_UDA_UOAs_delivered)) %>%
    mutate(target = if_else(financial_quarter == "Apr-Jun (Q1)" | financial_quarter == "Jul-Sep (Q2)", septemberTarget/3,decemberTarget/3)) 
  
  #plot code
  ggplot(data_to_plot, 
         aes(x = month, 
             y = perc_of_contracted_UDA_UOAs_delivered)) +
    geom_bar(stat = "identity", 
             fill = barCol, 
             width = 10) +
    geom_line(aes(x = month, 
                  y = target,
                  colour = financial_quarter), 
              linetype = "dashed") +
    geom_point(aes(x = month,
                   y = target,
                   colour = financial_quarter),
               shape = 4,
               size = 3) +
    geom_vline(xintercept = as.Date("2021-09-01") + lubridate::days(15),
               linetype = "dotted") +
    geom_vline(xintercept = as.Date("2021-06-01") + lubridate::days(15),
               linetype = "dotted") +
    theme_bw() +
    annotate(geom = "label", 
             x = data$month, 
             y = data$perc_of_contracted_UDA_UOAs_delivered + 1, 
             label = format(round(data$perc_of_contracted_UDA_UOAs_delivered, 1), nsmall = 1), 
             size = 3,
             label.size = 0) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_colour_manual(labels = c(paste0("Expected monthly delivery to reach \nQ1 threshold of ", septemberTarget,"% by end of Jun-21*"),
                                   paste0("Expected monthly delivery to reach \nQ2 threshold of ",septemberTarget,"% by end of Sep-21*"),
                                   paste0("Expected monthly delivery to reach \nQ3 threshold of ",decemberTarget,"% by end of Dec-22*")
                                   ),
                        values = c("darkred", "blue", "darkgreen")) + 
      labs(title = title, 
           x = "Month", 
           y = ylab,
           subtitle = paste0(subtitle, cat_sub),
           caption = paste0("*expected monthly delivery to meet threshold is caluclated by assuming equal delivery across the 3 month period.
           For Apr-Jun and Jul-Sep the monthly expected delivery is ",septemberTarget,"/3 = ",round(septemberTarget/3,1),"%, for Oct-Dec the monthly expected delivery is ",decemberTarget,"/3 = ",round(decemberTarget/3,1),"%")) +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0.5, 
                                      face= "italic"),
          plot.caption.position =  "plot"
          )

}



################################################################################
#function to plot second chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either UDA_calendar_data or orthodontic_data_combined_calendar
plot_Q3_cumulative_UDA_UOA_to_target <- function(data = UDA_calendar_data, 
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
    decemberTarget <- 65
    marchTarget <- 85
    title <- "Cumulative monthly % of Q1, Q2 and Q3 contracted UDAs delivered"
    ylab <- "Cumulative % of quarterly \ncontracted UDAs delivered"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_into_slide4_format_calendar(data, remove_prototypes = T)
    
  }else{
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    title <- "Cumulative monthly % of Q1, Q2 and Q3 contracted UOAs delivered"
    ylab <- "Cumulative % of quarterly \ncontracted UOAs delivered"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_into_slide6_format_calendar(data, remove_prototypes = F)
    
  }
  
  #add blanks for future dates
  if(nrow(data) < 12){
    
    if(!(as.Date("2022-01-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2022-01-01"))
    }
    
    if(!(as.Date("2022-02-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2022-02-01"))
    }
    
    if(!(as.Date("2022-03-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2022-03-01"))
    }
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / target_UDA_UOAs_delivered_in_target_period) %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 100 * 4/ total_annual_UDA_UOAs_contracted) %>%
    mutate(financial_quarter = case_when(month < as.Date("2021-07-01") ~ "Apr-Jun (Q1)", 
                                         month < as.Date("2021-10-01") ~ "Jul-Sep (Q2)",
                                         month < as.Date("2022-01-01") ~ "Oct-Dec (Q3)",
                                         month < as.Date("2022-04-01") ~ "Jan-Mar (Q4)"))
  
  data$financial_quarter <- factor(data$financial_quarter,
                                   levels = c("Apr-Jun (Q1)",
                                              "Jul-Sep (Q2)",
                                              "Oct-Dec (Q3)",
                                              "Jan-Mar (Q4)"))
  
  #cumulative sum column
  data <- data  %>%
    group_by(financial_quarter) %>%
    mutate(cumulative_perc_of_contracted_UDA_UOAs_delivered = cumsum(perc_of_contracted_UDA_UOAs_delivered)) %>%
    ungroup()
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(cumulative_perc_of_contracted_UDA_UOAs_delivered = if_else(is.na(monthly_UDA_UOAs_delivered),
                                                                 0, 
                                                                 cumulative_perc_of_contracted_UDA_UOAs_delivered)) %>% 
    mutate(target = c(
      rep(seq(from = septemberTarget/3, to = septemberTarget, length.out = 3),2),
      seq(from = decemberTarget/3, to = decemberTarget, length.out = 3),
      seq(from = marchTarget/3, to = marchTarget, length.out = 3))) 
  
  #plot code
  ggplot(data_to_plot) +
    theme_bw() +
    geom_bar(aes(x = month,
                 y = cumulative_perc_of_contracted_UDA_UOAs_delivered),
             stat = "identity",
             fill = barCol,
             width = 10) +
    geom_line(aes(x = month,
                  y = target,
                  colour = financial_quarter),
              linetype = "dashed") +
    geom_point(aes(x = month,
                   y = target,
                   colour = financial_quarter),
               shape = 4,
               size = 3) +
    geom_vline(xintercept = as.Date("2021-09-01") + lubridate::days(15),
               linetype = "dotted") +
    geom_vline(xintercept = as.Date("2021-06-01") + lubridate::days(15),
               linetype = "dotted") +
    geom_vline(xintercept = as.Date("2021-12-01") + lubridate::days(15),
               linetype = "dotted") +
    annotate(geom = "label",
             x = data_to_plot$month,
             y = data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered + 4,
             label = ifelse(data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered != 0,
                            format(round(data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered, 1), nsmall = 1),
                            ""),
             size = 3,
             label.size = 0) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    scale_colour_manual(labels = c(paste0("Expected cumulative delivery to reach \nQ1 threshold of ",septemberTarget,"% by end of Jun-21"),
                                   paste0("Expected cumulative delivery to reach \nQ2 threshold of ",septemberTarget,"% by end of Sep-21"),
                                   paste0("Expected cumulative delivery to reach \nQ3 threshold of ",decemberTarget,"% by end of Dec-21"),
                                   paste0("Expected cumulative delivery to reach \nQ4 threshold of ",marchTarget,"% by end of Mar-22")),
                        values = c("darkred", "blue", "darkgreen", "magenta4")) +
    labs(title = title,
         x = "Month",
         y = ylab,
         subtitle = subtitle,
         colour = "") +
    theme(legend.position = "bottom")


}




################################################################################
#function to create graph on slide 8
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_banded_CoT <- function(data = UDA_scheduled_data, 
                            calendar_data = UDA_calendar_data,
                            #existing_data = slide8_banded_CoT_historic,
                            historic_data = historical_UDA_scheduled_data, 
                            level = "National",
                            region_STP_name = NULL,
                            plot_chart = TRUE){
  
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
  
  if(plot_chart == TRUE){
    
    #plot code
    ggplot(data) +    
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
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
         UDAs and an 'other' FP17 = 0.6 UDAs. Scheduled data used.")
    
  }else{
    data
  }
  
  

}



################################################################################
#function to create graph on slide 5
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
#current data is in teams folder "Monthly performance data\April 21 to September 21 data\Scheduled data\April to Sept UDA 2020-2021 (May).xlsx"
plot_UDA_UOA_delivery <- function(data = UDA_scheduled_data, 
                                  #existing_data = slide5_UDA_delivery_historic,
                                  calendar_data = UDA_calendar_data,
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  remove_prototypes = T){
  
  data <- data %>%
    mutate(month = as.Date(month))
  
  calendar_data <- calendar_data %>%
    mutate(month = as.Date(month)) 
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, region_name) %>%
    distinct()
  
  # data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  data <- left_join(data, region_STP_lookup, by = c("contract_number"))
  
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
    data <- get_into_slide5_7_format(data, remove_prototypes, UDAorUOA = "UDA")
    title <- "Scheduled monthly percentage of usual annual contracted UDAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UDAs submitted"
    lineCol <- "coral"
    lineCol <- "#CC79A7"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
  }else{
    #get data into the right format
    data <- get_into_slide5_7_format(data, remove_prototypes, UDAorUOA = "UOA")
    title <- "Scheduled monthly percentage of usual annual contracted UOAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UOAs submitted"
    lineCol <- "#009E73"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
  }

  #plot code
  ggplot(data) +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_line(aes(x = month, 
                  y = perc_UDA_UOA_delivered), 
              colour = lineCol, 
              size = 1) +
    geom_point(aes(x = month, 
                  y = perc_UDA_UOA_delivered), 
              colour = lineCol
              ) +
    geom_segment(aes(x = as.Date("2021-04-01"), y = septemberTarget, xend = as.Date("2021-09-01"), yend = septemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    geom_segment(aes(x = as.Date("2021-10-01"), y = decemberTarget, xend = as.Date("2021-12-01"), yend = decemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    geom_segment(aes(x = as.Date("2022-01-01"), y = marchTarget, xend = as.Date("2022-03-01"), yend = marchTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    
    annotate(geom = "text", 
             x = as.Date("2021-04-01") + lubridate::weeks(2), 
             y = septemberTarget - 5, 
             label = "H1 threshold", 
             size = 3,
             colour = "#0072B2") + 
    annotate(geom = "text", 
             x = as.Date("2021-10-01") + lubridate::weeks(2), 
             y = decemberTarget - 5, 
             label = "Q3 threshold", 
             size = 3,
             colour = "#0072B2") +
    annotate(geom = "text", 
             x = as.Date("2022-01-01") + lubridate::weeks(2), 
             y = marchTarget - 5, 
             label = "Q4 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(limits = c(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5)) +
    labs(title = title, 
         x = "Month",
         y = ylab, 
         subtitle = subtitle,
         caption = "*Excluding prototype contracts and those with annual contracted UDA < 100 
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12.") +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_UDA_UOA_delivered + 3, 
             label = paste0(data$perc_UDA_UOA_delivered, "%"), 
             size = 3) 
}



################################################################################
#function to create graph on slide 5
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
#current data is in teams folder "Monthly performance data\April 21 to September 21 data\Scheduled data\April to Sept UDA 2020-2021 (May).xlsx"
plot_UDA_UOA_delivery_calendar <- function(data = UDA_calendar_data, 
                                  scheduled_data = UDA_scheduled_data,
                                  contractor_cats = contractor_categories,
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  remove_prototypes = T,
                                  regional_lines = F, 
                                  STP_lines = F,
                                  cat_lines = F,
                                  plot_chart = T){
  
  data <- data %>%
    mutate(month = as.Date(month))
  
  scheduled_data <- scheduled_data %>%
    mutate(month = as.Date(month)) 
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats)


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
    data <- get_into_slide5_7_format_calendar(data, scheduled_data, remove_prototypes, UDAorUOA = "UDA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UDAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UDAs delivered"
    lineCol <- "coral"
    #lineCol <- "#CC79A7"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
  }else{
    
    #get data into the right format
    data <- get_into_slide5_7_format_calendar(data, scheduled_data, remove_prototypes, UDAorUOA = "UOA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UOAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UOAs delivered"
    lineCol <- "#009E73"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
  }
  
  captionTitle <- "*Excluding prototype contracts and those with annual contracted UDA < 100
                   **This is calendar data which means that data may change as more CoTs are registered"
  subtitle_addition <- ""
  
  if(regional_lines){
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = region_name), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = region_name)
      )
    
    legendTitle <- "Region"
    
  }else if(STP_lines){
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = commissioner_name), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = commissioner_name)
      )
    
    legendTitle <- "STP"
    
  }else if(cat_lines){
    data <- data %>%
      filter(!is.na(category_sub_type))
    
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = category_sub_type), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = category_sub_type)
      )
    
    legendTitle <- "MY Category"
    subtitle_addition <- if_else(remove_prototypes, " - *Excluding prototypes and contracts with annual contracted UDA < 100",
                        " - *Including prototypes and contracts with annual contracted UDA < 100")
    
    captionTitle <- "**This is calendar data which means that data may change as more CoTs are registered"
    
  }else{
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered),
                colour = lineCol, 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered), 
                 colour = lineCol
      )+
      annotate(geom = "text", 
               x = data$month, 
               y = data$scaled_perc_UDA_UOA_delivered + 5, 
               label = paste0(round(data$scaled_perc_UDA_UOA_delivered), "%"), 
               size = 3) 
    
  }

   g <- g +
    geom_segment(aes(x = as.Date("2021-04-01"), 
                     y = septemberTarget, 
                     xend = as.Date("2021-09-01"), 
                     yend = septemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
     
    geom_segment(aes(x = as.Date("2021-10-01"), 
                     y = decemberTarget, 
                     xend = as.Date("2021-12-01"), 
                     yend = decemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
     
     geom_segment(aes(x = as.Date("2022-01-01"), 
                      y = marchTarget, 
                      xend = as.Date("2022-03-01"), 
                      yend = marchTarget),
                  colour = "#0072B2",
                  linetype = "dashed") +
     
    
     annotate(geom = "text", 
              x = as.Date("2021-04-01") + lubridate::weeks(2), 
              y = septemberTarget - 3, 
              label = "H1 threshold", 
              size = 3,
              colour = "#0072B2") + 
     
     annotate(geom = "text", 
              x = as.Date("2021-10-01") + lubridate::weeks(2), 
              y = decemberTarget - 3, 
              label = "Q3 threshold", 
              size = 3,
              colour = "#0072B2") +
     
     annotate(geom = "text", 
              x = as.Date("2022-01-01") + lubridate::weeks(2), 
              y = marchTarget - 3, 
              label = "Q4 threshold", 
              size = 3,
              colour = "#0072B2") +
     
     scale_x_date(date_breaks = "1 month", 
                  date_labels = "%b-%y") +
     scale_y_continuous(limits = c(0, max(c(data$scaled_perc_UDA_UOA_delivered, 90), na.rm = T) + 10),
                        breaks = scales::breaks_pretty()) 
   
   
   if(regional_lines == F & cat_lines == F){
     g <- g +
       # annotate(geom = "text", 
       #          x = as.Date("2021-04-01") + lubridate::weeks(2), 
       #          y = septemberTarget - 3, 
       #          label = "H1 threshold", 
       #          size = 3,
       #          colour = "#0072B2") + 
       # annotate(geom = "text", 
       #          x = as.Date("2021-10-01") + lubridate::weeks(2), 
       #          y = decemberTarget - 3, 
       #          label = "Q3 threshold", 
       #          size = 3,
       #         colour = "#0072B2") +
       labs(title = title, 
            x = "Month",
            y = ylab, 
            subtitle = paste0(subtitle, subtitle_addition),
            caption = "*Excluding prototype contracts and those with annual contracted UDA < 100 
                    **This is calendar data which means that data may change as more CoTs are registered")

   }else{
     g <- g +
       labs(title = title, 
            x = "Month",
            y = ylab, 
            subtitle = paste0(subtitle, subtitle_addition),
            caption = captionTitle,
            colour = legendTitle) 
       
   }
    
   
   if(plot_chart){
     g
   }else{
     data
   }
}


################################################################################
table_number_cat_contracts <- function(data = UDA_calendar_data, 
                                      scheduled_data = UDA_scheduled_data,
                                      contractor_cats = contractor_categories,
                                      remove_prototypes = F,
                                      level = NULL,
                                      region_STP_name = NULL){
  
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
  
  #join in contracted UDAs from scheduled data
  contracted_UDAs <- scheduled_data %>%
    select(month, contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(contracted_UDAs, by = c("month", "contract_number"))
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function•
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats) %>%
    group_by(month) %>%
    count(category_sub_type) %>%
    ungroup() %>%
    filter(month == max(data$month)) %>%
    select(category_sub_type, n) %>%
    rename(number_of_contracts = n) 
  
  data <- data %>%
    mutate(percentage_of_contracts = round(number_of_contracts * 100 / sum(data$number_of_contracts))) %>%
    add_row(category_sub_type = "TOTAL", number_of_contracts = sum(data$number_of_contracts), percentage_of_contracts = 100) %>%
    rename(`category sub type` = category_sub_type,
           `number of contracts` = number_of_contracts,
           `percentage of contracts` = percentage_of_contracts)
  
  data

  # ggplot(data, aes(x="", y=n, fill = category_sub_type)) +
  #  geom_bar(width = 1, stat = "identity") +
  #   coord_polar("y", start=0) +
  #   labs(title = "prototypes") 

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
         UDAs and an 'other' FP17 = 0.6 UDAs. Scheduled data used.")
    
    
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
  data <- get_slide5_table(data = data, UDAorUOA = UDAorUOA, remove_prototypes = T)
  
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
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == max(data$month))
  
  nrow(data)
}


################################################################################
get_Q3_num_contracts_on_target <- function(data = UDA_calendar_data, 
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
      select(month, contract_number, annual_contracted_UDA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.6 * annual_contracted_UDA/4,
                                                   month > as.Date("2021-10-01") ~ 0.65 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA, UOA_financial_half_target) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month > as.Date("2021-10-01") ~ 0.85 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2021-10-01"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2021-10-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2021-11-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2021-12-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                #mean_UDA_target = mean(UDA_financial_half_target),
                YTD_UDA_delivered = sum(UDA_total)) %>%
      mutate(mean_Q3_UDA_target = mean_annual_contracted_UDA * 0.65 / 4) %>%
      count(YTD_UDA_delivered >= (mean_Q3_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                #mean_Q3_UOA_target = mean(UOA_financial_half_target),
                YTD_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q3_UOA_target = mean_annual_contracted_UOA * 0.85 / 4) %>%
      count(YTD_UOA_delivered >= (mean_Q3_UOA_target) * month_factor / 3)
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


get_SOF_data <- function(data = UDA_scheduled_data){
  SOF_data <- data %>%
    select(month, contract_number, commissioner_name, annual_contracted_UDA, UDA_delivered) %>%
    filter(annual_contracted_UDA > 100 & !(contract_number %in% prototype_contracts)) %>%
    group_by(month, commissioner_name) %>%
    summarise(annual_contracted_UDA = sum(annual_contracted_UDA, na.rm = T), UDA_delivered = sum(UDA_delivered, na.rm = T)) %>%
    mutate(annual_contracted_UDA_scaled_monthly = annual_contracted_UDA / 12) %>%
    #change names to match ICS names
    mutate(commissioner_name = if_else(commissioner_name == "South East London STP", 
                                       "OUR HEALTHIER SOUTH EAST LONDON ICS", 
                                       commissioner_name)) %>%
    mutate(commissioner_name = if_else(commissioner_name == "Lancashire and South Cumbria STP", 
                                       "HEALTHIER LANCASHIRE AND SOUTH CUMBRIA ICS", 
                                       commissioner_name))
  
  #write.csv(SOF_data, "SOF_data.csv", row.names = F)
}
  

################################################################################
plot_111_referrals <- function(data = dental_data_111){
  
  data <- data %>% 
    mutate(month = as.Date(month)) %>%
    group_by(month) %>%
    summarise(monthly_case_volume = sum(monthly_case_volume))
  
  #plot code
  ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month, 
                  y = monthly_case_volume), 
              colour = "steelblue", 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(47000, 
                                    max(data$monthly_case_volume, na.rm = T) + 10000,
                                    10000)) +
    labs(title = "111 call volume recommended towards dental services", 
         x = "Month",
         y = "Call volume", 
         subtitle = "England"#,
         #caption = "*services include: "
    ) #+
  # annotate(geom = "text", 
  #          x = data$month, 
  #          y = data$monthly_case_volume + 1, 
  #          label = data$monthly_case_volume, 
  #          size = 3) 
  
  
}


################################################################################  
plot_breakdown_111_referrals <- function(data = dental_data_111){
  
  data <- data %>% 
    mutate(disposition_text = if_else(disposition_text == "Attend Emergency Dental Treatment Centre within 4",
                                      "Attend Emergency Dental Treatment Centre within 4hrs",
                                      disposition_text)) %>%
    mutate(month = as.Date(month)) 
  
  #get lines in the right order
  data$disposition_text <- factor(data$disposition_text,
                                  levels = c("Attend Emergency Dental Treatment Centre within 4hrs",
                                             "To Contact a Dental Service within 1 hour",
                                             "To Contact a Dental Service within 2 hours",
                                             "Speak to a Dental Service within 2 hours",
                                             "To Contact a Dental Service within 6 hours",
                                             "To Contact a Dental Service within 12 hours",
                                             "To Contact a Dental Service within 24 hours",
                                             "To Contact a Dental Practice within 5 working days",
                                             "Dental - Contact Orthodontist next working day"
                                             ))
  
  #plot code
  ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month, 
                  y = monthly_case_volume,
                  colour = str_wrap(disposition_text, 35)), 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    # scale_y_continuous(breaks = seq(47000, 
    #                                 max(data$monthly_case_volume, na.rm = T) + 10000,
    #                                 10000)) +
    labs(title = "111 call volume recommended towards dental services", 
         x = "Month",
         y = "Call volume", 
         subtitle = "England",
         colour = "Disposition"#,
         #caption = "*services include: "
    ) #+
  # annotate(geom = "text", 
  #          x = data$month, 
  #          y = data$monthly_case_volume + 1, 
  #          label = data$monthly_case_volume, 
  #          size = 3) 
  
  
}


################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter <- function(data = UDA_scheduled_data,
                                                   remove_prototypes = T,
                                                   plot_month = as.Date("2021-11-01"),
                                                   UDAorUOA = "UDA"){
  
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) 
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(colour = "steelblue") +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                       ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560."
    )
  
}



################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter_corporate <- function(data = UDA_scheduled_data,
                                                             calendar_data = UDA_calendar_data,
                                                             demographics_data = contract_demographics,
                                                             remove_prototypes = T,
                                                             plot_month = NULL,
                                                             UDAorUOA = "UDA",
                                                             level = "National",
                                                             region_STP_name = NULL,
                                                             get_num_above = NULL){
  if(is.null(plot_month)){
    #use latest month in data
    plot_month <- max(data$month)
  }
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, region_name) %>%
    distinct()
  
  # data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  data <- left_join(data, region_STP_lookup, by = c("contract_number"))
  
  #filter for STP or region
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
  
  
  #join in demographics data
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data)
  
  #create not in function
  `%notin%` = Negate(`%in%`)

  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }

  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) 
  
  data_to_plot <- data %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Corporate.Status)) %>%
    arrange(Corporate.Status)


  if(is.null(get_num_above)){
    p <- ggplot(data_to_plot, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
      geom_point(aes(colour = Corporate.Status)) +
      theme_bw() +
      geom_hline(yintercept = 0.65,
                 colour = "orangered4",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA),
               y = 0.59,
               label = "65% Q3 threshold",
               size = 3,
               #label.size = 0,
               colour = "orangered4") +
      geom_hline(yintercept = 1,
                 colour = "grey40",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA),
               y = 0.94,
               label = "100%",
               size = 3,
               #label.size = 0,
               colour = "grey40") +
      scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
      scale_x_continuous(breaks = seq(0, 175000, 20000),
                         limits = c(0, max(data_to_plot$annual_contracted_UDA) + 5000),
                         #labels = scales::percent_format(accuracy = 1)
                         labels=function(x) format(x, big.mark = ",", scientific = FALSE)
      ) +
      scale_y_continuous(breaks = seq(0, 3, 0.5),
                         limits = c(0, 3),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
           subtitle = paste(format(plot_month, "%B %Y"), "scheduled delivery -", subtitle),
           x = "Annual contracted UDAs",
           y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
           caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         Also excluding contracts with delivery > 300% for plot purposes.",
           colour = "Corporate status"
      )
    
    p
  }else{
    data <- data %>%
      filter(UDA_delivery >= get_num_above) 
    
    num_contracts_above_threshold <- round(nrow(data))
    mean_contract_size_above_threshold <- round(mean(data$annual_contracted_UDA, na.rm = T))
    
    list(num_contracts_above_threshold = num_contracts_above_threshold,
         mean_contract_size_above_threshold = mean_contract_size_above_threshold)
  }
  

}
  


################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter_dental_group <- function(data = UDA_scheduled_data,
                                                                demographics_data = contract_demographics,
                                                                remove_prototypes = T,
                                                                plot_month = as.Date("2021-11-01"),
                                                                UDAorUOA = "UDA"){
  
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Dental.Group)) %>%
    arrange(Dental.Group)
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(aes(colour = Dental.Group)) +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    #scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560.",
         colour = "Dental Group"
    )
  
}



################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter_region <- function(data = UDA_scheduled_data,
                                                                demographics_data = contract_demographics,
                                                                remove_prototypes = T,
                                                                plot_month = as.Date("2021-11-01"),
                                                                UDAorUOA = "UDA"){
  
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      #filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Average.UDA.value)) %>%
    arrange(Average.UDA.value)
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(aes(colour = Average.UDA.value)) +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    #scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560.",
         colour = "Average UDA value"
    )
  
}

