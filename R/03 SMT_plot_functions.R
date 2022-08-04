library(tidyverse)
#plot functions for SMT report

################################################################################
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
                                           plotChart = T){
  
  data <- data %>%
    mutate(month = as.Date(month))
  
  scheduled_data <- scheduled_data %>%
    mutate(month = as.Date(month)) 
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats, by = "contract_number")
  
  
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
    data <- get_delivery_data_calendar(data, scheduled_data, remove_prototypes, UDAorUOA = "UDA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UDAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UDAs delivered"
    captionTitle <- "*Excluding prototype contracts and those with annual contracted UDA < 100
                   **This is calendar data which means that data may change as more CoTs are registered"
    lineCol <- "coral"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
    juneTarget <- 95
  }else{
    
    #get data into the right format
    data <- get_delivery_data_calendar(data, scheduled_data, remove_prototypes, UDAorUOA = "UOA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UOAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UOAs delivered"
    captionTitle <- "*Excluding prototype contracts and those with zero annual contracted UOAs
                   **This is calendar data which means that data may change as more CoTs are registered"
    lineCol <- "#009E73"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    juneTarget <- 100
  }
  
  
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
    
    geom_segment(aes(x = as.Date("2022-04-01"), 
                     y = juneTarget, 
                     xend = as.Date("2022-06-01"), 
                     yend = juneTarget),
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
    
    annotate(geom = "text", 
             x = as.Date("2022-04-01") + lubridate::weeks(2), 
             y = juneTarget - 3, 
             label = "Q1 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(limits = c(0, max(c(data$scaled_perc_UDA_UOA_delivered, 90), na.rm = T) + 10),
                       breaks = scales::breaks_pretty()) 
  
  
  if(regional_lines == F & cat_lines == F){
    g <- g +
      labs(title = title, 
           x = "Month",
           y = ylab, 
           subtitle = paste0(subtitle, subtitle_addition),
           caption = captionTitle)
    
  }else{
    g <- g +
      labs(title = title, 
           x = "Month",
           y = ylab, 
           subtitle = paste0(subtitle, subtitle_addition),
           caption = captionTitle,
           colour = legendTitle) 
    
  }
  
  
  if(plotChart){
    g
  }else{
    
    if(UDAorUOA == "UDA"){
      new_col_names <- c(Month = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UDAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UDAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UDAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled percentage of UDAs delivered` = "scaled_perc_UDA_UOA_delivered",
                         `Category Sub-type` = "category_sub_type")
    }else{
      new_col_names <- c(Month = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UOAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UOAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UOAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled percentage of UOAs delivered` = "scaled_perc_UDA_UOA_delivered",
                         `Category Sub-type` = "category_sub_type")
    }
    
    data <- data %>%
      rename(any_of(new_col_names))
  }
}


################################################################################
plot_cumulative_UDA_UOA_to_target <- function(data = UDA_calendar_data, 
                                              UDAorUOA = "UDA", 
                                              level = "National",
                                              region_STP_name = NULL,
                                              plotChart = TRUE,
                                              all_regions_and_STPs = FALSE){
  
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
    juneTarget <- 95
    september22Target <- 100
    
    title <- "Cumulative monthly % of quarterly contracted UDAs delivered"
    ylab <- "Cumulative % of quarterly \ncontracted UDAs delivered"
    captionTitle <- "*Excluding prototype contracts and those with annual contracted UDA < 100
                   **This is calendar data which means that data may change as more CoTs are registered"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_data_for_cumulative_plot(data, remove_prototypes = TRUE, all_regions_and_STPs = all_regions_and_STPs)
    
  }else{
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    juneTarget <- 100
    september22Target <- 100
    
    title <- "Cumulative monthly % of quarterly contracted UOAs delivered"
    ylab <- "Cumulative % of quarterly \ncontracted UOAs delivered"
    captionTitle <- "*Excluding prototype contracts and those with zero annual contracted UOAs
                   **This is calendar data which means that data may change as more CoTs are registered"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_data_for_cumulative_plot_UOA(data, remove_prototypes = TRUE, all_regions_and_STPs = all_regions_and_STPs)
    
  }
  
  #add blanks for future dates if on single level
  if(all_regions_and_STPs == FALSE & nrow(data) < 19){

    if(!(as.Date("2022-08-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2022-08-01"))
    }
    
    if(!(as.Date("2022-09-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2022-09-01"))
    }
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_threshold_delivered = monthly_UDA_UOAs_delivered * 100 / threshold_UDA_UOAs_contracted_in_threshold_period) %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 100 * 4/ total_annual_UDA_UOAs_contracted) %>%
    mutate(financial_quarter = case_when(month < as.Date("2021-07-01") ~ "Apr-Jun (Q1)",
                                         month < as.Date("2021-10-01") ~ "Jul-Sep (Q2)",
                                         month < as.Date("2022-01-01") ~ "Oct-Dec (Q3)",
                                         month < as.Date("2022-04-01") ~ "Jan-Mar (Q4)",
                                         month < as.Date("2022-07-01") ~ "Apr-Jun (Q1 22/23)",
                                         month < as.Date("2022-10-01") ~ "Jul-Sep (Q2 22/23)"))
  
  data$financial_quarter <- factor(data$financial_quarter,
                                   levels = c("Apr-Jun (Q1)",
                                              "Jul-Sep (Q2)",
                                              "Oct-Dec (Q3)",
                                              "Jan-Mar (Q4)",
                                              "Apr-Jun (Q1 22/23)",
                                              "Jul-Sep (Q2 22/23)"))

  
  if(all_regions_and_STPs == FALSE){
    
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
        seq(from = marchTarget/3, to = marchTarget, length.out = 3),
        seq(from = juneTarget/3, to = juneTarget, length.out = 3),
        seq(from = september22Target/3, to = september22Target, length.out = 3)
        )
        )
    
  }else{
    #cumulative sum column
    data <- data  %>%
      group_by(financial_quarter, region_name, commissioner_name) %>%
      mutate(cumulative_perc_of_contracted_UDA_UOAs_delivered = cumsum(perc_of_contracted_UDA_UOAs_delivered)) %>%
      ungroup()
  }
  
  if(plotChart == TRUE){
  #plot code
  p <- ggplot(data_to_plot) +
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
    geom_vline(xintercept = as.Date("2022-03-01") + lubridate::days(15),
               linetype = "dotted") +
    geom_vline(xintercept = as.Date("2022-06-01") + lubridate::days(15),
               linetype = "dotted") +
    annotate(geom = "text",
             x = data_to_plot$month,
             y = data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered + 4,
             label = ifelse(data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered != 0,
                            format(round(data_to_plot$cumulative_perc_of_contracted_UDA_UOAs_delivered, 1), nsmall = 1),
                            ""),
             size = 3) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, 120, 10)) +
    scale_colour_manual(labels = c(paste0("Expected cumulative delivery to reach \nQ1 threshold of ",septemberTarget,"% by end of Jun-21"),
                                   paste0("Expected cumulative delivery to reach \nQ2 threshold of ",septemberTarget,"% by end of Sep-21"),
                                   paste0("Expected cumulative delivery to reach \nQ3 threshold of ",decemberTarget,"% by end of Dec-21"),
                                   paste0("Expected cumulative delivery to reach \nQ4 threshold of ",marchTarget,"% by end of Mar-22"),
                                   paste0("Expected cumulative delivery to reach \nQ1 threshold of ",juneTarget,"% by end of Jun-22"),
                                   paste0("Expected cumulative delivery to reach \nQ2 threshold of ",september22Target,"% by end of Sep-22")),
                        values = c("darkred", "blue", "darkgreen", "darkgoldenrod3", "darkorange3", "deeppink2")) +
    labs(title = title,
         x = "Month",
         y = ylab,
         subtitle = subtitle,
         colour = "",
         caption = captionTitle) +
    theme(legend.position = "bottom"#, legend.box="vertical", legend.margin=margin()
    ) +
    guides(colour=guide_legend(nrow=2,byrow=TRUE))
  
  
    p
  }else{
    
    if(UDAorUOA == "UDA"){
      data %>%
        select(-threshold_period) %>%
        mutate(`Quarterly contracted UDAs` = total_annual_UDA_UOAs_contracted / 4) %>%
        rename(Month = "month", 
               `Region Name` = "region_name", 
               `Commissioner Name` = "commissioner_name", 
               `Monthly UDAs delivered` = "monthly_UDA_UOAs_delivered", 
               `Total annual contracted UDAs` = "total_annual_UDA_UOAs_contracted", 
               `Threshold percentage` = "threshold_perc", 
               `Threshold contracted UDAs for quarter` = "threshold_UDA_UOAs_contracted_in_threshold_period", 
               `Monthly percentage of threshold UDAs delivered` = "perc_of_UDA_UOA_threshold_delivered", 
               `Montly percentage of quarterly contracted UDAs` = "perc_of_contracted_UDA_UOAs_delivered", 
               `Financial quarter` = "financial_quarter", 
               `Cumulative percentage of quarterly contracted UDAs` = "cumulative_perc_of_contracted_UDA_UOAs_delivered") %>%
        select(Month, `Financial quarter`, `Region Name`, `Commissioner Name`, 
               `Monthly UDAs delivered`, `Quarterly contracted UDAs`, 
               `Montly percentage of quarterly contracted UDAs`,
               `Cumulative percentage of quarterly contracted UDAs`)
    }else{
      data %>%
        select(-threshold_period) %>%
        mutate(`Quarterly contracted UOAs` = total_annual_UDA_UOAs_contracted / 4) %>%
        rename(Month = "month", 
               `Region Name` = "region_name", 
               `Commissioner Name` = "commissioner_name", 
               `Monthly UOAs delivered` = "monthly_UDA_UOAs_delivered", 
               `Total annual contracted UOAs` = "total_annual_UDA_UOAs_contracted", 
               `Threshold percentage` = "threshold_perc", 
               `Threshold contracted UOAs for quarter` = "threshold_UDA_UOAs_contracted_in_threshold_period", 
               `Monthly percentage of threshold UOAs delivered` = "perc_of_UDA_UOA_threshold_delivered", 
               `Montly percentage of quarterly contracted UOAs` = "perc_of_contracted_UDA_UOAs_delivered", 
               `Financial quarter` = "financial_quarter", 
               `Cumulative percentage of quarterly contracted UOAs` = "cumulative_perc_of_contracted_UDA_UOAs_delivered") %>%
        select(Month, `Financial quarter`, `Region Name`, `Commissioner Name`, 
               `Monthly UOAs delivered`, `Quarterly contracted UOAs`, 
               `Montly percentage of quarterly contracted UOAs`,
               `Cumulative percentage of quarterly contracted UOAs`)
    }
    
  }
  
}

################################################################################
plot_UDA_UOA_delivery <- function(data = UDA_scheduled_data, 
                                  calendar_data = UDA_calendar_data,
                                  historic_data = historical_UDA_scheduled_data,
                                  UDAorUOA = "UDA",
                                  level = "National",
                                  region_STP_name = NULL,
                                  remove_prototypes = TRUE, 
                                  plotChart = TRUE, 
                                  all_regions_and_STPs = FALSE,
                                  include_historic = TRUE){

  data <- data %>%
    mutate(month = as.Date(month))

  calendar_data <- calendar_data %>%
    mutate(month = as.Date(month))

  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number"))
  
  #bind in historic data if required
  if(include_historic == TRUE & UDAorUOA == "UDA"){
    data <- data %>%
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, UDA_delivered)
    
    historic_data <- historic_data %>%
      mutate(month = as.Date(month)) %>%
      filter(!is.na(annual_contracted_UDAs)) %>% #filters out contracts with no data on contracted UDAs
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA = annual_contracted_UDAs, 
             UDA_delivered = total_UDAs)
    
    data <- bind_rows(data, historic_data)

  }
  
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
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UDA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "Scheduled monthly percentage of usual annual contracted UDAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UDAs submitted"
    captionTitle <- "*Excluding prototype contracts and those with annual contracted UDA < 100 
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
    lineCol <- "coral"
    lineCol <- "#CC79A7"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
    juneTarget <- 95
  }else{
    #get data into the right format
    data <- get_delivery_data(data, remove_prototypes, UDAorUOA = "UOA", all_regions_and_STPs = all_regions_and_STPs)
    title <- "Scheduled monthly percentage of usual annual contracted UOAs \nsubmitted across all contracts* scaled up to 12 months**"
    ylab <- "% of contracted UOAs submitted"
    captionTitle <- "*Excluding prototype contracts and with zero annual contracted UOAs 
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
    lineCol <- "steelblue"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    juneTarget <- 100
  }
  
  if(plotChart == TRUE){
  #plot code
  p <- ggplot(data) +
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
    geom_segment(aes(x = as.Date("2022-04-01"), y = juneTarget, xend = as.Date("2022-06-01"), yend = juneTarget),
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
    annotate(geom = "text", 
             x = as.Date("2022-04-01") + lubridate::weeks(2), 
             y = juneTarget - 5, 
             label = "Q1 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(limits = c(0, max(c(data$perc_UDA_UOA_delivered, 95), na.rm = T) + 5)) +
    labs(title = title, 
         x = "Month",
         y = ylab, 
         subtitle = subtitle,
         caption = captionTitle) +
    annotate(geom = "text",
             x = data$month,
             y = data$perc_UDA_UOA_delivered + 3,
             label = paste0(data$perc_UDA_UOA_delivered, "%"),
             size = 3) +
    theme(axis.text.x = element_text(angle = 90))
  
    p
    
  }else{
    
    if(UDAorUOA == "UDA"){
      new_col_names <- c(`Schedule month` = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UDAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UDAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UDAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled monthly percentage of contracted UDAs delivered` = "perc_UDA_UOA_delivered")
    }else{
      new_col_names <- c(`Schedule month` = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UOAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UOAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UOAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled monthly percentage of contracted UOAs delivered` = "perc_UDA_UOA_delivered")
    }
    
    
    data <- data %>%
      rename(any_of(new_col_names))
  }
}

################################################################################
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
    left_join(demographics_data, by = "contract_number")
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 100)
  }
  
  #scale up April by 18
  scaleFactor <- if_else(plot_month == as.Date("2022-04-01"), 18, 12)
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * scaleFactor / annual_contracted_UDA) 
  
  
  data_to_plot <- data %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Corporate.Status)) %>%
    arrange(Corporate.Status)
  
  
  if(is.null(get_num_above)){
    p <- ggplot(data_to_plot, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
      geom_point(aes(colour = Corporate.Status)) +
      theme_bw() +
      geom_hline(yintercept = 0.95,
                 colour = "orangered4",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA) - 8000,
               y = 0.87,
               label = "95% Q1 threshold",
               size = 3,
               colour = "orangered4") +
      geom_hline(yintercept = 1,
                 colour = "grey40",
                 linetype = "dashed") +
      annotate(geom = "text",
               x = max(data_to_plot$annual_contracted_UDA),
               y = 1.08,
               label = "100%",
               size = 3,
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
      labs(title = "UDA contract size Vs UDA delivery scaled up 12** months",
           subtitle = paste(format(plot_month, "%B %Y"), "scheduled delivery -", subtitle),
           x = "Annual contracted UDAs",
           y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
           caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         Also excluding contracts with delivery > 300% for plot purposes.
           **April is scaled up by 18 due to short schedule period for April",
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
#function to plt delivery profile month by month 
plot_UDA_UOA_delivery_profile <- function(data = UDA_scheduled_data, 
                                          calendar_data = UDA_calendar_data,
                                          historic_data = historical_UDA_scheduled_data,
                                          UDAorUOA = "UDA",
                                          level = "National",
                                          region_STP_name = NULL,
                                          plotChart = TRUE,
                                          all_regions_and_STPs = FALSE,
                                          include_historic = TRUE){
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #bind in historic data if required
  if(include_historic == TRUE & UDAorUOA == "UDA"){
    data <- data %>%
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, UDA_delivered)
    
    historic_data <- historic_data %>%
      mutate(month = as.Date(month)) %>%
      filter(!is.na(annual_contracted_UDAs)) %>% #filters out contracts with no data on contracted UDAs
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA = annual_contracted_UDAs, 
             UDA_delivered = total_UDAs)
    
    data <- bind_rows(data, historic_data)
    
  }
  
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
  data <- get_delivery_profile_data(data = data, UDAorUOA = UDAorUOA, remove_prototypes = T, all_regions_and_STPs = all_regions_and_STPs)
  
  #change title for UDA or UOA
  if(UDAorUOA == "UDA"){
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UDA per month"
    legTitle <- "Performance band of \nUDA delivery"
    captionTitle <- "*Excluding prototype contracts and those with annual contracted UDA < 100 
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
  }else{
    title <- "Proportion of contracts delivering in each performance band \nof total contracted UOA per month"
    legTitle <- "Performance band of \nUOA delivery"
    captionTitle <- "*Excluding prototype contracts and those with zero annual contracted UOAs 
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
  }
  
  #get bars in the correct order
  data$performance_band <- factor(data$performance_band, levels = c("0-9%","10-19%", 
                                                                    "20-29%", "30-39%", "40-49%", "50-59%",
                                                                    "60-69%", "70-79%", "80-89%", "90-99%",
                                                                    "100% +"))
  #colour blind friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                 "#52854C", "#4E84C4", "#293352", "#FFDB6D")
  
  if(plotChart == TRUE){
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
           subtitle = subtitle,
           caption = captionTitle) +
      #scale_fill_manual(values = cbPalette) +
      scale_x_datetime(breaks = data$month, 
                       labels = scales::date_format("%b-%y")) +
      geom_vline(xintercept = as.Date("2020-07-01"), colour = "black", size = 5) +
      scale_y_continuous(limits = c(0, 55)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }else{
    data %>%
      mutate(month = as.Date(month)) %>%
      rename(Month = "month", 
             `Region Name` = "region_name", 
             `Commissioner Name` = "commissioner_name", 
             `Performance band` = "performance_band", 
             `Number of contracts in performance band`= "n", 
             `Total number of contracts` = "no_of_contracts", 
             `Percentage of contracts in performance band` = "perc_of_contracts")
  }
  
}

################################################################################
plot_banded_CoT <- function(data = UDA_scheduled_data, 
                            calendar_data = UDA_calendar_data,
                            historic_data = historical_UDA_scheduled_data, 
                            level = "National",
                            region_STP_name = NULL,
                            plotChart = TRUE, 
                            all_regions_and_STPs = FALSE,
                            asIndex = FALSE){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  #historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
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
  data_to_print <- get_banded_COTs_data(data, historic_data = historic_data, remove_prototypes = F, all_regions_and_STPs = all_regions_and_STPs)
  data <- reshape2::melt(data_to_print, id.vars = "month")
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    rename(band = variable, CoTs = value)
  
  #set everything as indext of April 2019
  if(asIndex == TRUE){
    
    april2019 <- data %>%
      filter(month == as.Date("2019-04-01")) %>%
      select(-month,
             april_CoTs = CoTs)
    
    data <- data %>%
      left_join(april2019, by = "band") %>%
      mutate(CoTs = CoTs * 100 / april_CoTs)
    
    title <- "Banded Courses of Treatment as a Percentage of April 2019 Delivery"
    ylab <- "Percentage of April 2019 FP17* forms submitted"
    
  }else{
    title <- "Banded Courses of Treatment"
    ylab <- "Number of FP17* forms submitted"
  }
  
  if(plotChart == TRUE){

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
      labs(title = title,
           x = "Month",
           y = ylab,
           colour = "Band",
           subtitle = subtitle,
           caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs,
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2
         UDAs and an 'other' FP17 = 0.6 UDAs. Scheduled data used.")

  }else{
    data_to_print %>%
      mutate(month = as.Date(month)) %>%
      rename(Month = month,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Band 1` = band1,
             `Band 2` = band2,
             `Band 3` = band3,
             `Other` = other,
             `Urgent` = urgent)
  }

}


################################################################################
#function to create graph on slide 9
#current data source: Dental activity annual running total.xlsx sent by email by Caroline
plot_urgent_form_submissions <- function(data = UDA_scheduled_data, 
                                         calendar_data = UDA_calendar_data,
                                         historic_data = historical_UDA_scheduled_data, 
                                         level = "National",
                                         region_STP_name = "Cheshire and Merseyside STP",
                                         plotChart = TRUE,
                                         all_regions_and_STPs = FALSE){
  
  #avoid standard form on axes
  options(scipen = 100)
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, name_or_company_name, commissioner_name, region_name) %>%
    distinct()
  
  data <- left_join(data, region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
  
  #add a region column to the historic data
  #historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
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
  data <- get_banded_COTs_data(data, historic_data = historic_data, remove_prototypes = F, all_regions_and_STPs = all_regions_and_STPs)
  data <- data %>%
    mutate(date = as.Date(month)) %>%
    mutate(financial_year = case_when(month >= as.Date("2019-04-01") & month < as.Date("2020-04-01") ~ "2019/20",
                                      month >= as.Date("2020-04-01") & month < as.Date("2021-04-01") ~ "2020/21",
                                      month >= as.Date("2021-04-01") & month < as.Date("2022-04-01") ~ "2021/22",
                                      month >= as.Date("2022-04-01") & month < as.Date("2023-04-01") ~ "2022/23"
    ))
  
  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    geom_line(aes(x = factor(lubridate::month(date, label=TRUE, abbr=TRUE),
                             levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
                  y = urgent,
                  group = factor(financial_year),
                  colour = factor(financial_year)),
              size = 1) +
    geom_point(aes(x = factor(lubridate::month(date, label=TRUE, abbr=TRUE),
                              levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")),
                   y = urgent,
                   group = factor(financial_year),
                   colour = factor(financial_year))) +
    scale_y_continuous(breaks = scales::breaks_pretty(),
                       limits = c(0, max(data$urgent))) +
    labs(title = "Urgent treatment form submissions",
         x = "Month",
         y = "Number of urgent FP17* forms submitted",
         colour = "Financial year",
         subtitle = subtitle,
         caption = "*UDA to FP17 conversion has been done assuming a band 1 FP17 
         is equivalent to 1 UDA, a band 2 FP17 = 3 UDAs, 
         a band 3 FP17 = 12 UDAs, an urgent FP17 = 1.2 
         UDAs and an 'other' FP17 = 0.6 UDAs. Scheduled data used.")
  
  if(plotChart == TRUE){
    p
  }else{
    
    new_cols <- c(Month = "month",
                  `Region Name` = "region_name",
                  `Commissioner Name` = "commissioner_name",
                  `Urgent forms` = "urgent",
                  `Financial year` = "financial_year")

    data %>% 
      mutate(month = as.Date(month)) %>%
      select(-c(band1, band2, band3, other, date)) %>%
      rename(any_of(new_cols))
  }
  
  
}

################################################################################
plot_111_referrals <- function(data = dental_data_111,
                               plotChart = TRUE){
  
  data <- data %>% 
    mutate(month = as.Date(month)) %>%
    group_by(month) %>%
    summarise(monthly_case_volume = sum(monthly_case_volume))
  
  #plot code
  p <- ggplot(data) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_line(aes(x = month, 
                  y = monthly_case_volume), 
              colour = "steelblue", 
              size = 1) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, 
                                    max(data$monthly_case_volume, na.rm = T) + 10000,
                                    10000),
                       limits = c(0, max(data$monthly_case_volume))) +
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
  
  if(plotChart == TRUE){
    p
  }else{
    data
  }
  
}


################################################################################  
plot_breakdown_111_referrals <- function(data = dental_data_111,
                                         plotChart = TRUE){
  
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
  p <- ggplot(data) +
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
  
  if(plotChart == TRUE){
    p
  }else{
    data %>%
      select(`Month` = month,
             `Disposition` = disposition_text,
             `Monthly case volume` = monthly_case_volume)
  }
  
}


################################################################################
#function to plot unique patients by band
plot_unique_patients_rolling <- function(data = unique_patients_rolling,
                                         calendar_data = UDA_calendar_data,
                                         scheduled_data = UDA_scheduled_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         plotChart = TRUE,
                                         remove_prototypes = TRUE,
                                         get_perc = FALSE,
                                         all_regiona_and_STPs = FALSE,
                                         asIndex = FALSE){
  
  #avoid standard form notation
  options(scipen = 5)
  
  data <- data %>%
    rename(month = month_ending)
  
  #join in region and STP data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, commissioner_name, region_name) %>%
    unique()
  
  #join annual contracted UDAs
  contracted_UDAs <- scheduled_data %>%
    filter(month == max(scheduled_data$month, na.rm = TRUE)) %>%
    select(contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(region_STP_lookup, by = "contract_number") %>%
    left_join(contracted_UDAs, by = "contract_number")
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UDA > 100)
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
  
  if(all_regiona_and_STPs == TRUE){
    data <- data %>%
      group_by(month, region_name, commissioner_name)
  }else{
    data <- data %>%
      group_by(month)
  }
  
  data_to_print <- data %>%
    summarise(unique_patients_rolling_12M = sum(unique_patients_rolling_12M, na.rm = TRUE),
              band1_unique_patients_rolling_12M = sum(band1_unique_patients_rolling_12M, na.rm = TRUE),
              band2_or_3_unique_patients_rolling_12M = sum(band2_or_3_unique_patients_rolling_12M, na.rm = TRUE),
              band1_urgent_unique_patients_rolling_12M = sum(band1_urgent_unique_patients_rolling_12M, na.rm = TRUE),
              band_other_unique_patients_rolling_12M = sum(band_other_unique_patients_rolling_12M, na.rm = TRUE)) 
  
  data <- data_to_print %>%
    pivot_longer(c(unique_patients_rolling_12M,
                   band1_unique_patients_rolling_12M,
                   band2_or_3_unique_patients_rolling_12M,
                   band1_urgent_unique_patients_rolling_12M,
                   band_other_unique_patients_rolling_12M)) %>%
    rename(band = name, total_unique_patients = value) %>%
    mutate(band = case_when(band == "unique_patients_rolling_12M" ~ "Any band",
                            band == "band1_unique_patients_rolling_12M" ~ "Band 1",
                            band == "band2_or_3_unique_patients_rolling_12M" ~ "Band 2 or 3",
                            band == "band1_urgent_unique_patients_rolling_12M" ~ "Urgent band 1",
                            band == "band_other_unique_patients_rolling_12M" ~ "Other band")) %>%
    mutate(month = as.Date(month))
  
  data$band <- factor(data$band,
                      levels = c(
                        "Any band",
                        "Band 1",
                        "Band 2 or 3",
                        "Urgent band 1",
                        "Other band"
                      ))
  
  #set everything as indext of April 2019
  if(asIndex == TRUE){
    
    april2018 <- data %>%
      filter(month == as.Date("2018-04-01")) %>%
      select(-month,
             april_total_unique_patients = total_unique_patients)
    
    data <- data %>%
      left_join(april2018, by = "band") %>%
      mutate(total_unique_patients = total_unique_patients * 100 / april_total_unique_patients)
    
    title <- "Total number of unique patients seen over a rolling 12 month period by band \nas a percentage of April 2018 figures"
    ylab <- "Percentage of April 2018 unique patients"
    
  }else{
    title <- "Total number of unique patients seen over a rolling 12 month period by band"
    ylab <- "Unique patients"
  }
  
  
  if(plotChart == TRUE & get_perc == FALSE){
    #plot code
    ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month,
                    y = total_unique_patients,
                    colour = band)) +
      geom_point(aes(x = month,
                     y = total_unique_patients,
                     colour = band)) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(title = title,
           x = "12 month rolling period end date",
           y = ylab,
           subtitle = subtitle,
           caption = "*N.B. this analysis uses unique patients per contract** and does not take \ninto account patients who have been seen at more than one dental practice. \n**Excluding prototype contracts and those with annual contracted UDAs < 100."
      ) +
      scale_x_date(date_breaks = "1 month", 
                   date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90))
    #theme(legend.position = "bottom")
  }else if(plotChart == FALSE & get_perc == FALSE){
    data_to_print <- data_to_print %>%
      mutate(month = as.Date(month)) %>%
      rename(`12 month period end` = month,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Any band unique patients rolling 12 month` = unique_patients_rolling_12M,
             `Band 1 unique patients rolling 12 month` = band1_unique_patients_rolling_12M,
             `Band 2 or 3 unique patients rolling 12 month` = band2_or_3_unique_patients_rolling_12M,
             `Urgent band 1 unique patients rolling 12 month` = band1_urgent_unique_patients_rolling_12M,
             `Other unique patients rolling 12 month` = band_other_unique_patients_rolling_12M
             )
  }else{
    #percentage of pre-covid levels
    data <- data %>%
      filter(band == "Any band") %>%
      arrange(month)
    
    round(data$total_unique_patients[nrow(data)] * 100 / data$total_unique_patients[1])
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
    left_join(contractor_cats, by = "contract_number") %>%
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
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)###############
  }
  
  data <- data %>%
    filter(month == max(data$month))
  
  nrow(data)
}



################################################################################
get_Q4_num_contracts_on_target <- function(data = UDA_calendar_data, 
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
                                                   month < as.Date("2022-01-01") ~ 0.65 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-04-01") ~ 0.85 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-01-01") ~ 0.85 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-04-01") ~ 0.90 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2022-01-01"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2022-01-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2022-02-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2022-03-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                #mean_UDA_target = mean(UDA_financial_half_target),
                YTD_UDA_delivered = sum(UDA_total)) %>%
      mutate(mean_Q4_UDA_target = mean_annual_contracted_UDA * 0.85 / 4) %>%
      count(YTD_UDA_delivered >= (mean_Q4_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                #mean_Q3_UOA_target = mean(UOA_financial_half_target),
                quarter_to_date_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q4_UOA_target = mean_annual_contracted_UOA * 0.90 / 4) %>%
      count(quarter_to_date_UOA_delivered >= (mean_Q4_UOA_target) * month_factor / 3)
  }
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)

}


################################################################################
get_Q1_22_num_contracts_on_target <- function(data = UDA_calendar_data, 
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
                                                   month < as.Date("2022-01-01") ~ 0.65 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-04-01") ~ 0.85 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-07-01") ~ 0.95 * annual_contracted_UDA/4,
                                                   month < as.Date("2022-09-01") ~ 1 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-01-01") ~ 0.85 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-04-01") ~ 0.90 * annual_contracted_UOA/4,
                                                   month < as.Date("2022-09-01") ~ 1 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2022-07-01")) ##must update this at start of each quarter
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UOA > 0)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2022-07-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2022-08-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2022-09-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                quarter_to_date_UDA_delivered = sum(UDA_total)) %>%
      mutate(mean_Q4_UDA_target = mean_annual_contracted_UDA * 1 / 4) %>%
      count(quarter_to_date_UDA_delivered >= (mean_Q4_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                quarter_to_date_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q4_UOA_target = mean_annual_contracted_UOA * 1 / 4) %>%
      count(quarter_to_date_UOA_delivered >= (mean_Q4_UOA_target) * month_factor / 3)
  }
  
  
 no_on_target <- data[2, "n"]
 as.integer(no_on_target)
  
}


################################################################################
get_num_urgent_forms <- function(data = UDA_scheduled_data, 
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
  
  data <- get_banded_COTs_data(data = data, 
                                 historic_data = historic_data,
                                 remove_prototypes = remove_prototypes)
  
  data <- data %>% 
    filter(month == max(data$month))
  
  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)
  
}


################################################################################
get_num_urgent_forms_2019 <- function(data = UDA_scheduled_data, 
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
  #historic_data <- left_join(historic_data, region_STP_lookup, by = c("contract_number"))
  
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
  
  data <- get_banded_COTs_data(data = data, 
                                 historic_data = historic_data,
                                 remove_prototypes = remove_prototypes)
  
  data <- data %>%
    filter(month == max(data$month) - lubridate::years(3))

  num_urgent_forms <- data[1, "urgent"]
  as.integer(num_urgent_forms)

}

  
################################################################################
plot_patient_recalls <- function(data = dental_recalls_STP_2018_22,
                                 treatment_band = "Band_1",
                                 level = "National",
                                 region_STP_name = NULL,
                                 calendar_data = UDA_calendar_data,
                                 year = "2021/22",
                                 plotChart = TRUE){
  
  data <- clean_dental_recalls(data)
  
  data <- data %>% 
    filter(financial_year == year) %>%
    filter(Band == treatment_band) 
  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(region_STP_lookup, by = "commissioner_name")
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
    
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
    
  }
  
  data <- data %>%
    group_by(Months_Last_Visit, patient_group) %>% 
    summarise(forms = sum(forms, na.rm = TRUE))
  
  data$Months_Last_Visit <- factor(data$Months_Last_Visit,
                                   levels = c("1 Month",
                                              "2 Months",
                                              "3 Months",
                                              "4 Months",
                                              "5 Months",
                                              "6 Months",
                                              "7 Months",
                                              "8 Months",
                                              "9 Months",
                                              "10 Months",
                                              "11 Months",
                                              "12 Months",
                                              "12-18 Months",
                                              "19-24 Months",
                                              "No Previous Visit"
                                              ))
  
  if(plotChart == TRUE){
    ggplot(data) +
      geom_col(aes(x = Months_Last_Visit,
                   y = forms,
                   fill = patient_group),
               position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
      scale_fill_manual(values = c("royalblue2", "limegreen")) +
      labs(title = paste0(year, " - Adult and Child Re-attendance Intervals"),
           subtitle = paste0(str_replace(treatment_band, "_", " "), "\n", region_STP_name),
           x = "Months since last visit",
           y = "Total form count",
           fill = "") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
  }else{
    data
  }

  
}

################################################################################
plot_patient_recalls_facet <- function(data = dental_recalls_STP_2018_22,
                                       ICB_lookup = STP_ICB_lookup,
                                 treatment_band = "Band_1",
                                 level = "National",
                                 region_STP_name = NULL,
                                 calendar_data = UDA_calendar_data,
                                 plotChart = TRUE){

  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  #sort out STP ICB issue
  ICB_lookup <- ICB_lookup %>%
    rename(commissioner_name = commissioner_name_STP)
  
  #for output when plotChart == FALSE
  data_output <- data %>%
    left_join(ICB_lookup, by = "commissioner_name") %>%
    mutate(commissioner_name = commissioner_name_ICB) %>%
    select(-commissioner_name_ICB) %>%
    left_join(region_STP_lookup, by = "commissioner_name")
  
  data <- clean_dental_recalls(data_output)
  
  data <- data %>% 
    filter(Band == treatment_band) 
  
  subtitle <- "England"

  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)

    subtitle <- region_STP_name

  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)

    subtitle <- region_STP_name

  }

  data <- data %>%
    group_by(Months_Last_Visit, patient_group, financial_year) %>%
  summarise(forms = sum(forms, na.rm = TRUE))

  data$Months_Last_Visit <- factor(data$Months_Last_Visit,
                                   levels = c("1 Month",
                                              "2 Months",
                                              "3 Months",
                                              "4 Months",
                                              "5 Months",
                                              "6 Months",
                                              "7 Months",
                                              "8 Months",
                                              "9 Months",
                                              "10 Months",
                                              "11 Months",
                                              "12 Months",
                                              "12-18 Months",
                                              "19-24 Months",
                                              "No Previous Visit"
                                   ))

  if(plotChart == TRUE){
    ggplot(data) +
      geom_col(aes(x = Months_Last_Visit,
                   y = forms,
                   fill = patient_group),
               position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
      scale_fill_manual(values = c("royalblue2", "limegreen")) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(title = "Adult and Child Re-attendance Intervals",
           subtitle = paste0(str_replace(treatment_band, "_", " "), "\n", region_STP_name),
           x = "Months since last visit",
           y = "Total form count",
           fill = "") +
      facet_wrap(vars(financial_year),
                 #scales = "free_y",
                 nrow = 2)
  }else{

    data_output <- data_output %>%
      select(`Financial Year` = financial_year,
             `Region Name` = region_name,
             `Commissioner Name` = commissioner_name,
             `Patient Group` = patient_group,
             `Months Since Last Visit` = Months_Last_Visit,
             `Band 1` = Band_1,
             `Band 2` = Band_2,
             `Band 3` = Band_3,
             Other,
             Urgent
             )

  }

}

################################################################################
plot_workforce_returns <- function(data = dental_workforce_returns,
                                   level = "National",
                                   region_STP_name = NULL,
                                   calendar_data = UDA_calendar_data){
  
  data <- clean_workforce_returns(data)

  
  #add a region column to the data
  region_STP_lookup <- calendar_data %>%
    select(commissioner_name, region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(region_STP_lookup, by = "contract_number")
  
  subtitle <- "England"
  
  #only filter for regional or STP level
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
    
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  data <- data %>%
    group_by(date) %>%
    summarise(total_workforce_returns_due = sum(total_workforce_returns_due, na.rm = TRUE),
              total_workforce_returns_submitted = sum(total_workforce_returns_submitted, na.rm = TRUE),
              workforce_returns = sum(workforce_returns, na.rm = TRUE)
              ) %>%
    mutate(monthly_workforce_returns_due = total_workforce_returns_due / 12) %>%
    select(date, monthly_workforce_returns_due, workforce_returns) %>%
    pivot_longer(cols = c("monthly_workforce_returns_due", "workforce_returns"))
    
  ggplot(data) +
    geom_line(aes(x = date,
                  y = value,
                  colour = name)) +
    theme_bw() +
    scale_colour_manual(values = c("coral", "steelblue"),
                        labels = c("Monthly returns due", "Monthly returns submitted")) +
    labs(title = "Total monthly workforce returns",
         subtitle = subtitle,
         x = "Month",
         y = "Number of returns",
         colour = "") +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) 
}


################################################################################
plot_delivery_vs_workforce_returns <- function(data = UDA_scheduled_data,
                                               calendar_data = UDA_calendar_data,
                                               remove_prototypes = T,
                                               plot_month = NULL,
                                               level = "National",
                                               region_STP_name = NULL,
                                               workforce_data = dental_workforce_returns){
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
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  data <- data %>%
    filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
    filter(annual_contracted_UDA > 100)
  
  #scale up April by 18
  scaleFactor <- if_else(plot_month == as.Date("2022-04-01"), 18, 12)
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered) %>%
    mutate(UDA_delivery = UDA_delivered * scaleFactor / annual_contracted_UDA) 
  
  #get workforce returns data
  workforce_data <- clean_workforce_returns(workforce_data)
  workforce_data <- workforce_data  %>%
    mutate(perc_workforce_returns_submitted = total_workforce_returns_submitted / total_workforce_returns_due) %>%
    select(contract_number, perc_workforce_returns_submitted)
  
  data <- data %>%
    left_join(workforce_data, by = "contract_number")
    

  p <- ggplot(data) +
    geom_point(aes(x = perc_workforce_returns_submitted, y = UDA_delivery)) +
    theme_bw() +

    # scale_x_continuous(breaks = seq(0, 175000, 20000),
    #                    limits = c(0, max(data_to_plot$annual_contracted_UDA) + 5000),
    #                    #labels = scales::percent_format(accuracy = 1)
    #                    labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    # ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Percentage of expected workforce returns submitted \nVs UDA delivery scaled up 12** months",
         subtitle = paste(format(plot_month, "%B %Y"), "scheduled delivery -", subtitle),
         x = "Percentage of expected workforce returns submitted",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         Also excluding contracts with delivery > 300% for plot purposes.
           **April is scaled up by 18 due to short schedule period for April"
    )

    p

}


################################################################################
plot_CDS_patients_waiting_for_assessment <- function(data = CDS_data,
                                                     calendar_data = UDA_calendar_data,
                                                     level = "National",
                                                     region_STP_name = NULL,
                                                     all_regions_and_STPs = FALSE){

  data <- data %>% 
    rename(region_name = NHSREGION,
           quarter = Month)
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
    
  data <- data %>%
    select(quarter,
           region_name,
           routine_num_patients_waiting_for_first_assessment_adult, 
           routine_num_patients_waiting_for_first_assessment_child,
           GA_num_patients_waiting_for_first_assessment_adult,
           GA_num_patients_waiting_for_first_assessment_child,
           SS_num_patients_waiting_for_first_assessment_adult,
           SS_num_patients_waiting_for_first_assessment_child
    ) %>%
    mutate(routine_num_patients_waiting_for_first_assessment_adult = as.numeric(routine_num_patients_waiting_for_first_assessment_adult), 
           routine_num_patients_waiting_for_first_assessment_child = as.numeric(routine_num_patients_waiting_for_first_assessment_child),
           GA_num_patients_waiting_for_first_assessment_adult = as.numeric(GA_num_patients_waiting_for_first_assessment_adult),
           GA_num_patients_waiting_for_first_assessment_child = as.numeric(GA_num_patients_waiting_for_first_assessment_child),
           SS_num_patients_waiting_for_first_assessment_adult = as.numeric(SS_num_patients_waiting_for_first_assessment_adult),
           SS_num_patients_waiting_for_first_assessment_child = as.numeric(SS_num_patients_waiting_for_first_assessment_child)
    ) 
  
    
    if(all_regions_and_STPs == TRUE){
      data <- data %>%
        group_by(quarter, region_name)
    }else{
      data <- data %>%
        group_by(quarter)
    }
     
    data <- data %>%
      summarise(routine_adult = mean(routine_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
                routine_child = mean(routine_num_patients_waiting_for_first_assessment_child, na.rm = TRUE),
                GA_adult = mean(GA_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
                GA_child = mean(GA_num_patients_waiting_for_first_assessment_child, na.rm = TRUE),
                SS_adult = mean(SS_num_patients_waiting_for_first_assessment_adult, na.rm = TRUE),
                SS_child = mean(SS_num_patients_waiting_for_first_assessment_child, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_longer(cols = c(routine_adult,
                            routine_child,
                            GA_adult,
                            GA_child,
                            SS_adult,
                            SS_child),
                   names_to = "variable",
                   values_to = "patients_waiting") %>%
      mutate(patient_group = sub(".*\\_", "", variable),
             appointment_type = sub("\\_.*", "", variable))



    ggplot(data) +
      geom_line(aes(x = quarter,
                    y = patients_waiting,
                    colour = appointment_type)) +
      geom_point(aes(x = quarter,
                    y = patients_waiting,
                    colour = appointment_type
                    )) +
      ggrepel::geom_text_repel(aes(x = quarter,
                          y = patients_waiting,
                          colour = appointment_type,
                          label = round(patients_waiting)),
        size=3, box.padding = unit(0.2, "lines")
      ) +
      theme_bw() +
      labs(title = "Quarterly mean number of patients per contract waiting for first assessment",
           subtitle = subtitle,
           x = "Quarter start date",
           y = "Mean number of paitings waiting",
           colour = "") +
      facet_wrap(vars(patient_group),
                 #scales = "free_y",
                 nrow = 1) +
      theme(axis.text.x = element_text(angle = 90))


  
}


################################################################################
plot_CDS_patients_waiting_for_treatment <- function(data = CDS_data,
                                                     calendar_data = UDA_calendar_data,
                                                     level = "National",
                                                     region_STP_name = NULL,
                                                     all_regions_and_STPs = FALSE){
  
  data <- data %>% 
    rename(region_name = NHSREGION,
           quarter = Month)
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  data <- data %>%
    select(quarter,
           region_name,
           routine_num_patients_waitng_for_first_treatment_adult, 
           routine_num_patients_waitng_for_first_treatment_child,
           GA_num_patients_waitng_for_first_treatment_adult,
           GA_num_patients_waitng_for_first_treatment_child,
           SS_num_patients_waitng_for_first_treatment_adult,
           SS_num_patients_waitng_for_first_treatment_child
    ) %>%
    mutate(routine_num_patients_waitng_for_first_treatment_adult = as.numeric(routine_num_patients_waitng_for_first_treatment_adult), 
           routine_num_patients_waitng_for_first_treatment_child = as.numeric(routine_num_patients_waitng_for_first_treatment_child),
           GA_num_patients_waitng_for_first_treatment_adult = as.numeric(GA_num_patients_waitng_for_first_treatment_adult),
           GA_num_patients_waitng_for_first_treatment_child = as.numeric(GA_num_patients_waitng_for_first_treatment_child),
           SS_num_patients_waitng_for_first_treatment_adult = as.numeric(SS_num_patients_waitng_for_first_treatment_adult),
           SS_num_patients_waitng_for_first_treatment_child = as.numeric(SS_num_patients_waitng_for_first_treatment_child)
    ) 
  
  
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(quarter, region_name)
  }else{
    data <- data %>%
      group_by(quarter)
  }
  
  data <- data %>% 
    summarise(routine_adult = mean(routine_num_patients_waitng_for_first_treatment_adult, na.rm = TRUE), 
              routine_child = mean(routine_num_patients_waitng_for_first_treatment_child, na.rm = TRUE),
              GA_adult = mean(GA_num_patients_waitng_for_first_treatment_adult, na.rm = TRUE),
              GA_child = mean(GA_num_patients_waitng_for_first_treatment_child, na.rm = TRUE),
              SS_adult = mean(SS_num_patients_waitng_for_first_treatment_adult, na.rm = TRUE),
              SS_child = mean(SS_num_patients_waitng_for_first_treatment_child, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = c(routine_adult, 
                          routine_child,
                          GA_adult,
                          GA_child,
                          SS_adult,
                          SS_child),
                 names_to = "variable",
                 values_to = "patients_waiting") %>%
    mutate(patient_group = sub(".*\\_", "", variable),
           appointment_type = sub("\\_.*", "", variable))
  
  
  
  ggplot(data) +
    geom_line(aes(x = quarter,
                  y = patients_waiting,
                  colour = appointment_type)) +
    geom_point(aes(x = quarter,
                   y = patients_waiting,
                   colour = appointment_type)) +
    ggrepel::geom_text_repel(aes(x = quarter,
                                 y = patients_waiting,
                                 colour = appointment_type,
                                 label = round(patients_waiting)),
                             size=3, box.padding = unit(0.2, "lines")
    ) +
    theme_bw() +
    labs(title = "Quarterly mean number of patients per contract waiting for first treatment",
         subtitle = subtitle,
         x = "Quarter start date",
         y = "Mean number of paitings waiting",
         colour = "") +
    facet_wrap(vars(patient_group), 
               #scales = "free_y", 
               nrow = 1) +
    theme(axis.text.x = element_text(angle = 90))
  
  
  
}


################################################################################
plot_CDS_wait_times_for_assessment <- function(data = CDS_data,
                                                    calendar_data = UDA_calendar_data,
                                                    level = "National",
                                                    region_STP_name = NULL,
                                                    all_regions_and_STPs = FALSE){
  
  data <- data %>% 
    rename(region_name = NHSREGION,
           quarter = Month)
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  data <- data %>%
    select(quarter,
           region_name,
           routine_average_weeks_waiting_for_assessment_adult, 
           routine_averag_weeks_waiting_for_assessment_child,
           GA_average_weeks_waiting_for_assessment_adult,
           GA_averag_weeks_waiting_for_assessment_child,
           SS_average_weeks_waiting_for_assessment_adult,
           SS_averag_weeks_waiting_for_treatment_child
    ) %>%
    mutate(routine_average_weeks_waiting_for_assessment_adult = as.numeric(routine_average_weeks_waiting_for_assessment_adult), 
           routine_averag_weeks_waiting_for_assessment_child = as.numeric(routine_averag_weeks_waiting_for_assessment_child),
           GA_average_weeks_waiting_for_assessment_adult = as.numeric(GA_average_weeks_waiting_for_assessment_adult),
           GA_averag_weeks_waiting_for_assessment_child = as.numeric(GA_averag_weeks_waiting_for_assessment_child),
           SS_average_weeks_waiting_for_assessment_adult = as.numeric(SS_average_weeks_waiting_for_assessment_adult),
           SS_averag_weeks_waiting_for_treatment_child = as.numeric(SS_averag_weeks_waiting_for_treatment_child)
    ) 
  
  
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(quarter, region_name)
  }else{
    data <- data %>%
      group_by(quarter)
  }
  
  data <- data %>% 
    summarise(routine_adult = mean(routine_average_weeks_waiting_for_assessment_adult, na.rm = TRUE), 
              routine_child = mean(routine_averag_weeks_waiting_for_assessment_child, na.rm = TRUE),
              GA_adult = mean(GA_average_weeks_waiting_for_assessment_adult, na.rm = TRUE),
              GA_child = mean(GA_averag_weeks_waiting_for_assessment_child, na.rm = TRUE),
              SS_adult = mean(SS_average_weeks_waiting_for_assessment_adult, na.rm = TRUE),
              SS_child = mean(SS_averag_weeks_waiting_for_treatment_child, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = c(routine_adult, 
                          routine_child,
                          GA_adult,
                          GA_child,
                          SS_adult,
                          SS_child),
                 names_to = "variable",
                 values_to = "weeks_waiting") %>%
    mutate(patient_group = sub(".*\\_", "", variable),
           appointment_type = sub("\\_.*", "", variable))
  
  
  
  ggplot(data) +
    geom_line(aes(x = quarter,
                  y = weeks_waiting,
                  colour = appointment_type)) +
    geom_point(aes(x = quarter,
                   y = weeks_waiting,
                   colour = appointment_type)) +
    ggrepel::geom_text_repel(aes(x = quarter,
                                 y = weeks_waiting,
                                 colour = appointment_type,
                                 label = round(weeks_waiting)),
                             size=3, box.padding = unit(0.2, "lines")
    ) +
    theme_bw() +
    labs(title = "Quarterly mean number of weeks waiting for first assessment",
         subtitle = subtitle,
         x = "Quarter start date",
         y = "Mean number of paitings waiting",
         colour = "") +
    facet_wrap(vars(patient_group), 
               #scales = "free_y", 
               nrow = 1) +
    theme(axis.text.x = element_text(angle = 90))
  
  
  
}



################################################################################
plot_CDS_wait_times_for_treatment <- function(data = CDS_data,
                                               calendar_data = UDA_calendar_data,
                                               level = "National",
                                               region_STP_name = NULL,
                                               all_regions_and_STPs = FALSE){
  
  data <- data %>% 
    rename(region_name = NHSREGION,
           quarter = Month)
  
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  data <- data %>%
    select(quarter,
           region_name,
           routine_averag_weeks_waiting_for_treatment_adult, 
           routine_averag_weeks_waiting_for_treatment_child,
           GA_averag_weeks_waiting_for_treatment_adult,
           GA_averag_weeks_waiting_for_treatment_child,
           SS_averag_weeks_waiting_for_treatment_adult,
           SS_averag_weeks_waiting_for_treatment_child
    ) %>%
    mutate(routine_averag_weeks_waiting_for_treatment_adult = as.numeric(routine_averag_weeks_waiting_for_treatment_adult), 
           routine_averag_weeks_waiting_for_treatment_child = as.numeric(routine_averag_weeks_waiting_for_treatment_child),
           GA_averag_weeks_waiting_for_treatment_adult = as.numeric(GA_averag_weeks_waiting_for_treatment_adult),
           GA_averag_weeks_waiting_for_treatment_child = as.numeric(GA_averag_weeks_waiting_for_treatment_child),
           SS_averag_weeks_waiting_for_treatment_adult = as.numeric(SS_averag_weeks_waiting_for_treatment_adult),
           SS_averag_weeks_waiting_for_treatment_child = as.numeric(SS_averag_weeks_waiting_for_treatment_child)
    ) 
  
  
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(quarter, region_name)
  }else{
    data <- data %>%
      group_by(quarter)
  }
  
  data <- data %>% 
    summarise(routine_adult = mean(routine_averag_weeks_waiting_for_treatment_adult, na.rm = TRUE), 
              routine_child = mean(routine_averag_weeks_waiting_for_treatment_child, na.rm = TRUE),
              GA_adult = mean(GA_averag_weeks_waiting_for_treatment_adult, na.rm = TRUE),
              GA_child = mean(GA_averag_weeks_waiting_for_treatment_child, na.rm = TRUE),
              SS_adult = mean(SS_averag_weeks_waiting_for_treatment_adult, na.rm = TRUE),
              SS_child = mean(SS_averag_weeks_waiting_for_treatment_child, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = c(routine_adult, 
                          routine_child,
                          GA_adult,
                          GA_child,
                          SS_adult,
                          SS_child),
                 names_to = "variable",
                 values_to = "weeks_waiting") %>%
    mutate(patient_group = sub(".*\\_", "", variable),
           appointment_type = sub("\\_.*", "", variable))
  
  
  
  ggplot(data) +
    geom_line(aes(x = quarter,
                  y = weeks_waiting,
                  colour = appointment_type)) +
    geom_point(aes(x = quarter,
                   y = weeks_waiting,
                   colour = appointment_type)) +
    ggrepel::geom_text_repel(aes(x = quarter,
                                 y = weeks_waiting,
                                 colour = appointment_type,
                                 label = round(weeks_waiting)),
                             size=3, box.padding = unit(0.2, "lines")
    ) +
    theme_bw() +
    labs(title = "Quarterly mean number of weeks waiting for first treatment",
         subtitle = subtitle,
         x = "Quarter start date",
         y = "Mean number of paitings waiting",
         colour = "") +
    facet_wrap(vars(patient_group), 
               #scales = "free_y", 
               nrow = 1) +
    theme(axis.text.x = element_text(angle = 90))
  
  
  
}

################################################################################
plot_practice_closures <- function(data = contract_handbacks,
                                   level = "National",
                                   region_STP_name = NULL,
                                   all_regions_and_STPs = FALSE,
                                   plotChart = TRUE){
  
  data <- data %>%
    mutate(region_name = case_when(region_name == "East" ~ "East of England",
                                   region_name == "NE & Yorkshire" ~ "North East and Yorkshire",
                                   TRUE ~ region_name)) %>%
    select(time_period, region_name, commissioner_termination_notices, contractor_termination_notices,
           retirement_of_contractor, death_of_contractor, other) %>%
    pivot_longer(cols = c("commissioner_termination_notices", "contractor_termination_notices",
                          "retirement_of_contractor", "death_of_contractor", "other"),
                 names_to = "reason",
                 values_to = "closures"
                 )
  
  #set defualt subtitle
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  totals <- data %>%
    group_by(time_period) %>%
    summarise(closures = sum(closures, na.rm = TRUE))

  #group data
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(time_period, region_name, reason)
  }else{
    data <- data %>% 
      group_by(time_period, reason)
  }
  
  data <- data %>%
    summarise(closures = sum(closures, na.rm = TRUE))
  
  data$time_period <- factor(data$time_period,
                             levels = c("Apr - Jun 2021",
                                        "Jul - Sep 2021",
                                        "October 2021",
                                        "November 2021",
                                        "December 2021",
                                        "January 2022",
                                        "February 2022",
                                        "March 2022",
                                        "April 2022",
                                        "May 2022"))
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#CC79A7")
  
  if(plotChart == TRUE){
    ggplot() +
      geom_bar(data = data,
               aes(x = time_period,
                   y = closures,
                   fill = reason),
               position = "stack",
               stat = "identity") +
      geom_text(data = totals,
                aes(x = time_period,
                    y = closures + (closures/20) + 0.3,
                    label = closures),
                size = 3) +
      scale_fill_manual(values = cbPalette, labels = c("Commissioner termination notices", "Contractor termination notices",
                                                       "Death of contractor", "Other reason", "Retirement of contractor")) +
      theme_bw() +
      labs(title = "Practice Closures",
           subtitle = subtitle,
           x = "Time period",
           y = "Number of closures",
           fill = "Closure reason") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }else{
    data <- data %>%
      rename(`Time period` = time_period,
             `Region Name` = region_name,
             `Closure reason` = reason,
             `Number of closures` = closures)
  }
  
}

################################################################################
plot_re_and_de_commissioned_UDAs_UOAs <- function(data = contract_handbacks,
                                                  level = "National",
                                                  region_STP_name = NULL,
                                                  all_regions_and_STPs = FALSE,
                                                  plotChart = TRUE){
  
  data <- data %>%
    mutate(region_name = case_when(region_name == "East" ~ "East of England",
                                   region_name == "NE & Yorkshire" ~ "North East and Yorkshire",
                                   TRUE ~ region_name)) %>%
    mutate(decommissioned_UDAs = -decommissioned_UDAs,
           decommissioned_UOAs = -decommissioned_UOAs) %>%
    select(time_period, region_name, decommissioned_UDAs, recommissioned_UDAs, decommissioned_UOAs, recommissioned_UOAs) %>%
    pivot_longer(cols = c("decommissioned_UDAs", "recommissioned_UDAs", "decommissioned_UOAs", "recommissioned_UOAs"),
                 names_to = "commission_type",
                 values_to = "value")
  
  #set defualt subtitle
  subtitle <- "England"
  
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    
    subtitle <- region_STP_name
  }
  
  totals <- data %>%
    group_by(time_period) %>%
    summarise(value = sum(value, na.rm = TRUE))
  
  #group data
  if(all_regions_and_STPs == TRUE){
    data <- data %>%
      group_by(time_period, region_name, commission_type)
  }else{
    data <- data %>% 
      group_by(time_period, commission_type)
  }
  
  data <- data %>%
    summarise(value = sum(value, na.rm = TRUE))
  
  data$time_period <- factor(data$time_period,
                             levels = c("Apr - Jun 2021",
                                        "Jul - Sep 2021",
                                        "October 2021",
                                        "November 2021",
                                        "December 2021",
                                        "January 2022",
                                        "February 2022",
                                        "March 2022",
                                        "April 2022",
                                        "May 2022"))

  
  if(plotChart == TRUE){
    ggplot() +
      geom_bar(data = data,
               aes(x = time_period,
                   y = value,
                   fill = commission_type),
               position = "dodge",
               stat = "identity") +
      geom_hline(yintercept = 0,
                 colour = "black") +
      # geom_text(data = data,
      #           aes(x = time_period,
      #               y = value + (value/10),
      #               label = value ,
      #               group = commission_type),
      #           position = position_dodge(width = 1),
      #           size = 3) +
      scale_fill_manual(values = c("firebrick3", "coral", "springgreen4", "palegreen1"), 
                        labels = c("De-commissioned UDAs", "De-commissioned UOAs",
                                   "Re-commissioned UDAs", "Re-commissioned UOAs")) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      theme_bw() +
      labs(title = "De-commissioned and re-commissoned UDAs and UOAs \nas a result of practice closures",
           subtitle = subtitle,
           x = "Time period",
           y = "Number of de-commissioned or \nre-commissioned UDAs and UOAs",
           fill = "") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }else{
    data <- data %>%
      rename(`Time period` = time_period,
             `Region name` = region_name,
             `Re- or de- commission` = commission_type,
             `UDAs or UOAs re- or de- commissioned` = value)
  }
  
}

################################################################################
plot_closures_facet <- function(data = contract_handbacks,
                                level = "National",
                                region_STP_name = NULL,
                                all_regions_and_STPs = FALSE,
                                plotChart = TRUE){
  
  p1 <- plot_practice_closures(data = data,
                               level = level,
                               region_STP_name = region_STP_name,
                               all_regions_and_STPs = all_regions_and_STPs,
                               plotChart = plotChart)
  
  p2 <- plot_re_and_de_commissioned_UDAs_UOAs(data = data,
                                              level = level,
                                              region_STP_name = region_STP_name,
                                              all_regions_and_STPs = all_regions_and_STPs,
                                              plotChart = plotChart)
  
  
  ggpubr::ggarrange(p1, p2, nrow = 2)
}
