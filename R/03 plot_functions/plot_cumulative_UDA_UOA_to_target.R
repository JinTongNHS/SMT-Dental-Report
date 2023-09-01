################################################################################
plot_cumulative_UDA_UOA_to_target <- function(data = UDA_scheduled_data, 
                                              UDAorUOA = "UDA", 
                                              level = "National",
                                              region_STP_name = NULL,
                                              plotChart = TRUE,
                                              all_regions_and_STPs = FALSE,
                                              remove_prototypes = TRUE){

  
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
    captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022."
    barCol <- "#CC79A7"
    
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
    captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022."
    barCol <- "steelblue"
    
    #get raw data into fight format
    data <- get_data_for_cumulative_plot_UOA(data = data, 
                                             calendar_data = calendar_data, 
                                             remove_prototypes = remove_prototypes, 
                                             all_regions_and_STPs = all_regions_and_STPs)
    
  }
  
  #add blanks for future dates if on single level
  if(all_regions_and_STPs == FALSE & nrow(data) < 28){

    
    # if(!(as.Date("2023-05-01") %in% data$month)){
    #   data <- data %>% add_row(month = as.Date("2023-05-01"))
    # }
    
    if(!(as.Date("2023-06-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-06-01"))
    }
    if(!(as.Date("2023-07-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2023-07-01"))
    }
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_threshold_delivered = monthly_UDA_UOAs_delivered * 100 / threshold_UDA_UOAs_contracted_in_threshold_period) %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 100 * 4/ total_annual_UDA_UOAs_contracted) %>%
    mutate(financial_quarter = case_when(month < as.Date("2021-07-01") ~ "Apr-Jun (Q1 21/22)",
                                         month < as.Date("2021-10-01") ~ "Jul-Sep (Q2 21/22)",
                                         month < as.Date("2022-01-01") ~ "Oct-Dec (Q3 21/22)",
                                         month < as.Date("2022-04-01") ~ "Jan-Mar (Q4 21/22)",
                                         month < as.Date("2022-07-01") ~ "Apr-Jun (Q1 22/23)",
                                         month < as.Date("2022-10-01") ~ "Jul-Sep (Q2 22/23)",
                                         month < as.Date("2023-01-01") ~ "Oct-Dec (Q3 22/23)",
                                         month < as.Date("2023-04-01") ~ "Jan-Mar (Q4 22/23)",
                                         month < as.Date("2023-07-01") ~ "Apr-Jun (Q1 23/24)",
                                         month < as.Date("2023-10-01") ~ "Jul-Sep (Q2 23/24)"
                                         ))
  
  data$financial_quarter <- factor(data$financial_quarter,
                                   levels = c("Apr-Jun (Q1 21/22)",
                                              "Jul-Sep (Q2 21/22)",
                                              "Oct-Dec (Q3 21/22)",
                                              "Jan-Mar (Q4 21/22)",
                                              "Apr-Jun (Q1 22/23)",
                                              "Jul-Sep (Q2 22/23)",
                                              "Oct-Dec (Q3 22/23)",
                                              "Jan-Mar (Q4 22/23)",
                                              "Apr-Jun (Q1 23/24)",
                                              "Jul-Sep (Q2 23/24)"
                                              ))
  
  
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
        rep(seq(from = 100/3, to = 100, length.out = 3),9),
        seq(from = 100/3, to = 100, length.out = 1)
          #seq(from = septemberTarget/3, to = septemberTarget, length.out = 4)),
        # seq(from = decemberTarget/3, to = decemberTarget, length.out = 3),
        # seq(from = marchTarget/3, to = marchTarget, length.out = 3),
        # seq(from = juneTarget/3, to = juneTarget, length.out = 3),
        # # 
        # seq(from = 100/3, to = 100, length.out = 3),
        # seq(from = 100/3, to = 100, length.out = 3),
        # seq(from = 100/3, to = 100, length.out = 3),
        # seq(from = 100/3, to = 100, length.out = 3),
        # seq(from = 100/3, to = 100, length.out = 3)
      )
      ) %>%
      filter(month >= as.Date("2022-07-01"))
    
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
      geom_vline(xintercept = as.Date("2022-09-01") + lubridate::days(15),
                 linetype = "dotted") +
      geom_vline(xintercept = as.Date("2022-12-01") + lubridate::days(15),
                 linetype = "dotted") +
      geom_vline(xintercept = as.Date("2023-03-01") + lubridate::days(15),
                 linetype = "dotted") +
      geom_vline(xintercept = as.Date("2023-06-01") + lubridate::days(15),
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
      scale_colour_manual(labels = c(#paste0("Expected cumulative delivery to reach \nQ1 threshold of ",septemberTarget,"% by end of Jun-21"),
                                     #paste0("Expected cumulative delivery to reach \nQ2 threshold of ",septemberTarget,"% by end of Sep-21"),
                                     #paste0("Expected cumulative delivery to reach \nQ3 threshold of ",decemberTarget,"% by end of Dec-21"),
                                     #paste0("Expected cumulative delivery to reach \nQ4 threshold of ",marchTarget,"% by end of Mar-22"),
                                     paste0("Expected cumulative delivery to reach \nQ1 threshold of ",juneTarget,"% by end of Jun-22"),
                                     paste0("Expected cumulative delivery to reach \nQ2 threshold of ",september22Target,"% by end of Sep-22"),
                                     paste0("Expected cumulative delivery to reach \nQ3 threshold of ",100,"% by end of Dec-22"),
                                     paste0("Expected cumulative delivery to reach \nQ4 threshold of ",100,"% by end of Mar-23"),
                                     paste0("Expected cumulative delivery to reach \nQ1 threshold of ",100,"% by end of Jun-24")),
                          values = c("darkred", "blue", "darkgreen", "darkgoldenrod3", "darkorange3", "deeppink2", "green")) +
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